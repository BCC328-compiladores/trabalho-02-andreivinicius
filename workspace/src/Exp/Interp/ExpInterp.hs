module Exp.Interp.ExpInterp
  ( eval
  , interpProgram
  ) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad (when)
import Data.Map (Map)
import qualified Data.Map as Map

import Exp.Frontend.Syntax.ExpSyntax

-- Minimal evaluator used by the expression REPL (arithmetic only).
eval :: Exp -> Int
eval (EInt n) = fromIntegral n
eval (EBinary Add l r) = eval l + eval r
eval (EBinary Sub l r) = eval l - eval r
eval (EBinary Mul l r) = eval l * eval r
eval (EBinary Div l r) = eval l `div` eval r
eval _ = error "Unsupported expression in eval"

-- Interpreter for full SL programs.

data Value
  = VInt Integer
  | VFloat Double
  | VString String
  | VBool Bool
  | VArray [Value]
  | VStruct (Map String Value)
  | VFunc String
  | VUndef
  deriving (Eq, Show)

data ExecResult = Normal | Ret Value

data Runtime = Runtime
  { scopes :: [Map String Value]
  , funcs  :: Map String FuncDef
  , structs :: Map String StructDef
  }

type Interp a = ExceptT String (StateT Runtime IO) a

interpProgram :: Program -> IO ()
interpProgram (Program sdefs fdefs) = do
  let fenv = Map.fromList [(funcName f, f) | f <- fdefs]
      senv = Map.fromList [(structName s, s) | s <- sdefs]
      rt = Runtime [Map.empty] fenv senv
  (r, _) <- runStateT (runExceptT (callFunction "main" [])) rt
  case r of
    Left err -> putStrLn err
    Right _ -> pure ()

-- Function calls

callFunction :: String -> [Value] -> Interp Value
callFunction name args = do
  fenv <- gets funcs
  case Map.lookup name fenv of
    Nothing -> throwError $ "Undefined function: " ++ name
    Just f -> do
      let params = funcParams f
      when (length params /= length args) $
        throwError $ "Wrong number of arguments in call to " ++ name
      let pnames = map fst params
      let pscope = Map.fromList (zip pnames args)
      oldScopes <- gets scopes
      modify (\st -> st { scopes = pscope : oldScopes })
      r <- interpStmt (funcBody f)
      modify (\st -> st { scopes = oldScopes })
      case r of
        Ret v -> pure v
        Normal -> pure VUndef

-- Statements

interpStmt :: Stmt -> Interp ExecResult
interpStmt stmt = case stmt of
  SBlock ss -> do
    pushScope
    r <- interpStmts ss
    popScope
    pure r
  SDecl name mty mexp -> do
    val <- case mexp of
      Nothing -> case mty of
        Nothing -> pure VUndef
        Just ty -> defaultValue ty
      Just e  -> evalExp e
    declareVar name val
    pure Normal
  SIf cond tbranch melse -> do
    v <- evalExp cond
    case v of
      VBool True -> interpStmt tbranch
      VBool False -> case melse of
        Nothing -> pure Normal
        Just s  -> interpStmt s
      _ -> throwError "if condition is not boolean"
  SWhile cond body -> do
    v <- evalExp cond
    case v of
      VBool False -> pure Normal
      VBool True -> do
        r <- interpStmt body
        case r of
          Ret _ -> pure r
          Normal -> interpStmt (SWhile cond body)
      _ -> throwError "while condition is not boolean"
  SFor initS cond step body -> do
    pushScope
    _ <- interpStmt initS
    r <- loop
    popScope
    pure r
    where
      loop = do
        v <- evalExp cond
        case v of
          VBool False -> pure Normal
          VBool True -> do
            r <- interpStmt body
            case r of
              Ret _ -> pure r
              Normal -> do
                _ <- interpStmt step
                loop
          _ -> throwError "for condition is not boolean"
  SReturn mexp -> do
    v <- case mexp of
      Nothing -> pure VUndef
      Just e  -> evalExp e
    pure (Ret v)
  SPrint e -> do
    v <- evalExp e
    liftIO $ print v
    pure Normal
  SExpr e -> do
    _ <- evalExp e
    pure Normal

interpStmts :: [Stmt] -> Interp ExecResult
interpStmts [] = pure Normal
interpStmts (s:ss) = do
  r <- interpStmt s
  case r of
    Normal -> interpStmts ss
    Ret _ -> pure r

-- Expressions

evalExp :: Exp -> Interp Value
evalExp expr = case expr of
  EInt n -> pure (VInt n)
  EFloat f -> pure (VFloat f)
  EString s -> pure (VString s)
  EVar v -> do
    mvar <- lookupVarMaybe v
    case mvar of
      Just val -> pure val
      Nothing -> do
        fenv <- gets funcs
        case Map.lookup v fenv of
          Nothing -> throwError $ "Undefined variable: " ++ v
          Just _ -> pure (VFunc v)
  ECall name args -> do
    mvar <- lookupVarMaybe name
    case mvar of
      Just (VFunc fname) -> mapM evalExp args >>= callFunction fname
      Just _ -> throwError $ "Trying to call non-function: " ++ name
      Nothing -> mapM evalExp args >>= callFunction name
  EIndex arr idx -> do
    vArr <- evalExp arr
    vIdx <- evalExp idx
    i <- expectInt vIdx
    case vArr of
      VArray xs -> indexArray xs i
      _ -> throwError "Indexing non-array value"
  EField e field -> do
    v <- evalExp e
    case v of
      VArray xs | field == "size" -> pure (VInt (fromIntegral (length xs)))
      VStruct m -> case Map.lookup field m of
        Nothing -> throwError $ "Unknown field: " ++ field
        Just val -> pure val
      _ -> throwError "Field access on non-struct value"
  EStruct name fields -> do
    sdef <- lookupStruct name
    let sfields = structFields sdef
    let positional = all (\(n, _) -> null n) fields
    if positional
      then do
        when (length fields /= length sfields) $
          throwError $ "Wrong number of fields in struct " ++ name
        vals <- mapM (evalExp . snd) fields
        let m = Map.fromList (zip (map fst sfields) vals)
        pure (VStruct m)
      else do
        vals <- mapM (\(n, e) -> (n,) <$> evalExp e) fields
        pure (VStruct (Map.fromList vals))
  ENew ty size -> do
    vsize <- evalExp size
    n <- expectInt vsize
    def <- defaultValue ty
    pure (VArray (replicate (fromIntegral n) def))
  EAssign lhs rhs -> do
    v <- evalExp rhs
    setLValue lhs v
    pure v
  EBinary op l r -> evalBin op l r
  EUnary op e -> evalUn op e
  EArrayLit es -> VArray <$> mapM evalExp es

evalBin :: BinOp -> Exp -> Exp -> Interp Value
evalBin op l r = do
  vl <- evalExp l
  vr <- evalExp r
  case op of
    Or  -> boolBin (||) vl vr
    And -> boolBin (&&) vl vr
    Eq  -> pure (VBool (vl == vr))
    Neq -> pure (VBool (vl /= vr))
    Gt  -> numCmp (>) (>) vl vr
    Lt  -> numCmp (<) (<) vl vr
    Ge  -> numCmp (>=) (>=) vl vr
    Le  -> numCmp (<=) (<=) vl vr
    Add -> numBin (+) (+) vl vr
    Sub -> numBin (-) (-) vl vr
    Mul -> numBin (*) (*) vl vr
    Div -> numBin div (/) vl vr
  where
    boolBin f (VBool a) (VBool b) = pure (VBool (f a b))
    boolBin _ _ _ = throwError "Expected boolean operands"
    numBin fInt fFloat (VInt a) (VInt b) = pure (VInt (fInt a b))
    numBin _ fFloat (VFloat a) (VFloat b) = pure (VFloat (fFloat a b))
    numBin _ _ _ _ = throwError "Expected numeric operands"
    numCmp fInt _ (VInt a) (VInt b) = pure (VBool (fInt a b))
    numCmp _ fFloat (VFloat a) (VFloat b) = pure (VBool (fFloat a b))
    numCmp _ _ _ _ = throwError "Expected numeric operands"

evalUn :: UnOp -> Exp -> Interp Value
evalUn op e = do
  v <- evalExp e
  case op of
    Not -> case v of
      VBool b -> pure (VBool (not b))
      _ -> throwError "Expected boolean operand"
    Neg -> case v of
      VInt n -> pure (VInt (-n))
      VFloat n -> pure (VFloat (-n))
      _ -> throwError "Expected numeric operand"

-- LValues

setLValue :: Exp -> Value -> Interp ()
setLValue target val = case target of
  EVar v -> updateVar v val
  EIndex arr idx -> do
    vArr <- evalExp arr
    vIdx <- evalExp idx
    i <- expectInt vIdx
    case vArr of
      VArray xs -> do
        xs' <- updateArray xs i val
        setLValue arr (VArray xs')
      _ -> throwError "Index assignment on non-array value"
  EField e field -> do
    when (field == "size") $
      throwError "Cannot assign to array size"
    v <- evalExp e
    case v of
      VStruct m -> do
        when (Map.notMember field m) $
          throwError $ "Unknown field: " ++ field
        let m' = Map.insert field val m
        setLValue e (VStruct m')
      _ -> throwError "Field assignment on non-struct value"
  _ -> throwError "Invalid assignment target"

-- Environment helpers

pushScope :: Interp ()
pushScope = modify (\st -> st { scopes = Map.empty : scopes st })

popScope :: Interp ()
popScope = modify (\st -> st { scopes = drop 1 (scopes st) })

declareVar :: String -> Value -> Interp ()
declareVar name val = do
  scs <- gets scopes
  case scs of
    [] -> modify (\st -> st { scopes = [Map.singleton name val] })
    (s:ss) ->
      if Map.member name s
        then throwError $ "Variable already declared: " ++ name
        else modify (\st -> st { scopes = Map.insert name val s : ss })

lookupVar :: String -> Interp Value
lookupVar name = do
  m <- lookupVarMaybe name
  case m of
    Nothing -> throwError $ "Undefined variable: " ++ name
    Just v  -> pure v
  where
    lookupInScopes [] = Nothing
    lookupInScopes (s:ss) = case Map.lookup name s of
      Just v  -> Just v
      Nothing -> lookupInScopes ss

lookupVarMaybe :: String -> Interp (Maybe Value)
lookupVarMaybe name = do
  scs <- gets scopes
  let lookupInScopes [] = Nothing
      lookupInScopes (s:ss) = case Map.lookup name s of
        Just v  -> Just v
        Nothing -> lookupInScopes ss
  pure (lookupInScopes scs)

updateVar :: String -> Value -> Interp ()
updateVar name val = do
  scs <- gets scopes
  case updateInScopes scs of
    Nothing -> throwError $ "Undefined variable: " ++ name
    Just scs' -> modify (\st -> st { scopes = scs' })
  where
    updateInScopes [] = Nothing
    updateInScopes (s:ss)
      | Map.member name s = Just (Map.insert name val s : ss)
      | otherwise = do
          rest <- updateInScopes ss
          pure (s : rest)

lookupStruct :: String -> Interp StructDef
lookupStruct name = do
  senv <- gets structs
  case Map.lookup name senv of
    Nothing -> throwError $ "Undefined struct: " ++ name
    Just s  -> pure s

-- Helpers

expectInt :: Value -> Interp Integer
expectInt (VInt n) = pure n
expectInt _ = throwError "Expected integer"

indexArray :: [Value] -> Integer -> Interp Value
indexArray xs i
  | i < 0 || i >= fromIntegral (length xs) = throwError "Array index out of bounds"
  | otherwise = pure (xs !! fromIntegral i)

updateArray :: [Value] -> Integer -> Value -> Interp [Value]
updateArray xs i v
  | i < 0 = throwError "Array index out of bounds"
  | otherwise = pure (prefix ++ [v] ++ drop (n + 1) xs)
  where
    n = fromIntegral i
    prefix =
      if n < length xs
        then take n xs
        else xs ++ replicate (n - length xs) VUndef

defaultValue :: Type -> Interp Value
defaultValue t = case t of
  TypeInt -> pure (VInt 0)
  TypeFloat -> pure (VFloat 0)
  TypeString -> pure (VString "")
  TypeBool -> pure (VBool False)
  TypeVoid -> pure VUndef
  TypeStruct sname -> do
    sdef <- lookupStruct sname
    vals <- mapM (defaultValue . snd) (structFields sdef)
    let m = Map.fromList (zip (map fst (structFields sdef)) vals)
    pure (VStruct m)
  TTypeVar _ -> pure VUndef
  TArray _ -> pure (VArray [])
  TFuncType _ _ -> pure VUndef
