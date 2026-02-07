module Exp.Frontend.Semantic.ExpSemant
  ( checkProgram
  ) where

import Control.Monad (foldM, when, zipWithM_, forM_)
import Control.Monad.Except
import Control.Monad.State
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map

import Exp.Frontend.Syntax.ExpSyntax

-- Internal type representation for inference/checking.
data Ty
  = TyInt
  | TyFloat
  | TyString
  | TyBool
  | TyVoid
  | TyStruct String
  | TyArray Ty
  | TyFunc [Ty] Ty
  | TyVar TV
  deriving (Eq, Show)

newtype TV = TV Int
  deriving (Eq, Ord, Show)

type Subst = Map TV Ty

data InferState = InferState
  { nextVar :: Int
  , subst :: Subst
  }

type Infer a = ExceptT String (State InferState) a

data FuncSig = FuncSig
  { sigTypeVars :: Map String TV
  , sigParams :: [Ty]
  , sigRet :: Ty
  }

data Env = Env
  { scopes :: [Map String Ty]
  , funcs  :: Map String FuncSig
  , structs :: Map String StructDef
  , retTy  :: Ty
  , typeVars :: Map String TV
  }

checkProgram :: Program -> Either String ()
checkProgram (Program sdefs fdefs) =
  evalState (runExceptT go) (InferState 0 Map.empty)
  where
    go = do
      structMap <- buildStructs sdefs
      funcMap <- buildFuncSigs structMap fdefs
      mapM_ (checkFunc structMap funcMap) fdefs

-- Structs

buildStructs :: [StructDef] -> Infer (Map String StructDef)
buildStructs defs = do
  let names = map structName defs
  ensureUnique "struct" names
  mapM_ checkStructFields defs
  let smap = Map.fromList [(structName d, d) | d <- defs]
  mapM_ (checkStructTypes smap) defs
  pure smap

checkStructFields :: StructDef -> Infer ()
checkStructFields (StructDef _ fields) =
  ensureUnique "field" (map fst fields)

checkStructTypes :: Map String StructDef -> StructDef -> Infer ()
checkStructTypes smap (StructDef _ fields) =
  mapM_ (\(_, t) -> validateType smap Map.empty t) fields

-- Functions

buildFuncSigs :: Map String StructDef -> [FuncDef] -> Infer (Map String FuncSig)
buildFuncSigs structMap defs = do
  let names = map funcName defs
  ensureUnique "function" names
  fmap Map.fromList $ mapM (mkSig structMap) defs

mkSig :: Map String StructDef -> FuncDef -> Infer (String, FuncSig)
mkSig structMap f = do
  tvs <- makeTypeVars (funcTypeVars f)
  let mkParam (n, mty) = do
        ty <- case mty of
          Nothing -> freshTyVar
          Just t  -> do
            validateType structMap tvs t
            typeFromAst tvs t
        pure (n, ty)
  params <- mapM mkParam (funcParams f)
  ensureUnique "parameter" (map fst params)
  ret <- case funcReturnType f of
    Nothing -> freshTyVar
    Just t  -> do
      validateType structMap tvs t
      typeFromAst tvs t
  let sig = FuncSig tvs (map snd params) ret
  pure (funcName f, sig)

checkFunc :: Map String StructDef -> Map String FuncSig -> FuncDef -> Infer ()
checkFunc structMap funcMap f = do
  case Map.lookup (funcName f) funcMap of
    Nothing -> throwError $ "Missing function signature for: " ++ funcName f
    Just sig -> do
      let paramNames = map fst (funcParams f)
      ensureUnique "parameter" paramNames
      let paramsWithTypes = zip paramNames (sigParams sig)
      let env = Env
            { scopes = [Map.fromList paramsWithTypes]
            , funcs = funcMap
            , structs = structMap
            , retTy = sigRet sig
            , typeVars = sigTypeVars sig
            }
      _ <- checkStmt env (funcBody f)
      pure ()

-- Statements

checkStmt :: Env -> Stmt -> Infer Env
checkStmt env stmt = case stmt of
  SBlock ss -> do
    let env' = env { scopes = Map.empty : scopes env }
    env'' <- foldM checkStmt env' ss
    pure env'' { scopes = tail (scopes env'') }

  SDecl name mty mexp -> do
    ensureNotDeclared env name
    ty <- case (mty, mexp) of
      (Nothing, Nothing) -> throwError $ "Declaration without type or initializer: " ++ name
      (Just t, Nothing)  -> do
        validateType (structs env) (typeVars env) t
        typeFromAst (typeVars env) t
      (Nothing, Just e)  -> inferExp env e
      (Just t, Just e)   -> do
        validateType (structs env) (typeVars env) t
        t1 <- typeFromAst (typeVars env) t
        t2 <- inferExp env e
        unify t1 t2
        pure t1
    let scopes' = case scopes env of
          [] -> [Map.singleton name ty]
          (s:ss) -> Map.insert name ty s : ss
    pure env { scopes = scopes' }

  SIf cond tbranch melse -> do
    tcond <- inferExp env cond
    unify tcond TyBool
    _ <- checkStmt env tbranch
    case melse of
      Nothing -> pure env
      Just s  -> checkStmt env s

  SWhile cond body -> do
    tcond <- inferExp env cond
    unify tcond TyBool
    _ <- checkStmt env body
    pure env

  SFor initS cond step body -> do
    let env' = env { scopes = Map.empty : scopes env }
    env1 <- checkStmt env' initS
    tcond <- inferExp env1 cond
    unify tcond TyBool
    _ <- checkStmt env1 step
    _ <- checkStmt env1 body
    pure env

  SReturn mexp -> do
    case mexp of
      Nothing -> unify (retTy env) TyVoid
      Just e  -> do
        t <- inferExp env e
        unify t (retTy env)
    pure env

  SPrint e -> do
    _ <- inferExp env e
    pure env

  SExpr e -> do
    _ <- inferExp env e
    pure env

-- Expressions

inferExp :: Env -> Exp -> Infer Ty
inferExp env expr = case expr of
  EInt _ -> pure TyInt
  EFloat _ -> pure TyFloat
  EString _ -> pure TyString
  EVar v -> do
    mvar <- lookupVarMaybe env v
    case mvar of
      Just t -> pure t
      Nothing -> case Map.lookup v (funcs env) of
        Nothing -> throwError $ "Undefined variable: " ++ v
        Just s  -> do
          (params, ret) <- instantiateSig s
          pure (TyFunc params ret)
  ECall name args -> do
    mvar <- lookupVarMaybe env name
    (params, ret) <- case mvar of
      Just t -> do
        t' <- resolve t
        case t' of
          TyFunc ps r -> pure (ps, r)
          _ -> throwError $ "Trying to call non-function: " ++ name
      Nothing -> do
        sig <- case Map.lookup name (funcs env) of
          Nothing -> throwError $ "Undefined function: " ++ name
          Just s  -> pure s
        instantiateSig sig
    when (length params /= length args) $
      throwError $ "Wrong number of arguments in call to " ++ name
    argTys <- mapM (inferExp env) args
    zipWithM_ unify params argTys
    pure ret
  EIndex arr idx -> do
    tArr <- inferExp env arr
    tIdx <- inferExp env idx
    unify tIdx TyInt
    tArr' <- resolve tArr
    case tArr' of
      TyArray tElem -> pure tElem
      _ -> throwError "Indexing non-array value"
  EField e field -> do
    t <- inferExp env e
    t' <- resolve t
    case t' of
      TyArray _ | field == "size" -> pure TyInt
      TyArray _ -> throwError $ "Invalid array field: " ++ field
      TyStruct sname -> do
        sdef <- lookupStruct env sname
        case lookup field (structFields sdef) of
          Nothing -> throwError $ "Unknown field " ++ field ++ " in struct " ++ sname
          Just ft -> typeFromAst (typeVars env) ft
      _ -> throwError "Field access on non-struct value"
  EStruct sname fields -> do
    sdef <- lookupStruct env sname
    let fdefs = structFields sdef
    let positional = all (\(n, _) -> null n) fields
    if positional
      then do
        when (length fields /= length fdefs) $
          throwError $ "Wrong number of fields in struct " ++ sname
        zipWithM_ (\(_, e) (_, t) -> do te <- inferExp env e
                                        t' <- typeFromAst (typeVars env) t
                                        unify te t') fields fdefs
        pure (TyStruct sname)
      else do
        -- Named fields (shouldn't happen with current parser, but keep safe).
        let names = map fst fields
        ensureUnique "struct field" names
        forM_ fields $ \(n, e) -> case lookup n fdefs of
          Nothing -> throwError $ "Unknown field " ++ n ++ " in struct " ++ sname
          Just t  -> do
            te <- inferExp env e
            t' <- typeFromAst (typeVars env) t
            unify te t'
        pure (TyStruct sname)
  ENew t size -> do
    tsize <- inferExp env size
    unify tsize TyInt
    t' <- typeFromAst (typeVars env) t
    pure (TyArray t')
  EAssign lhs rhs -> do
    tLhs <- inferLValue env lhs
    tRhs <- inferExp env rhs
    unify tLhs tRhs
    pure tRhs
  EBinary op l r -> inferBinOp env op l r
  EUnary op e -> inferUnOp env op e
  EArrayLit es -> do
    case es of
      [] -> TyArray <$> freshTyVar
      (x:xs) -> do
        t <- inferExp env x
        mapM_ (\e -> inferExp env e >>= unify t) xs
        pure (TyArray t)

inferLValue :: Env -> Exp -> Infer Ty
inferLValue env e = case e of
  EVar v -> lookupVar env v
  EIndex arr idx -> do
    tArr <- inferExp env arr
    tIdx <- inferExp env idx
    unify tIdx TyInt
    tArr' <- resolve tArr
    case tArr' of
      TyArray tElem -> pure tElem
      _ -> throwError "Indexing non-array value"
  EField ex field -> do
    when (field == "size") $
      throwError "Cannot assign to array size"
    t <- inferExp env ex
    t' <- resolve t
    case t' of
      TyStruct sname -> do
        sdef <- lookupStruct env sname
        case lookup field (structFields sdef) of
          Nothing -> throwError $ "Unknown field " ++ field ++ " in struct " ++ sname
          Just ft -> typeFromAst (typeVars env) ft
      _ -> throwError "Field assignment on non-struct value"
  _ -> throwError "Invalid assignment target"

inferBinOp :: Env -> BinOp -> Exp -> Exp -> Infer Ty
inferBinOp env op l r = case op of
  Or  -> boolBin
  And -> boolBin
  Eq  -> eqBin
  Neq -> eqBin
  Gt  -> relBin
  Lt  -> relBin
  Ge  -> relBin
  Le  -> relBin
  Add -> numBin
  Sub -> numBin
  Mul -> numBin
  Div -> numBin
  where
    boolBin = do
      tl <- inferExp env l
      tr <- inferExp env r
      unify tl TyBool
      unify tr TyBool
      pure TyBool
    numBin = do
      tl <- inferExp env l
      tr <- inferExp env r
      unify tl tr
      t <- resolve tl
      case t of
        TyInt -> pure TyInt
        TyFloat -> pure TyFloat
        _ -> throwError "Expected numeric operands"
    relBin = do
      tl <- inferExp env l
      tr <- inferExp env r
      unify tl tr
      t <- resolve tl
      case t of
        TyInt -> pure TyBool
        TyFloat -> pure TyBool
        _ -> throwError "Expected numeric operands"
    eqBin = do
      tl <- inferExp env l
      tr <- inferExp env r
      unify tl tr
      pure TyBool

inferUnOp :: Env -> UnOp -> Exp -> Infer Ty
inferUnOp env op e = case op of
  Not -> do
    t <- inferExp env e
    unify t TyBool
    pure TyBool
  Neg -> do
    t <- inferExp env e
    t' <- resolve t
    case t' of
      TyInt -> pure TyInt
      TyFloat -> pure TyFloat
      _ -> throwError "Expected numeric operand"

-- Env helpers

lookupVar :: Env -> String -> Infer Ty
lookupVar env name = do
  m <- lookupVarMaybe env name
  case m of
    Nothing -> throwError $ "Undefined variable: " ++ name
    Just t  -> pure t

lookupVarMaybe :: Env -> String -> Infer (Maybe Ty)
lookupVarMaybe env name = do
  let search [] = Nothing
      search (s:ss) = case Map.lookup name s of
        Just t  -> Just t
        Nothing -> search ss
  pure (search (scopes env))

lookupStruct :: Env -> String -> Infer StructDef
lookupStruct env name =
  case Map.lookup name (structs env) of
    Nothing -> throwError $ "Undefined struct: " ++ name
    Just s  -> pure s

ensureUnique :: String -> [String] -> Infer ()
ensureUnique label xs = do
  let dups = xs \\ nub xs
  when (not (null dups)) $
    throwError $ "Duplicate " ++ label ++ " name(s): " ++ show dups
  where
    (\\) = foldl (flip removeFirst)
    removeFirst y = go
      where
        go [] = []
        go (x:rest)
          | x == y = rest
          | otherwise = x : go rest

ensureNotDeclared :: Env -> String -> Infer ()
ensureNotDeclared env name = case scopes env of
  [] -> pure ()
  (s:_) ->
    when (Map.member name s) $
      throwError $ "Variable already declared in this scope: " ++ name

-- Type conversion

makeTypeVars :: [String] -> Infer (Map String TV)
makeTypeVars names = do
  ensureUnique "type variable" names
  pairs <- mapM (\n -> (n,) <$> freshTV) names
  pure $ Map.fromList pairs

typeFromAst :: Map String TV -> Type -> Infer Ty
typeFromAst tvs t = case t of
  TypeInt -> pure TyInt
  TypeFloat -> pure TyFloat
  TypeString -> pure TyString
  TypeBool -> pure TyBool
  TypeVoid -> pure TyVoid
  TypeStruct s -> case Map.lookup s tvs of
    Just tv -> pure (TyVar tv)
    Nothing -> pure (TyStruct s)
  TArray ty -> TyArray <$> typeFromAst tvs ty
  TFuncType args ret -> TyFunc <$> mapM (typeFromAst tvs) args <*> typeFromAst tvs ret
  TTypeVar v -> case Map.lookup v tvs of
    Nothing -> throwError $ "Unknown type variable: " ++ v
    Just tv -> pure (TyVar tv)

validateType :: Map String StructDef -> Map String TV -> Type -> Infer ()
validateType smap tvs t = case t of
  TypeStruct s ->
    when (Map.notMember s smap && Map.notMember s tvs) $
      throwError $ "Unknown struct type: " ++ s
  TTypeVar v ->
    when (Map.notMember v tvs) $
      throwError $ "Unknown type variable: " ++ v
  TArray ty -> validateType smap tvs ty
  TFuncType args ret -> mapM_ (validateType smap tvs) args >> validateType smap tvs ret
  _ -> pure ()

-- Unification

freshTV :: Infer TV
freshTV = do
  st <- get
  let n = nextVar st
  put st { nextVar = n + 1 }
  pure (TV n)

freshTyVar :: Infer Ty
freshTyVar = TyVar <$> freshTV

resolve :: Ty -> Infer Ty
resolve t = do
  s <- gets subst
  pure (applySubst s t)

applySubst :: Subst -> Ty -> Ty
applySubst s t = case t of
  TyArray a -> TyArray (applySubst s a)
  TyFunc as r -> TyFunc (map (applySubst s) as) (applySubst s r)
  TyVar v -> case Map.lookup v s of
    Nothing -> TyVar v
    Just t' -> applySubst s t'
  _ -> t

instantiateSig :: FuncSig -> Infer ([Ty], Ty)
instantiateSig sig = do
  s <- gets subst
  let params0 = map (applySubst s) (sigParams sig)
  let ret0 = applySubst s (sigRet sig)
  let tvs = map snd (Map.toList (sigTypeVars sig))
  fresh <- mapM (const freshTyVar) tvs
  let inst = Map.fromList (zip tvs fresh)
  let params = map (applySubst inst) params0
  let ret = applySubst inst ret0
  pure (params, ret)

unify :: Ty -> Ty -> Infer ()
unify t1 t2 = do
  s <- gets subst
  let a = applySubst s t1
  let b = applySubst s t2
  case (a, b) of
    (TyInt, TyInt) -> pure ()
    (TyFloat, TyFloat) -> pure ()
    (TyString, TyString) -> pure ()
    (TyBool, TyBool) -> pure ()
    (TyVoid, TyVoid) -> pure ()
    (TyStruct x, TyStruct y) | x == y -> pure ()
    (TyArray x, TyArray y) -> unify x y
    (TyFunc as r, TyFunc bs r2) -> do
      when (length as /= length bs) $
        throwError "Function arity mismatch"
      zipWithM_ unify as bs
      unify r r2
    (TyVar v, t) -> bindVar v t
    (t, TyVar v) -> bindVar v t
    _ -> throwError $ "Type mismatch: " ++ show a ++ " vs " ++ show b

bindVar :: TV -> Ty -> Infer ()
bindVar v t
  | t == TyVar v = pure ()
  | occurs v t = throwError "Occurs check failed"
  | otherwise = modify (\st -> st { subst = Map.insert v t (subst st) })

occurs :: TV -> Ty -> Bool
occurs v t = case t of
  TyVar v' -> v == v'
  TyArray a -> occurs v a
  TyFunc as r -> any (occurs v) as || occurs v r
  _ -> False
