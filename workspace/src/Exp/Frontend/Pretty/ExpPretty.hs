module Exp.Frontend.Pretty.ExpPretty where 

import Exp.Frontend.Syntax.ExpSyntax
--import Utils.Pretty 
import Data.List (intercalate)
import Text.Printf

indentation :: String
indentation = "    "

prettyPrint :: Program -> String
prettyPrint (Program structs funcs) =
    intercalate "\n\n" (map (prettyPrintStruct 0) structs) ++
    (if null structs || null funcs then "" else "\n\n") ++
    intercalate "\n\n" (map (prettyPrintFunc 0) funcs)

prettyPrintStruct :: Int -> StructDef -> String
prettyPrintStruct level (StructDef name fields) =
    indent level ++ "struct " ++ name ++ " {\n" ++
    intercalate "\n" (map (prettyPrintField (level + 1)) fields) ++ "\n" ++
    indent level ++ "}"

prettyPrintField :: Int -> (String, Type) -> String
prettyPrintField level (name, t) =
    indent level ++ name ++ " : " ++ prettyPrintType t ++ ";"

prettyPrintFunc :: Int -> FuncDef -> String
prettyPrintFunc level (FuncDef tvars name params retType body) =
    (if not (null tvars) then "forall " ++ unwords tvars ++ ". " else "") ++
    "func " ++ name ++
    "(" ++ intercalate ", " (map prettyPrintParam params) ++ ")" ++ 
    (case retType of  
        Nothing -> ""
        Just t -> " : " ++ prettyPrintType t) ++ " " ++  
    prettyPrintStmt level body

prettyPrintParam :: (String, Maybe Type) -> String  
prettyPrintParam (name, Nothing) = name  
prettyPrintParam (name, Just t) = name ++ " : " ++ prettyPrintType t  

prettyPrintType :: Type -> String
prettyPrintType TypeInt = "int"
prettyPrintType TypeFloat = "float"
prettyPrintType TypeString = "string"
prettyPrintType TypeBool = "bool"
prettyPrintType TypeVoid = "void"
prettyPrintType (TypeStruct n) = n
prettyPrintType (TTypeVar v) = v
prettyPrintType (TArray t) = prettyPrintType t ++ "[]"
prettyPrintType (TFuncType ts t) =
    "(" ++ intercalate ", " (map prettyPrintType ts) ++ ") -> " ++ prettyPrintType t

prettyPrintStmtNoSemi :: Int -> Stmt -> String
prettyPrintStmtNoSemi level stmt = 
    case stmt of
        SDecl name mType mExp ->
            indent level ++ "let " ++ name ++
            maybe "" (\t -> " : " ++ prettyPrintType t) mType ++
            maybe "" (\e -> " = " ++ prettyPrintExp e) mExp

        SExpr expr ->
            indent level ++ prettyPrintExp expr

        _ -> prettyPrintStmt level stmt 

prettyPrintStmt :: Int -> Stmt -> String
prettyPrintStmt level (SBlock stmts) =
    "{\n" ++
    intercalate "\n" (map (prettyPrintStmt (level + 1)) stmts) ++ "\n" ++
    indent level ++ "}"

prettyPrintStmt level (SDecl name mType mExp) =
    indent level ++ "let " ++ name ++
    maybe "" (\t -> " : " ++ prettyPrintType t) mType ++
    maybe "" (\e -> " = " ++ prettyPrintExp e) mExp ++ ";"

prettyPrintStmt level (SIf cond thenB mElseB) =
    indent level ++ "if (" ++ prettyPrintExp cond ++ ") " ++
    prettyPrintStmt level thenB ++
    maybe "" (\eB -> " else " ++ prettyPrintStmt level eB) mElseB

prettyPrintStmt level (SWhile cond body) =
    indent level ++ "while (" ++ prettyPrintExp cond ++ ") " ++
    prettyPrintStmt level body

prettyPrintStmt level (SFor initS cond stepS body) =
    indent level ++ "for (" ++
    prettyPrintStmtNoSemi (level + 1) initS ++ "; " ++
    prettyPrintExp cond ++ "; " ++
    prettyPrintStmtNoSemi (level + 1) stepS ++ ") " ++
    prettyPrintStmt level body

prettyPrintStmt level (SReturn mExp) =
    indent level ++ "return" ++
    maybe "" (\e -> " " ++ prettyPrintExp e) mExp ++ ";"

prettyPrintStmt level (SPrint expr) =
    indent level ++ "print(" ++ prettyPrintExp expr ++ ");"

prettyPrintStmt level (SExpr expr) =
    indent level ++ prettyPrintExp expr ++ ";"

prettyPrintExp :: Exp -> String
prettyPrintExp (EInt i) = show i
prettyPrintExp (EFloat f) = printf "%.2f" f
prettyPrintExp (EString s) = show s
prettyPrintExp (EVar v) = v
prettyPrintExp (ECall name args) =
    name ++ "(" ++ intercalate ", " (map prettyPrintExp args) ++ ")"
prettyPrintExp (EIndex arr idx) =
    prettyPrintExp arr ++ "[" ++ prettyPrintExp idx ++ "]"
prettyPrintExp (EField expr f) =
    prettyPrintExp expr ++ "." ++ f
prettyPrintExp (EStruct name fields) =
    name ++ "{" ++
    intercalate ", " (map prettyPrintStructField fields) ++
    "}"
    where   
        prettyPrintStructField (f, e) = 
            if null f 
                then prettyPrintExp e
                else f ++ "=" ++ prettyPrintExp e

prettyPrintExp (ENew t size) =
    "new " ++ prettyPrintType t ++ "[" ++ prettyPrintExp size ++ "]"
prettyPrintExp (EAssign lhs rhs) =
    prettyPrintExp lhs ++ " = " ++ prettyPrintExp rhs
prettyPrintExp (EBinary op l r) =
    "(" ++ prettyPrintExp l ++ " " ++ showOp op ++ " " ++ prettyPrintExp r ++ ")"
prettyPrintExp (EUnary op e) =
    showUnOp op ++ prettyPrintExp e
prettyPrintExp (EArrayLit es) =
    "[" ++ intercalate ", " (map prettyPrintExp es) ++ "]"

showOp :: BinOp -> String
showOp Or = "||"
showOp And = "&&"
showOp Eq = "=="
showOp Neq = "!="
showOp Gt = ">"
showOp Lt = "<"
showOp Ge = ">="
showOp Le = "<="
showOp Add = "+"
showOp Sub = "-"
showOp Mul = "*"
showOp Div = "/"

showUnOp :: UnOp -> String
showUnOp Not = "!"
showUnOp Neg = "-"

indent :: Int -> String
indent level =
    concat (replicate level indentation)
