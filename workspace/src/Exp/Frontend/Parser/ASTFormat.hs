module Exp.Frontend.Parser.ASTFormat (showAST) where

import Data.Tree (Tree(Node))
import Data.List (intercalate)
import Exp.Frontend.Syntax.ExpSyntax

showAST :: Program -> Tree String
showAST (Program structs funcs) =
    Node "Program" $
        (if null structs then [] else [Node "Structs" (map showStructDef structs)]) ++
        [Node "Funcs" (map showFuncDef funcs)]

showStructDef :: StructDef -> Tree String
showStructDef (StructDef name fields) =
    Node ("StructDef: " ++ name) $
        map (\(n, t) -> Node (n ++ " : " ++ showType t) []) fields

showFuncDef :: FuncDef -> Tree String
showFuncDef (FuncDef tvars name params retType body) =
    Node ("FuncDef: " ++ name) $
        (if not (null tvars)
            then [Node ("Type Vars: " ++ show tvars) []]
            else []) ++
        [Node "Params" (map (\(n, mt) -> Node (n ++ maybe "" (\t -> " : " ++ showType t) mt) []) params)] ++ 
        [Node ("Return Type: " ++ showMaybeType retType) []] ++ 
        [showStmt body]


showMaybeType :: Maybe Type -> String
showMaybeType Nothing = "void"
showMaybeType (Just t) = showType t

showType :: Type -> String
showType TypeInt = "int"
showType TypeFloat = "float"
showType TypeString = "string"
showType TypeBool = "bool"
showType TypeVoid = "void"
showType (TypeStruct n) = n
showType (TTypeVar v) = v
showType (TArray t) = showType t ++ "[]"
showType (TFuncType ts t) =
    "(" ++ intercalate ", " (map showType ts) ++ ") -> " ++ showType t

showStmt :: Stmt -> Tree String
showStmt (SBlock stmts) =
    Node "Block" (map showStmt stmts)

showStmt (SDecl name mType mExp) =
    Node ("Decl: " ++ name) $
        maybe [] (\t -> [Node ("Type: " ++ showType t) []]) mType ++
        maybe [] (\e -> [Node "Value" [showExp e]]) mExp

showStmt (SIf cond thenB elseB) =
    Node "If" $
        [Node "Condition" [showExp cond], showStmt thenB] ++
        maybe [] (\eB -> [Node "Else" [showStmt eB]]) elseB

showStmt (SWhile cond body) =
    Node "While" [Node "Condition" [showExp cond], showStmt body]

showStmt (SFor initS cond stepS body) =
    Node "For"
        [ Node "Init" [showStmt initS]
        , Node "Cond" [showExp cond]
        , Node "Step" [showStmt stepS]
        , showStmt body
        ]

showStmt (SReturn mExp) =
    Node "Return" (maybe [] (\e -> [showExp e]) mExp)

showStmt (SPrint expr) =
    Node "Print" [showExp expr]

showStmt (SExpr expr) =
    Node "Expression" [showExp expr]

showExp :: Exp -> Tree String
showExp (EInt i) =
    Node ("Int: " ++ show i) []

showExp (EFloat f) =
    Node ("Float: " ++ show f) []

showExp (EString s) =
    Node ("String: " ++ show s) []

showExp (EVar v) =
    Node ("Var: " ++ v) []

showExp (ECall name args) =
    Node ("Call: " ++ name) (map showExp args)

showExp (EIndex arr idx) =
    Node "Index"
        [ Node "Array" [showExp arr]
        , Node "Index" [showExp idx]
        ]

showExp (EField expr f) =
    Node ("Field: " ++ f) [showExp expr]

showExp (EStruct name fields) =
    Node ("Struct: " ++ name)
        (map (\(f, e) -> Node ("Field: " ++ f) [showExp e]) fields)

showExp (ENew t size) =
    Node ("New Array: " ++ showType t)
        [Node "Size" [showExp size]]

showExp (EAssign lhs rhs) =
    Node "Assign"
        [ Node "LHS" [showExp lhs]
        , Node "RHS" [showExp rhs]
        ]

showExp (EBinary op l r) =
    Node (show op) [showExp l, showExp r]

showExp (EUnary op e) =
    Node (show op) [showExp e]

showExp (EArrayLit es) =
    Node "Array Lit" (map showExp es)
