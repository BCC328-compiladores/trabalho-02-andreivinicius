module Exp.Frontend.Syntax.ExpSyntax where

data BinOp
    = Or | And
    | Eq | Neq | Gt | Lt | Ge | Le
    | Add | Sub | Mul | Div
    deriving (Show, Eq)

data UnOp
    = Not | Neg
    deriving (Show, Eq)

data Type
    = TypeInt
    | TypeFloat
    | TypeString
    | TypeBool
    | TypeVoid
    | TypeStruct String
    | TTypeVar String
    | TArray Type
    | TFuncType [Type] Type
    deriving (Show, Eq)

data Exp
    = EInt Integer
    | EFloat Double
    | EString String
    | EVar String
    | ECall String [Exp]
    | EIndex Exp Exp
    | EField Exp String
    | EStruct String [(String, Exp)]
    | ENew Type Exp
    | EAssign Exp Exp
    | EBinary BinOp Exp Exp
    | EUnary UnOp Exp
    | EArrayLit [Exp]
    deriving (Show, Eq)

data Stmt
    = SBlock [Stmt]
    | SDecl String (Maybe Type) (Maybe Exp)
    | SIf Exp Stmt (Maybe Stmt)
    | SWhile Exp Stmt
    | SFor Stmt Exp Stmt Stmt
    | SReturn (Maybe Exp)
    | SPrint Exp
    | SExpr Exp
    deriving (Show, Eq)

data StructDef = StructDef
    { structName :: String
    , structFields :: [(String, Type)]
    }
    deriving (Show, Eq)

data FuncDef = FuncDef
    { funcTypeVars :: [String]
    , funcName     :: String
    , funcParams   :: [(String, Maybe Type)]
    , funcReturnType :: Maybe Type
    , funcBody     :: Stmt
    }
    deriving (Show, Eq)

data Program = Program
    { programStructs :: [StructDef]
    , programFuncs   :: [FuncDef]
    }
    deriving (Show, Eq)