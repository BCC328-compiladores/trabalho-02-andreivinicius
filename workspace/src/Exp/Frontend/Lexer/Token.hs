module Exp.Frontend.Lexer.Token where

data Token = Token
  { pos :: (Int, Int)
  , lexeme :: Lexeme
  }
  deriving(Eq)

data Lexeme
  = TFunc | TLet | TWhile | TFor | TIf | TElse
  | TReturn | TPrint | TStruct | TNew | TAll
  | TInt | TFloat | TString | TBool | TVoid
  | TGte | TLte | TEq | TDiff | TAnd | TOr
  | TGt | TLt | TPlus | TMinus | TTimes | TDiv
  | TNot | TAssign | TArrow
  | TLParen | TRParen | TLBrace | TRBrace
  | TLBracket | TRBracket | TColon | TSemicolon
  | TComma | TDot
  | TNumber Int | TFloatLit Double | TStringLit String
  | TIdent String | TTypeVar String
  | TEOF
  deriving(Eq)

formatLexeme :: Lexeme -> String
formatLexeme lexe = case lexe of
    TFunc        -> "TFunc"
    TLet         -> "TLet"
    TWhile       -> "TWhile"
    TFor         -> "TFor"
    TIf          -> "TIf"
    TElse        -> "TElse"
    TReturn      -> "TReturn"
    TPrint       -> "TPrint"
    TStruct      -> "TStruct"
    TNew         -> "TNew"
    TAll         -> "TAll"
    TInt         -> "TInt"
    TFloat       -> "TFloat"
    TString      -> "TString"
    TBool        -> "TBool"
    TVoid        -> "TVoid"
    TGte         -> "TGte"
    TLte         -> "TLte"
    TEq          -> "TEq"
    TDiff        -> "TDiff"
    TAnd         -> "TAnd"
    TOr          -> "TOr"
    TGt          -> "TGt"
    TLt          -> "TLt"
    TPlus        -> "TPlus"
    TMinus       -> "TMinus"
    TTimes       -> "TTimes"
    TDiv         -> "TDiv"
    TNot         -> "TNot"
    TAssign      -> "TAssign"
    TArrow       -> "TArrow"
    TLParen      -> "TLParen"
    TRParen      -> "TRParen"
    TLBrace      -> "TLBrace"
    TRBrace      -> "TRBrace"
    TLBracket    -> "TLBracket"
    TRBracket    -> "TRBracket"
    TColon       -> "TColon"
    TSemicolon   -> "TSemicolon"
    TComma       -> "TComma"
    TDot         -> "TDot"
    TNumber n    -> "TNumber " ++ show n
    TFloatLit f  -> "TFloatLit " ++ show f
    TStringLit s -> "TStringLit \"" ++ s ++ "\""
    TIdent s     -> "TIdent " ++ s
    TTypeVar s   -> "TTypeVar " ++ s
    TEOF         -> "TEOF"

formatToken :: Token -> String
formatToken t =
  "Token {pos = " ++ show (pos t) ++ ", lexeme = " ++ formatLexeme (lexeme t) ++ "}"
