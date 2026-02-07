{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Exp.Frontend.Lexer.Alex.ExpLexer where

import Control.Monad
import Exp.Frontend.Lexer.Token
}

%wrapper "monadUserState"

$digit   = 0-9
$alpha   = [a-zA-Z]
$idchar  = [a-zA-Z0-9_]
$typevar = [a-z]

@number  = $digit+
@float   = $digit+ "." $digit+
@string  = \"([^\"\\]|\\.)*\"

tokens :-

    -- whitespace and comments
    <0> $white+ ;
    <0> "//".*  ;

    <0> "/*"                { nestComment `andBegin` state_comment }
    <0> "*/"                { \_ _ -> alexError "Unexpected */" }
    <state_comment> "/*"    { nestComment }
    <state_comment> "*/"    { unnestComment }
    <state_comment> .       ;
    <state_comment> \n      ;

    -- Keywords
    <0> "func" { simpleToken TFunc }
    <0> "let" { simpleToken TLet }
    <0> "while" { simpleToken TWhile }
    <0> "for" { simpleToken TFor }
    <0> "if" { simpleToken TIf }
    <0> "else" { simpleToken TElse }
    <0> "return" { simpleToken TReturn }
    <0> "print" { simpleToken TPrint }
    <0> "struct" { simpleToken TStruct }
    <0> "new" { simpleToken TNew }
    <0> "forall" { simpleToken TAll }

    -- types
    <0> "int" { simpleToken TInt }
    <0> "float" { simpleToken TFloat }
    <0> "string" { simpleToken TString }
    <0> "bool" { simpleToken TBool }
    <0> "void" { simpleToken TVoid }

    -- operators
    <0> ">=" { simpleToken TGte }
    <0> "<=" { simpleToken TLte }
    <0> "==" { simpleToken TEq }
    <0> "!=" { simpleToken TDiff }
    <0> "&&" { simpleToken TAnd }
    <0> "||" { simpleToken TOr }
    <0> "->" { simpleToken TArrow }
    <0> ">" { simpleToken TGt }
    <0> "<" { simpleToken TLt }
    <0> "+" { simpleToken TPlus }
    <0> "-" { simpleToken TMinus }
    <0> "*" { simpleToken TTimes }
    <0> "/" { simpleToken TDiv }
    <0> "!" { simpleToken TNot }
    <0> "=" { simpleToken TAssign }

    -- punctuation
    <0> "(" { simpleToken TLParen }
    <0> ")" { simpleToken TRParen }
    <0> "{" { simpleToken TLBrace }
    <0> "}" { simpleToken TRBrace }
    <0> "[" { simpleToken TLBracket }
    <0> "]" { simpleToken TRBracket }
    <0> ":" { simpleToken TColon }
    <0> ";" { simpleToken TSemicolon }
    <0> "," { simpleToken TComma }
    <0> "." { simpleToken TDot }

    -- literals
    <0> @float  { mkFloat }
    <0> @number { mkNumber }
    <0> @string { mkString }

    -- identifiers
    <0> $alpha $idchar*   { mkIdent }

{
-- STATE
data AlexUserState = AlexUserState
  { nestLevel :: Int }

alexInitUserState = AlexUserState 0

get = Alex $ \s -> Right (s, alex_ust s)
put st = Alex $ \s -> Right (s{alex_ust = st}, ())
modify f = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())

alexEOF = do
  (p,_,_,_) <- alexGetInput
  if nestLevel alexInitUserState > 0
     then alexError "Unclosed comment"
     else pure $ Token (position p) TEOF

position (AlexPn _ l c) = (l,c)

-- COMMENT HANDLING
nestComment inp len = modify (\s -> s{nestLevel = nestLevel s + 1}) >> skip inp len
unnestComment inp len = do
  s <- get
  let lvl = nestLevel s - 1
  put s{nestLevel = lvl}
  when (lvl == 0) (alexSetStartCode 0)
  skip inp len

-- TOKEN BUILDERS
simpleToken lx (st,_,_,_) _ = pure (Token (position st) lx)

mkNumber (st,_,_,str) n =
  pure $ Token (position st) (TNumber (read (take n str)))

mkFloat (st,_,_,str) n =
  pure $ Token (position st) (TFloatLit (read (take n str)))

mkString (st,_,_,str) n =
  let s = take n str
      unq = init (tail s)
  in pure $ Token (position st) (TStringLit unq)

mkIdent (st,_,_,str) n =
  let ident = take n str
  in pure $ Token (position st) (TIdent ident)

mkTypeVar (st,_,_,str) n =
  pure $ Token (position st) (TTypeVar (take n str))

-- MAIN
lexer :: String -> Either String [Token]
lexer s = runAlex s go
  where
    go = do
      t <- alexMonadScan
      if lexeme t == TEOF
         then pure [t]
         else (t :) <$> go
}
