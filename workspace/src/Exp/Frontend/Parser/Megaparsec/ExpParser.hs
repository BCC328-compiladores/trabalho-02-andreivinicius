module Exp.Frontend.Parser.Megaparsec.ExpParser (expParser) where

import Exp.Frontend.Syntax.ExpSyntax
import Exp.Frontend.Lexer.Megaparsec.ExpLexer

import Text.Megaparsec
import Control.Monad.Combinators.Expr

-- definition of operator table

opTable :: [[Operator Parser Exp]]
opTable
  = [ [ infixL (bin Mul) "*" ]
    , [ infixL (bin Add) "+" ]
    ]
 where
  bin op a b = EBinary op a b
  infixL op sym = InfixL $ op <$ symbol sym

    <|> EInt <$> int

expP :: Parser Exp
expP = makeExprParser termP opTable

expParser :: String -> Either String Exp
expParser s = case parse expP "" s of
                Left err -> Left (errorBundlePretty err)
                Right e  -> Right e
