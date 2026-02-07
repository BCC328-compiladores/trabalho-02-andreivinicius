module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Data.Tree (drawTree)

import Exp.Frontend.Lexer.Alex.ExpLexer (lexer)
import Exp.Frontend.Parser.Happy.ExpParser (expParser)

import Exp.Frontend.Lexer.Token (Token, pos, lexeme, formatLexeme)
import Exp.Frontend.Parser.ASTFormat (showAST)
import Exp.Frontend.Pretty.ExpPretty (prettyPrint)
import Exp.Frontend.Semantic.ExpSemant (checkProgram)
import Exp.Interp.ExpInterp (interpProgram)

formatToken :: Token -> String
formatToken t =
  "Token {pos = " ++ show (pos t) ++ ", lexeme = " ++ formatLexeme (lexeme t) ++ "}"

runLexerPhase :: String -> IO ()
runLexerPhase src =
  case lexer src of
    Left err -> hPutStrLn stderr ("[LEXER ERROR]\n" ++ err)
    Right toks -> do
      putStrLn "=== TOKENS ==="
      mapM_ (putStrLn . formatToken) toks

runParserPhase :: String -> IO ()
runParserPhase src = do
  res <- expParser src
  case res of
    Left err -> hPutStrLn stderr ("[PARSER ERROR]\n" ++ err)
    Right ast -> do
      putStrLn "=== PARSER (AST) ==="
      putStrLn (drawTree (showAST ast))

runPrettyPhase :: String -> IO ()
runPrettyPhase src = do
  res <- expParser src
  case res of
    Left err -> hPutStrLn stderr ("[PARSER ERROR]\n" ++ err)
    Right ast -> do
      putStrLn "=== PRETTY PRINTING ==="
      putStrLn (prettyPrint ast)

runSemanticPhase :: String -> IO ()
runSemanticPhase src = do
  res <- expParser src
  case res of
    Left err -> hPutStrLn stderr ("[PARSER ERROR]\n" ++ err)
    Right ast -> case checkProgram ast of
      Left err -> hPutStrLn stderr ("[SEMANTIC ERROR]\n" ++ err)
      Right _ -> putStrLn "=== SEMANTIC CHECK OK ==="

runInterpPhase :: String -> IO ()
runInterpPhase src = do
  putStrLn "=== Interp Output ===\n"
  res <- expParser src
  case res of
    Left err -> hPutStrLn stderr ("[PARSER ERROR]\n" ++ err)
    Right ast -> case checkProgram ast of
      Left err -> hPutStrLn stderr ("[SEMANTIC ERROR]\n" ++ err)
      Right _ -> interpProgram ast

runAllPhases :: String -> IO ()
runAllPhases content = do
  runLexerPhase content
  putStrLn "\n-----------------------------------------------------\n"
  runParserPhase content
  putStrLn "\n-----------------------------------------------------\n"
  runPrettyPhase content
  putStrLn "\n-----------------------------------------------------\n"
  runSemanticPhase content
  putStrLn "\n-----------------------------------------------------\n"
  runInterpPhase content
  putStrLn "\n\n"

processFile :: String -> String -> IO ()
processFile content "lexer"   = runLexerPhase content
processFile content "--lexer" = runLexerPhase content
processFile content "parser"  = runParserPhase content
processFile content "--parser"= runParserPhase content
processFile content "pretty"  = runPrettyPhase content
processFile content "--pretty"= runPrettyPhase content
processFile content "check"   = runSemanticPhase content
processFile content "--check" = runSemanticPhase content
processFile content "interp"  = runInterpPhase content
processFile content "--interp"= runInterpPhase content
processFile _ option =
  hPutStrLn stderr
    ("Opcao invalida: " ++ option)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath, option] -> do
      content <- readFile filePath
      processFile content option

    [filePath] -> do
      content <- readFile filePath
      runAllPhases content

    _ ->
      hPutStrLn stderr
        "Uso: runghc -isrc app/Main.hs <caminho do arquivo> opcional -> [lexer | parser | pretty | check | interp | --lexer | --parser | --pretty | --check | --interp]"
