{
module Exp.Frontend.Parser.Happy.ExpParser (expParser, parseProgram, parseExp) where

import Exp.Frontend.Lexer.Alex.ExpLexer hiding (lexer)
import qualified Exp.Frontend.Lexer.Token as Tok
import Exp.Frontend.Lexer.Token (Token(..), formatLexeme)

import Exp.Frontend.Syntax.ExpSyntax
import qualified Exp.Frontend.Syntax.ExpSyntax as Syntax

import Control.Monad
}

%name parseProgram Program
%name parser Exp
%monad {Alex}{(>>=)}{return}
%tokentype {Token}
%error {parseError}
%lexer {lexer}{Token _ Tok.TEOF}

%token
      num         {Token _ (Tok.TNumber $$)}
      float       {Token _ (Tok.TFloatLit $$)}
      str         {Token _ (Tok.TStringLit $$)}
      ident       {Token _ (Tok.TIdent $$)}
      typevar     {Token _ (Tok.TTypeVar $$)}
      'func'      {Token _ Tok.TFunc}
      'let'       {Token _ Tok.TLet}
      'while'     {Token _ Tok.TWhile}
      'for'       {Token _ Tok.TFor}
      'if'        {Token _ Tok.TIf}
      'else'      {Token _ Tok.TElse}
      'return'    {Token _ Tok.TReturn}
      'print'     {Token _ Tok.TPrint}
      'struct'    {Token _ Tok.TStruct}
      'new'       {Token _ Tok.TNew}
      'forall'    {Token _ Tok.TAll}
      'int'       {Token _ Tok.TInt}
      'float'     {Token _ Tok.TFloat}
      'string'    {Token _ Tok.TString}
      'bool'      {Token _ Tok.TBool}
      'void'      {Token _ Tok.TVoid}
      '>='        {Token _ Tok.TGte}
      '<='        {Token _ Tok.TLte}
      '=='        {Token _ Tok.TEq}
      '!='        {Token _ Tok.TDiff}
      '&&'        {Token _ Tok.TAnd}
      '||'        {Token _ Tok.TOr}
      '>'         {Token _ Tok.TGt}
      '<'         {Token _ Tok.TLt}
      '+'         {Token _ Tok.TPlus}
      '-'         {Token _ Tok.TMinus}
      '*'         {Token _ Tok.TTimes}
      '/'         {Token _ Tok.TDiv}
      '!'         {Token _ Tok.TNot}
      '='         {Token _ Tok.TAssign}
      '->'        {Token _ Tok.TArrow}
      '('         {Token _ Tok.TLParen}
      ')'         {Token _ Tok.TRParen}
      '{'         {Token _ Tok.TLBrace}
      '}'         {Token _ Tok.TRBrace}
      '['         {Token _ Tok.TLBracket}
      ']'         {Token _ Tok.TRBracket}
      ':'         {Token _ Tok.TColon}
      ';'         {Token _ Tok.TSemicolon}
      ','         {Token _ Tok.TComma}
      '.'         {Token _ Tok.TDot}

%nonassoc NOELSE
%nonassoc 'else'
%nonassoc '='
%left '||'
%left '&&'
%nonassoc '==' '!=' '>' '<' '>=' '<='
%left '+' '-'
%left '*' '/'
%right '!'
%right UNOP
%right '->'
%nonassoc 'forall'
%nonassoc '.'

%%

Var :: {String}
    : ident        { $1 }
    | typevar      { $1 }

Program :: {Program}
    : Structs Funcs        { Syntax.Program $1 $2 }

Structs :: {[StructDef]}
    :                     { [] }
    | Struct Structs      { $1 : $2 }

Funcs :: {[FuncDef]}
    :                     { [] }
    | Func Funcs          { $1 : $2 }

Struct :: {StructDef}
    : 'struct' ident '{' Fields '}'   { Syntax.StructDef $2 $4 }

Fields :: {[(String, Type)]}
    :                     { [] }
    | NonEmptyFields      { $1 }

NonEmptyFields :: {[(String, Type)]}
    : Field               { [$1] }
    | NonEmptyFields Field { $1 ++ [$2] }

Field :: {(String, Type)}
    : ident ':' Type ';'  { ($1, $3) }

Func :: {FuncDef}
    : 'forall' TypeVarList '.' 'func' ident '(' Params ')' ReturnTypeOpt Block
        { Syntax.FuncDef $2 $5 $7 $9 $10 }
    | 'func' ident '(' Params ')' ReturnTypeOpt Block
        { Syntax.FuncDef [] $2 $4 $6 $7 }

TypeVarList :: {[String]}
    : Var              { [$1] }
    | TypeVarList Var  { $1 ++ [$2] }

Params :: {[(String, Maybe Type)]}
    :                     { [] }
    | ParamList           { reverse $1 }

ParamList :: {[(String, Maybe Type)]}
    : Param                             { [$1] }
    | ParamList ',' Param               { $3 : $1 }

Param :: {(String, Maybe Type)}
    : ident ':' Type                    { ($1, Just $3) }
    | ident                             { ($1, Nothing) }

ReturnTypeOpt :: {Maybe Type}
    : ':' Type                          { Just $2 }
    |                                  { Nothing }

Block :: {Stmt}
    : '{' Stmts '}'        { Syntax.SBlock $2 }

Stmts :: {[Stmt]}
    :                      { [] }
    | Stmt Stmts           { $1 : $2 }

Stmt :: {Stmt}
    : Exp ';'                               { Syntax.SExpr $1 }
    | 'let' Var ':' Type '=' Exp ';'         { Syntax.SDecl $2 (Just $4) (Just $6) }
    | 'let' Var ':' Type ';'                { Syntax.SDecl $2 (Just $4) Nothing }
    | 'let' Var '=' Exp ';'                 { Syntax.SDecl $2 Nothing (Just $4) }
    | 'if' '(' Exp ')' Stmt %prec NOELSE    { Syntax.SIf $3 $5 Nothing }
    | 'if' '(' Exp ')' Stmt Else            { Syntax.SIf $3 $5 $6 }
    | 'while' '(' Exp ')' Stmt              { Syntax.SWhile $3 $5 }
    | 'for' '(' ForInit ';' Exp ';' ForStep ')' Stmt
                                            { Syntax.SFor $3 $5 $7 $9 }
    | 'return' ExpOpt ';'                   { Syntax.SReturn $2 }
    | 'print' '(' Exp ')' ';'               { Syntax.SPrint $3 }
    | Block                                 { $1 }

Else :: {Maybe Stmt}
    : 'else' Stmt      { Just $2 }

ForInit :: {Stmt}
    :                   { Syntax.SExpr (Syntax.EInt 0) }
    | 'let' Var '=' Exp { Syntax.SDecl $2 Nothing (Just $4) }
    | Exp               { Syntax.SExpr $1 }

ForStep :: {Stmt}
    :                   { Syntax.SExpr (Syntax.EInt 0) }
    | Exp               { Syntax.SExpr $1 }

ExpOpt :: {Maybe Exp}
    :                   { Nothing }
    | Exp               { Just $1 }

Exp :: {Exp}
    : Term                                  { $1 }
    | Exp '=' Exp                           { Syntax.EAssign $1 $3 }
    | Exp '||' Exp                          { Syntax.EBinary Syntax.Or $1 $3 }
    | Exp '&&' Exp                          { Syntax.EBinary Syntax.And $1 $3 }
    | Exp '==' Exp                          { Syntax.EBinary Syntax.Eq $1 $3 }
    | Exp '!=' Exp                          { Syntax.EBinary Syntax.Neq $1 $3 }
    | Exp '>' Exp                           { Syntax.EBinary Syntax.Gt $1 $3 }
    | Exp '<' Exp                           { Syntax.EBinary Syntax.Lt $1 $3 }
    | Exp '>=' Exp                          { Syntax.EBinary Syntax.Ge $1 $3 }
    | Exp '<=' Exp                          { Syntax.EBinary Syntax.Le $1 $3 }
    | Exp '+' Exp                           { Syntax.EBinary Syntax.Add $1 $3 }
    | Exp '-' Exp                           { Syntax.EBinary Syntax.Sub $1 $3 }
    | Exp '*' Exp                           { Syntax.EBinary Syntax.Mul $1 $3 }
    | Exp '/' Exp                           { Syntax.EBinary Syntax.Div $1 $3 }
    | '!' Exp                               { Syntax.EUnary Syntax.Not $2 }
    | '-' Exp %prec UNOP                    { Syntax.EUnary Syntax.Neg $2 }
    | '(' Exp ')'                           { $2 }

Term :: {Exp}
    : num                                   { Syntax.EInt (fromIntegral $1) }
    | float                                 { Syntax.EFloat $1 }
    | str                                   { Syntax.EString $1 }
    | Var                                   { Syntax.EVar $1 }
    | ident '(' Args ')'                    { Syntax.ECall $1 $3 }
    | Term '[' Exp ']'                      { Syntax.EIndex $1 $3 }
    | Term '.' ident                        { Syntax.EField $1 $3 }
    | ident '{' StructFields '}'            { Syntax.EStruct $1 $3 }
    | 'new' BasicType '[' Exp ']'           { Syntax.ENew $2 $4 }
    | '[' Args ']'                          { Syntax.EArrayLit $2 }

Args :: {[Exp]}
    :                      { [] }
    | ArgList              { reverse $1 }

ArgList :: {[Exp]}
    : Exp                  { [$1] }
    | ArgList ',' Exp      { $3 : $1 }

StructFields :: {[(String, Exp)]}
    :                      { [] }
    | ArgList              { map (\e -> ("", e)) (reverse $1) }

Type :: {Type}
    : ArrowType        { $1 }

ArrowType :: {Type}
    : AtomicType '->' ArrowType          { Syntax.TFuncType [$1] $3 }
    | '(' Type ')' '->' ArrowType        { Syntax.TFuncType [$2] $5 }
    | '(' TypeList2 ')' '->' ArrowType   { Syntax.TFuncType $2 $5 }
    | AtomicType                         { $1 }

AtomicType :: {Type}
    : BasicType '[' Exp ']'   { Syntax.TArray $1 }
    | BasicType '[' ']'        { Syntax.TArray $1 }
    | BasicType                { $1 }
    | typevar                  { Syntax.TTypeVar $1 }

BasicType :: {Type}
    : 'int'       { Syntax.TypeInt }
    | 'float'     { Syntax.TypeFloat }
    | 'string'    { Syntax.TypeString }
    | 'bool'      { Syntax.TypeBool }
    | 'void'      { Syntax.TypeVoid }
    | ident       { Syntax.TypeStruct $1 }

TypeList2 :: {[Type]}
    : Type ',' Type            { [$1, $3] }
    | TypeList2 ',' Type       { $1 ++ [$3] }

{
parseError (Token (line, col) lexeme) =
    alexError ("Parse error at line " ++ show line ++ ", column " ++ show col
               ++ "\nUnexpected token: " ++ formatLexeme lexeme)

lexer :: (Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)

expParser :: String -> IO (Either String Program)
expParser content = pure (runAlex content parseProgram)

parseExp :: String -> IO (Either String Exp)
parseExp content = pure (runAlex content parser)
}