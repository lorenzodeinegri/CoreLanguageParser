module ParseProg where

import Control.Applicative
import Parser

type Name      = String
type Program a = [ScDef a]
type ScDef a   = (Name, [a], Expr a)
type Def a     = (a, Expr a)
type Alter a   = (Int, [a], Expr a)

data IsRec     = NonRecursive | Recursive
                 deriving Show
data Expr a    = EVar Name
               | ENum Int
               | EConstr Int Int
               | EAp (Expr a) (Expr a)
               | ELet IsRec [Def a] (Expr a)
               | ECase (Expr a) [Alter a]
               | ELam [a] (Expr a)
                 deriving Show

parseProg :: Parser (Program Name)
parseProg = do function <- parseScDef
               do symbol ";"
                  functions <- parseProg
                  return (function:functions)
                <|> return [function]

parseScDef :: Parser (ScDef Name)
parseScDef = do name <- parseVar
                parameters <- many parseVar
                symbol "="
                body <- parseExpr
                return (name, parameters, body)

parseExpr :: Parser (Expr Name)
parseExpr = do return (EVar "TODO")

parseAExpr :: Parser (Expr Name)
parseAExpr = do return (EVar "TODO")

parseDef :: Parser (Def Name)
parseDef = do return ("TODO", EVar "TODO")

parseAlt :: Parser (Alter Name)
parseAlt = do return (1, ["TODO"], EVar "TODO")

parseVar :: Parser (String)
parseVar = do return "TODO"
