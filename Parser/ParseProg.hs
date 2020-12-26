module ParseProg where

import Control.Applicative
import Parser

type Name = String
type Program a = [ScDef a]
type ScDef a = (Name, [a], Expr a)
data Expr a = EVar a
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

parseVar :: Parser (String)
parseVar = do return "TODO"
