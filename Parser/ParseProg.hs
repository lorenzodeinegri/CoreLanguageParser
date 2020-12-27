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
parseExpr = do symbol "let"
               definitions <- some parseDefs
               symbol "in"
               expression <- parseExpr
               return (ELet NonRecursive definitions expression)
            <|>
            do symbol "letrec"
               definitions <- some parseDefs
               symbol "in"
               expression <- parseExpr
               return (ELet Recursive definitions expression)
            <|>
            do symbol "case"
               expression <- parseExpr
               symbol "of"
               alternatives <- some parseAlts
               return (ECase expression alternatives)
            <|>
            do symbol "\\"
               variables <- some parseVar
               symbol "."
               expression <- parseExpr
               return (ELam variables expression)
            <|>
            do expression <- parseAExpr
               return expression

parseAExpr :: Parser (Expr Name)
parseAExpr = do variable <- parseVar
                return (EVar variable)
             <|>
             do number <- natural
                return (ENum number)
             <|>
             do symbol "Pack{"
                number1 <- natural
                symbol ","
                number2 <- natural
                symbol "}"
                return (EConstr number1 number2)
             <|>
             do symbol "("
                expression <- parseExpr
                symbol ")"
                return expression

parseDefs :: Parser (Def Name)
parseDefs = do def <- parseDef
               symbol ";"
               return def

parseDef :: Parser (Def Name)
parseDef = do variable <- parseVar
              symbol "="
              expression <- parseExpr
              return (variable, expression)

parseAlts :: Parser (Alter Name)
parseAlts = do alt <- parseAlt
               symbol ";"
               return alt

parseAlt :: Parser (Alter Name)
parseAlt = do symbol "<"
              number <- natural
              symbol ">"
              variables <- many parseVar
              symbol "->"
              expression <- parseExpr
              return (number, variables, expression)

parseVar :: Parser (Name)
parseVar = do variable <- identifier
              if isKeyword variable then empty else return variable
              
-- parseVar = do space
--               first <- letter
--               rest <- many (letter <|> digit <|> char '_')
--               space
--               if isKeyword (first:rest) then empty else return (first:rest)

isKeyword :: String -> Bool
isKeyword s = s == "let" || s == "letrec" || s == "case" || s == "Pack" || s == "of" || s == "if"
