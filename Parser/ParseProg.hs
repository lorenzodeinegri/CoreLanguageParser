module ParseProg where

import Control.Applicative
import Parse

type Name      = String
type Program a = [ScDef a]
type ScDef a   = (Name, [a], Expr a)
type Def a     = (a, Expr a)
type Alter a   = (Int, [a], Expr a)

data IsRec     = NonRecursive | Recursive
                 deriving (Show, Eq)
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
                <|>
                  return [function]

parseScDef :: Parser (ScDef Name)
parseScDef = do name <- parseVar
                parameters <- many parseVar
                symbol "="
                body <- parseExpr
                return (name, parameters, body)

parseExpr :: Parser (Expr Name)
parseExpr = do symbol "let"
               definitions <- parseDefs
               symbol "in"
               expression <- parseExpr
               return (ELet NonRecursive definitions expression)
            <|>
            do symbol "letrec"
               definitions <- parseDefs
               symbol "in"
               expression <- parseExpr
               return (ELet Recursive definitions expression)
            <|>
            do symbol "case"
               expression <- parseExpr
               symbol "of"
               alternatives <- parseAlts
               return (ECase expression alternatives)
            <|>
            do symbol "\\"
               variables <- some parseVar
               symbol "."
               expression <- parseExpr
               return (ELam variables expression)
            <|>
            do expression <- parseExpr1
               return expression

parseExpr1 :: Parser (Expr Name)
parseExpr1 = do expr2 <- parseExpr2
                do or <- parseOr
                   expr1 <- parseExpr1
                   return (EAp (EAp or expr2) expr1)
                 <|>
                   return expr2

parseExpr2 :: Parser (Expr Name)
parseExpr2 = do expr3 <- parseExpr3
                do and <- parseAnd
                   expr2 <- parseExpr2
                   return (EAp (EAp and expr3) expr2)
                 <|>
                   return expr3

parseExpr3 :: Parser (Expr Name)
parseExpr3 = do expr4 <- parseExpr4
                do op <- parseRelop
                   expr4' <- parseExpr4
                   return (EAp (EAp op expr4) expr4')
                 <|>
                   return expr4

parseExpr4 :: Parser (Expr Name)
parseExpr4 = do expr5 <- parseExpr5
                do add <- parseAdd
                   expr4 <- parseExpr4
                   return (EAp (EAp add expr5) expr4)
                   <|>
                   do sub <- parseSub
                      expr5' <- parseExpr5
                      return (EAp (EAp sub expr5) expr5')
                   <|>
                   return expr5

parseExpr5 :: Parser (Expr Name)
parseExpr5 = do expr6 <- parseExpr6
                do mul <- parseMul
                   expr5 <- parseExpr5
                   return (EAp (EAp mul expr6) expr5)
                   <|>
                   do div <- parseDiv
                      expr6' <- parseExpr6
                      return (EAp (EAp div expr6) expr6')
                   <|>
                   return expr6

parseExpr6 :: Parser (Expr Name)
parseExpr6 = do first <- parseAExpr
                do rest <- some parseAExpr
                   return (foldl (EAp) first rest)
                 <|>
                   return first

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

parseDefs :: Parser ([Def Name])
parseDefs = do def <- parseDef
               do symbol ";"
                  defs <- parseDefs
                  return (def:defs)
                <|>
                  return [def]

parseDef :: Parser (Def Name)
parseDef = do variable <- parseVar
              symbol "="
              expression <- parseExpr
              return (variable, expression)

parseAlts :: Parser ([Alter Name])
parseAlts = do alt <- parseAlt
               do symbol ";"
                  alts <- parseAlts
                  return (alt:alts)
                <|>
                  return [alt]

parseAlt :: Parser (Alter Name)
parseAlt = do symbol "<"
              number <- natural
              symbol ">"
              variables <- many parseVar
              symbol "->"
              expression <- parseExpr
              return (number, variables, expression)

parseVar :: Parser (Name)
parseVar = do space
              first <- letter
              rest <- many (letter <|> digit <|> char '_')
              space
              if isKeyword (first:rest) then empty else return (first:rest)

parseOr :: Parser (Expr Name)
parseOr = do or <- symbol "|"
             return (EVar or)

parseAnd :: Parser (Expr Name)
parseAnd = do and <- symbol "&"
              return (EVar and)

parseAdd :: Parser (Expr Name)
parseAdd = do add <- symbol "+"
              return (EVar add)

parseSub :: Parser (Expr Name)
parseSub = do sub <- symbol "-"
              return (EVar sub)

parseMul :: Parser (Expr Name)
parseMul = do mul <- symbol "*"
              return (EVar mul)

parseDiv :: Parser (Expr Name)
parseDiv = do div <- symbol "/"
              return (EVar div)

parseRelop :: Parser (Expr Name)
parseRelop = do op <- (symbol "==" <|> symbol "~=" <|> symbol ">" <|> symbol ">=" <|> symbol "<" <|> symbol "<=")
                return (EVar op)

isKeyword :: String -> Bool
isKeyword s = s == "let" || s == "letrec" || s == "case" || s == "Pack" || s == "of" || s == "in"
