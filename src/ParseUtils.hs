{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module ParseUtils where

import Control.Applicative (
  many,
  empty,
  (<|>), )

import Parse
import Keywords (
  isKeyword, )
import Core (
  Name,
  relopList, )

{- Token parsers and parser utils -}

-- anyOf ss succeeds if the current string is in the supplied list of strings ss.
-- Returns the parsed character.
anyOf :: [String] -> Parser String
anyOf [] = empty
anyOf (x:xs) = string x <|> anyOf xs

-- parses a relop (comparison operator) symbol, possibly surrounded by whitespace.
-- Returns the parsed symbol
anyRelop :: Parser String
anyRelop = token $ anyOf relopList

-- parses a valid variable character. A variable should only be composed of alphabetic
-- and numeric symbols or the '_' (underscore) symbol.
varch :: Parser Char
varch = alphaNum <|> char '_'

-- parses a possible variable/function identifier.
-- This parser isn't aware of the language keywords.
-- Returns the parsed identifier
identifier :: Parser String
identifier = do
    -- the book doesn't specify it, but, in order to be Haskell-compliant,
    -- the first character of an identifier should be lower-case
    x <- alpha
    xs <- many varch
    return (x:xs)

-- parser that succeeds when it reads an identifier that is not
-- a keyword of the Core language
validIdentifier :: Parser String
validIdentifier = do name <- token identifier
                     if isKeyword name
                     then empty
                     else return name

-- parser that succeeds when it reads the token <NUM> and
-- returns the matched number
altMatch :: Parser Int
altMatch = openAltBracket *> natToken <* closedAltBracket

-- parser that succeeds when it reads the token
-- `<num> var1 ... varN`, with N >= 0
-- and returns the matched number and variables
altHead :: Parser (Int, [Name])
altHead = do num <- altMatch
             vars <- many validIdentifier
             return (num, vars)

sepBy :: Parser a -> String -> Parser [a]
sepBy parser separator = do x <- parser
                            xs <- many (symbol separator >> parser)
                            return (x:xs)

sepBySemicolon :: Parser a -> Parser [a]
sepBySemicolon parser = sepBy parser ";"

{- Keywords/operators/special string parsers -}

openStdParens :: Parser String
openStdParens = symbol "("

closedStdParens :: Parser String
closedStdParens = symbol ")"

openCurlyParens :: Parser String
openCurlyParens = symbol "{"

closedCurlyParens :: Parser String
closedCurlyParens = symbol "}"

letKeyword :: Parser String
letKeyword = symbol "let"

letRecKeyword :: Parser String
letRecKeyword = symbol "letrec"

inKeyword :: Parser String
inKeyword = symbol "in"

constructorKeyword :: Parser String
constructorKeyword = symbol "Pack"

constructorSeparator :: Parser String
constructorSeparator = symbol ","

caseKeyword :: Parser String
caseKeyword = symbol "case"

ofKeyword :: Parser String
ofKeyword = symbol "of"

orOperator :: Parser String
orOperator = symbol "|"

andOperator :: Parser String
andOperator = symbol "&"

minusOperator :: Parser String
minusOperator = symbol "-"

plusOperator :: Parser String
plusOperator = symbol "+"

timesOperator :: Parser String
timesOperator = symbol "*"

divisionOperator :: Parser String
divisionOperator = symbol "/"

openAltBracket :: Parser String
openAltBracket = symbol "<"

closedAltBracket :: Parser String
closedAltBracket = symbol ">"

thinArrowToRight :: Parser String
thinArrowToRight = symbol "->"

definition :: Parser String
definition = symbol "="

-- the first '\' character needs to be escaped
lambdaPreamble :: Parser String
lambdaPreamble = symbol "\\"

lambdaBodyStart :: Parser String
lambdaBodyStart = symbol "."
