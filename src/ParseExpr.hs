{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module ParseExpr where

import Control.Applicative (
  (<|>),
  some, )

import Core
import Parse
import ParseUtils

{- Expression parsers

The following part takes care of respecting the associativity
precedences given in the table at section 1.2 of the book
"Implementing Functional Languages: a tutorial".
The main idea is using Recursive Descent parsing using
Precedence Climbing, as explained in detail in
http://www.engr.mun.ca/~theo/Misc/exp_parsing.htm#more_climbing.
The idea of recursive-descent parsing is to transform each nonterminal
of a grammar into a subroutine that will recognize exactly
that nonterminal in the input.
The higher the precedence, the tighter the binding.
-}

-- Parser for Core expressions
parseExpr :: Parser CoreExpr
parseExpr =  parseDef       -- definitions
         <|> parseCase      -- case of expressions
         <|> parseLambda    -- lambda-abstraction expressions
         <|> parseExprPrec1 -- operator expressions with lowest precedence

-- Left-recursive expression application machinery.
-- This would be `mk_ap_chain` of exercise 1.23
applicationChain :: [CoreExpr] -> CoreExpr
applicationChain [] = error "Function application expected"
applicationChain (e:es) = foldl EAp e es

-- Returns a Core language expression for two sub-expressions tied
-- together by an infix binary operator.
-- lhsParser is the parser for left-hand side expressions.
-- opParser is the parser for the infix binary operator symbol
-- rhsParser is the parser for the right-hand side expressions.
parseBinaryOperator :: Parser CoreExpr -> Parser String -> Parser CoreExpr -> Parser CoreExpr
parseBinaryOperator lhsParser opParser rhsParser = do
    lhs <- lhsParser -- left-hand side expressions
    op <- opParser   -- infix binary operator
    rhs <- rhsParser -- right-hand side expressions
    return ((EAp (EAp (EVar op) lhs)) rhs)

-- Parser for disjunction expressions (lhs | rhs).
-- Since it is right-associative, the rhs parser is defined recursively.
-- If it fails, it falls back to the parser of immediate higher precedence.
parseExprPrec1 :: Parser CoreExpr
parseExprPrec1 = parseBinaryOperator parseExprPrec2 orOperator parseExprPrec1 -- right associative
             <|> parseExprPrec2

-- Parser for conjunction expressions (lhs & rhs).
-- Since it is right-associative, the lhs parser is the parser of immediately
-- higher precedence, whereas the rhs parser is defined recursively.
-- If it fails, it falls back to the parser of immediate higher precedence.
parseExprPrec2 :: Parser CoreExpr
parseExprPrec2 = parseBinaryOperator parseExprPrec3 andOperator parseExprPrec2 -- right associative
             <|> parseExprPrec3

-- Parser for comparison expressions (lhs `op` rhs, where `op` is a comparison operator).
-- Since it is not-associative, both the lhs parser and the rhs parser are the
-- parser of immediately higher precedence.
-- If it fails, it falls back to the parser of immediate higher precedence.
parseExprPrec3 :: Parser CoreExpr
parseExprPrec3 = parseBinaryOperator parseExprPrec4 anyRelop parseExprPrec4 -- not associative
             <|> parseExprPrec4

-- Parser for addition and subtraction expressions. These 2 kinds of expressions
-- are handled by the same parser because they have the exact same precedence.
-- Since `+` is right-associative, the lhs parser is the parser of immediately
-- higher precedence, whereas the rhs parser is defined recursively.
-- Since `-` is not associative, both the lhs parser and the rhs parser are the
-- parser of immediately higher precedence.
-- If it fails, it falls back to the parser of immediate higher precedence.
parseExprPrec4 :: Parser CoreExpr
parseExprPrec4 = parseBinaryOperator parseExprPrec5 plusOperator parseExprPrec4  -- right-associative
             <|> parseBinaryOperator parseExprPrec5 minusOperator parseExprPrec5 -- not associative
             <|> parseExprPrec5

-- Parser for multiplications and division expressions. These 2 kinds of expressions
-- are handled by the same parser because they have the exact same precedence.
-- Since `*` is right-associative, the lhs parser is the parser of immediately
-- higher precedence, whereas the rhs parser is defined recursively.
-- Since `/` is not associative, both the lhs parser and the rhs parser are the
-- parser of immediately higher precedence.
-- If it fails, it falls back to the parser of immediate higher precedence.
parseExprPrec5 :: Parser CoreExpr
parseExprPrec5 = parseBinaryOperator parseExprPrec6 timesOperator parseExprPrec5    -- right-associative
             <|> parseBinaryOperator parseExprPrec6 divisionOperator parseExprPrec6 -- not associative
             <|> parseExprPrec6

-- Parser for function application expressions.
-- Since the function application is left-associative, the applicationChain
-- machinery must be used.      
parseExprPrec6 :: Parser CoreExpr
parseExprPrec6 = fmap applicationChain (some parseAExpr) -- left-associative

{- Atomic expression parser -}

parseAExpr :: Parser CoreExpr
parseAExpr = parseEVar            -- Variable
         <|> parseENum            -- Number
         <|> parseConstructor     -- Constructor
         <|> parseWrappedInParens -- Parenthesised expression

{- Parser for variables -}

parseEVar :: Parser CoreExpr
parseEVar = do name <- validIdentifier
               return (EVar name)

{- Parser for number literals -}

parseENum :: Parser CoreExpr
parseENum = do num <- natToken
               return (ENum num)

{- Parser for constructors -}

parseConstructor :: Parser CoreExpr
parseConstructor = do
    constructorKeyword
    (tag, arity) <- constructorValues
    return (EConstr tag arity) 

-- parses the Pack constructor body, i.e. 2 numbers separated by a comma
-- wrapped in curly parenthesis.
-- Returns the pair of numbers.
constructorValues :: Parser (Int, Int)
constructorValues = do
    openCurlyParens
    tag <- natToken
    constructorSeparator
    arity <- natToken
    closedCurlyParens
    return (tag, arity)

{- Parser for expressions wrapped in parenthesis -}

parseWrappedInParens :: Parser CoreExpr
parseWrappedInParens = openStdParens *> parseExpr <* closedStdParens

{- Parser for case-in expressions -}

parseCase :: Parser CoreExpr
parseCase = do
    caseKeyword
    expr <- parseExpr
    ofKeyword
    alts <- sepBySemicolon caseAlternative
    return (ECase expr alts)

caseAlternative :: Parser CoreAlter 
caseAlternative = do
    (num, vars) <- altHead
    thinArrowToRight
    expr <- parseExpr
    return (num, vars, expr)

{- Parser for let/letrec definitions -}

parseDef :: Parser CoreExpr
parseDef = letRecDef <|> letDef

letDef :: Parser CoreExpr
letDef = genericDef letKeyword NonRecursive

letRecDef :: Parser CoreExpr
letRecDef = genericDef letRecKeyword Recursive

singleDef :: Parser CoreDef
singleDef = do
    -- validIdentifier prevents mistreating keywords for variable names
    defId <- validIdentifier
    definition
    expr <- parseExpr
    return (defId, expr)

-- the process needed to parse let and let-rec expressions is the same, hence both
-- parseLet and parseLetRec invoke this utility method
genericDef :: Parser String -> IsRec -> Parser CoreExpr
genericDef defKeyword isRec = do
    defKeyword
    defns <- sepBySemicolon $ singleDef
    inKeyword
    expr <- parseExpr
    return (ELet isRec defns expr)

{- Parser for lambda expressions -}

parseLambda :: Parser CoreExpr
parseLambda = do
    lambdaPreamble
    -- at least a lambda parameter is required
    params <- parseLambdaParams
    lambdaBodyStart
    body <- parseExpr
    return (ELam params body)

parseLambdaParams :: Parser [Name]
parseLambdaParams = some validIdentifier
