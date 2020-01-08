{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module ParseProgr where

import Control.Applicative (
  many, )

import Core
import Parse
import ParseUtils
import ParseExpr

-- Parses a list of supercombinator definitions
parseProg :: Parser CoreProgram
parseProg = sepBySemicolon parseSuperCombinator

{- ParseSuperCombinator -}

-- Core supercombinator expression parser
parseSuperCombinator :: Parser CoreScDefn
parseSuperCombinator = do
    scId <- validIdentifier
    params <- many validIdentifier
    definition
    expr <- parseExpr
    return (scId, params, expr)
