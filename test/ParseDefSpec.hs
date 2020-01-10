module ParseDefSpec where

import Test.HUnit

import Core
import ParseExpr
import Parse (
  Parser,
  parse, )

parseDefGroupTest = test [
    TestLabel "parseLet simple" parseLetSimpleTest,
    TestLabel "parseLet with spaces" parseLetSpacedTest,
    TestLabel "parseLetRec simple" parseLetRecSimpleTest,
    TestLabel "parseLetRec simple" parseLetRecSpacedTest
  ]

{--- parseDef ---}

{- ELet NonRecursive -}

parseLetSimpleTest = TestCase (do
  let result1 = parse parseDef "let a = 1 in 5"
  let expectedExpr1 = [(ELet NonRecursive [("a",ENum 1)] (ENum 5),"")]
  assertEqual
    "Should parse non-rec def with single variable 'let a = 1 in 5'"
    expectedExpr1 result1

  let result2 = parse parseDef "let a = 1; b = 2 in 5"
  let expectedExpr2 = [(ELet NonRecursive [("a",ENum 1), ("b",ENum 2)] (ENum 5),"")]
  assertEqual
    "Should parse non-rec def with multiple variables 'let a = 1; b = 2 in 5'"
    expectedExpr2 result2

  let result3 = parse parseDef "let a = 1 in 5;"
  let expectedExpr3 = [(ELet NonRecursive [("a",ENum 1)] (ENum 5),";")]
  assertEqual
    "Should parse non-rec def with single variable 'let a = 1 in 5;' with ';' left unconsumed"
    expectedExpr3 result3
  
  let result4 = parse parseDef "let 1b = 1 in 5"
  assertEqual
    "Should not parse non-rec def with variables that start with a number"
    [] result4

  let result5 = parse parseDef "let in 5"
  assertEqual
    "Should not parse non-rec def with no definitions"
    [] result5

  let result6 = parse parseDef ""
  assertEqual
    "Should not parse empty input"
    [] result6)
  
parseLetSpacedTest = TestCase (do
  let result1 = parse parseDef " let a = 1 in 5 "
  let expectedExpr1 = [(ELet NonRecursive [("a",ENum 1)] (ENum 5),"")]
  assertEqual
    "Should parse non-rec def with single variable ' let a = 1 in 5 '"
    expectedExpr1 result1

  let result2 = parse parseDef " let a = 1; b = 2 in 5 "
  let expectedExpr2 = [(ELet NonRecursive [("a",ENum 1), ("b",ENum 2)] (ENum 5),"")]
  assertEqual
    "Should parse non-rec def with multiple variables ' let a = 1; b = 2 in 5 '"
    expectedExpr2 result2

  let result3 = parse parseDef " let a = 1 in 5 ; "
  let expectedExpr3 = [(ELet NonRecursive [("a",ENum 1)] (ENum 5),"; ")]
  assertEqual
    "Should parse non-rec def with single variable ' let a = 1 in 5 ; ' with '; ' left unconsumed"
    expectedExpr3 result3)

{- ELet Recursive -}

parseLetRecSimpleTest = TestCase (do
  let result1 = parse parseDef "letrec a = 1 in 5"
  let expectedExpr1 = [(ELet Recursive [("a",ENum 1)] (ENum 5),"")]
  assertEqual
    "Should parse rec def with single variable 'letrec a = 1 in 5'"
    expectedExpr1 result1

  let result2 = parse parseDef "letrec a = 1; b = 2 in 5"
  let expectedExpr2 = [(ELet Recursive [("a",ENum 1), ("b",ENum 2)] (ENum 5),"")]
  assertEqual
    "Should parse rec def with multiple variables 'letrec a = 1; b = 2 in 5'"
    expectedExpr2 result2

  let result3 = parse parseDef "letrec a = 1 in 5;"
  let expectedExpr3 = [(ELet Recursive [("a",ENum 1)] (ENum 5),";")]
  assertEqual
    "Should parse rec def with single variable 'letrec a = 1 in 5;' with ';' left unconsumed"
    expectedExpr3 result3
  
  let result4 = parse parseDef "letrec 1b = 1 in 5"
  assertEqual
    "Should not parse rec def with variables that start with a number"
    [] result4

  let result5 = parse parseDef "letrec in 5"
  assertEqual
    "Should not parse rec def with no definitions"
    [] result5

  let result6 = parse parseDef ""
  assertEqual
    "Should not parse empty input"
    [] result6)
  
parseLetRecSpacedTest = TestCase (do
  let result1 = parse parseDef " letrec a = 1 in 5 "
  let expectedExpr1 = [(ELet Recursive [("a",ENum 1)] (ENum 5),"")]
  assertEqual
    "Should parse rec def with single variable ' letrec a = 1 in 5 '"
    expectedExpr1 result1

  let result2 = parse parseDef " letrec a = 1; b = 2 in 5 "
  let expectedExpr2 = [(ELet Recursive [("a",ENum 1), ("b",ENum 2)] (ENum 5),"")]
  assertEqual
    "Should parse rec def with multiple variables ' letrec a = 1; b = 2 in 5 '"
    expectedExpr2 result2

  let result3 = parse parseDef " letrec a = 1 in 5 ; "
  let expectedExpr3 = [(ELet Recursive [("a",ENum 1)] (ENum 5),"; ")]
  assertEqual
    "Should parse rec def with single variable ' letrec a = 1 in 5 ; ' with '; ' left unconsumed"
    expectedExpr3 result3)