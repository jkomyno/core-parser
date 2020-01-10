module ParseLambdaSpec where

import Test.HUnit

import Core
import ParseExpr
import Parse (
  Parser,
  parse, )

parseLambdaGroupTest = test [
    TestLabel "parseLambda simple" parseLambdaSimpleTest,
    TestLabel "parseLambda with spaces" parseLambdaSpacedTest,
    TestLabel "parseLambdaParams simple" parseLambdaParamsSimpleTest,
    TestLabel "parseLambdaParams with spaces" parseLambdaParamsSpacedTest
  ]

{--- parseELam ---}

{- ELam -}

parseLambdaSimpleTest = TestCase (do
  let result1 = parse parseLambda "\\a1.a1+2"
  let expectedExpr1 = [(ELam ["a1"] (EAp (EAp (EVar "+") (EVar "a1")) (ENum 2)),"")]
  assertEqual
    "Should parse lambda with single param '\\a1.a1+2'"
    expectedExpr1 result1

  let result2 = parse parseLambda "\\a1 a2.a1+a2"
  let expectedExpr2 = [(ELam ["a1","a2"] (EAp (EAp (EVar "+") (EVar "a1")) (EVar "a2")),"")]
  assertEqual
    "Should parse lambda with multiple params '\\a1 a2.a1+a2'"
    expectedExpr2 result2

  let result3 = parse parseLambda "\\a1.a1+2."
  let expectedExpr3 = [(ELam ["a1"] (EAp (EAp (EVar "+") (EVar "a1")) (ENum 2)),".")]
  assertEqual
    "Should parse lambda with single param '\\a1.a1+2.' with '.' left unconsumed"
    expectedExpr3 result3

  let result4 = parse parseLambda "\\a1+2"
  assertEqual 
    "Should not parse lambda with 0 params" [] result4

  let result5 = parse parseLambda ""
  assertEqual
    "Should not parse empty lambda" [] result5)

parseLambdaSpacedTest = TestCase (do
  let result1 = parse parseLambda " \\ a1 . a1 + 2 "
  let expectedExpr1 = [(ELam ["a1"] (EAp (EAp (EVar "+") (EVar "a1")) (ENum 2)),"")]
  assertEqual
    "Should parse lambda with single param ' \\ a1 . a1 + 2 '"
    expectedExpr1 result1

  let result2 = parse parseLambda "\\ a1 a2 . a1 + a2 "
  let expectedExpr2 = [(ELam ["a1","a2"] (EAp (EAp (EVar "+") (EVar "a1")) (EVar "a2")),"")]
  assertEqual
    "Should parse lambda with multiple params '\\ a1 a2 . a1 + a2 '"
    expectedExpr2 result2

  let result3 = parse parseLambda " \\ a1 . a1 + 2 . "
  let expectedExpr3 = [(ELam ["a1"] (EAp (EAp (EVar "+") (EVar "a1")) (ENum 2)),". ")]
  assertEqual 
    "Should parse lambda with single param ' \\ a1 . a1 + a2 . ' with '. ' left unconsumed"
    expectedExpr3 result3)

{- Lambda params -}

parseLambdaParamsSimpleTest = TestCase (do
  let result1 = parse parseLambdaParams "a1"
  let expectedExpr1 = [(["a1"], "")]
  assertEqual "Should parse single param 'a1'" expectedExpr1 result1

  let result2 = parse parseLambdaParams "a1 b2 c3"
  let expectedExpr2 = [(["a1", "b2", "c3"], "")]
  assertEqual "Should parse multiple params 'a1 b2 c3'" expectedExpr2 result2

  let result3 = parse parseLambdaParams "a1 ."
  let expectedExpr3 = [(["a1"], ".")]
  assertEqual "Should parse single param 'a1' with '.' left unconsumed" expectedExpr3 result3
  
  let result4 = parse parseLambdaParams ""
  assertEqual "Should not parse empty input" result4 [])
  
parseLambdaParamsSpacedTest = TestCase (do
  let result1 = parse parseLambdaParams "  a1  "
  let expectedExpr1 = [(["a1"], "")]
  assertEqual "Should parse single param '  a1  '" expectedExpr1 result1

  let result2 = parse parseLambdaParams "  a1  b2  c3  "
  let expectedExpr2 = [(["a1", "b2", "c3"], "")]
  assertEqual "Should parse multiple params '  a1  b2  c3  '" expectedExpr2 result2

  let result3 = parse parseLambdaParams "  a1  .  "
  let expectedExpr3 = [(["a1"], ".  ")]
  assertEqual "Should parse single param '  a1  .  ' with '.  ' left unconsumed" expectedExpr3 result3)