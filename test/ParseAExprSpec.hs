module ParseAExprSpec where

import Test.HUnit

import Core
import ParseExpr
import Parse (
  Parser,
  parse, )

parseEVarGroupTest = test [
    TestLabel "parseEVar simple" parseEVarSimpleIdentifierTest,
    TestLabel "parseEVar with spaces" parseEVarSpacedIdentifierTest
  ]

parseENumGroupTest = test [
    TestLabel "parseENum simple" parseENumSimpleTest,
    TestLabel "parseENum with spaces" parseENumSpacedTest
  ]

parseConstructorGroupTest = test [
    TestLabel "parseContructor simple" parseConstructorSimpleTest,
    TestLabel "parseENum number with spaces" parseConstructorSpacedTest
  ]

parseAExprGroupTest = test [
    TestLabel "parseAExpr EVar" parseAExprEVarTest,
    TestLabel "parseAExpr ENum" parseAExprENumTest,
    TestLabel "parseAExpr EConstr" parseAExprEConstrTest
  ]

parseAExprTestList = TestList [
    parseEVarGroupTest,
    parseENumGroupTest,
    parseConstructorGroupTest,
    parseAExprGroupTest
  ]

{--- parseEAxpr ---}

{- EVar -}

parseEVarSimpleIdentifierTest = TestCase (do
  let result1 = parse parseEVar "a"
  let expectedExpr1 = [(EVar "a", "")]
  assertEqual "Should parse 'a'" expectedExpr1 result1
  
  let result2 = parse parseEVar "a123 b"
  let expectedExpr2 = [(EVar "a123", "b")]
  assertEqual "Should parse 'a123 b' with 'b' left unconsumed" expectedExpr2 result2
  
  let result3 = parse parseEVar "a_"
  let expectedExpr3 = [(EVar "a_", "")]
  assertEqual "Should parse 'a_'" expectedExpr3 result3

  let result4 = parse parseEVar "1a23"
  assertEqual "Should not parse '1a23'" [] result4

  let result5 = parse parseEVar "-"
  assertEqual "Should not parse '-'" [] result5
  
  let result6 = parse parseEVar ";"
  assertEqual "Should not parse ';'" [] result6
  
  let result7 = parse parseEVar "."
  assertEqual "Should not parse '.'" [] result7
  
  let result8 = parse parseEVar "_"
  assertEqual "Should not parse '_'" [] result8)
  
parseEVarSpacedIdentifierTest = TestCase (do
  let result1 = parse parseEVar "  a  "
  let expectedExpr1 = [(EVar "a", "")]
  assertEqual "Should parse '  a  '" expectedExpr1 result1
  
  let result2 = parse parseEVar "     a123    "
  let expectedExpr2 = [(EVar "a123", "")]
  assertEqual "Should parse '    a123    '" expectedExpr2 result2
  
  let result3 = parse parseEVar " a_ "
  let expectedExpr3 = [(EVar "a_", "")]
  assertEqual "Should parse ' a_ '" expectedExpr3 result3)
  
{- ENum -}

parseENumSimpleTest = TestCase (do
  let result1 = parse parseENum "53"
  let expectedExpr1 = [(ENum 53, "")]
  assertEqual "Should parse '53'" expectedExpr1 result1
  
  let result2 = parse parseENum "53a"
  let expectedExpr2 = [(ENum 53, "a")]
  assertEqual "Should parse '53a' with 'a' left unconsumed" expectedExpr2 result2
  
  let result3 = parse parseENum "-5"
  assertEqual "Should not parse '-5'" result3 [])

parseENumSpacedTest = TestCase (do
  let result1 = parse parseENum "  53  "
  let expectedExpr1 = [(ENum 53, "")]
  assertEqual "Should parse '  53  '" expectedExpr1 result1
  
  let result2 = parse parseENum "  53a  "
  let expectedExpr2 = [(ENum 53, "a  ")]
  assertEqual "Should parse '  53a  ' with 'a  ' left unconsumed" expectedExpr2 result2
  
  let result3 = parse parseENum "  -5  "
  assertEqual "Should not parse '  -5  '" result3 [])

{- EPack -}

parseConstructorSimpleTest = TestCase (do
  let result1 = parse parseConstructor "Pack{1,0}"
  let expectedExpr1 = [(EConstr 1 0, "")]
  assertEqual "Should parse 'Pack{1,0}'" expectedExpr1 result1
  
  let result2 = parse parseConstructor "Pack{10,20}a123"
  let expectedExpr2 = [(EConstr 10 20, "a123")]
  assertEqual "Should parse 'Pack{10,20}a123' with 'a123' left unconsumed" expectedExpr2 result2
  
  let result3 = parse parseConstructor "Pack{10,a}"
  assertEqual "Should not parse 'Pack{10,a}'" result3 [])
  
parseConstructorSpacedTest = TestCase (do
  let result1 = parse parseConstructor "  Pack{1,0}  "
  let expectedExpr1 = [(EConstr 1 0, "")]
  assertEqual "Should parse '  Pack{1,0}  '" expectedExpr1 result1
  
  let result2 = parse parseConstructor "  Pack{10,20}a123  "
  let expectedExpr2 = [(EConstr 10 20, "a123  ")]
  assertEqual "Should parse '  Pack{10,20}a123  ' with 'a123  ' left unconsumed" expectedExpr2 result2
  
  let result3 = parse parseConstructor "  Pack{10,a}  "
  assertEqual "Should not parse '  Pack{10,a}  '" result3 [])

{- AExpr -}

parseAExprEVarTest = TestCase (do
  let result1 = parse parseAExpr "a123 b"
  let expectedExpr1 = [(EVar "a123", "b")]
  assertEqual "Should parse 'a123 b' with 'b' left unconsumed" expectedExpr1 result1)

parseAExprENumTest = TestCase (do
  let result1 = parse parseAExpr "53a"
  let expectedExpr1 = [(ENum 53, "a")]
  assertEqual "Should parse '53a' with 'a' left unconsumed" expectedExpr1 result1)

parseAExprEConstrTest = TestCase (do
  let result1 = parse parseAExpr "  Pack{10,20}a123  "
  let expectedExpr1 = [(EConstr 10 20, "a123  ")]
  assertEqual "Should parse '  Pack{10,20}a123  ' with 'a123  ' left unconsumed" expectedExpr1 result1)