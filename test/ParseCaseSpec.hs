module ParseCaseSpec where

import Test.HUnit

import Core
import ParseExpr
import Parse (
  Parser,
  parse, )

parseCaseGroupTest = test [
    TestLabel "caseAlternative simple" caseAlternativeSimpleTest,
    TestLabel "caseAlternative with spaces" caseAlternativeSpacedTest,
    TestLabel "parseCase simple" parseCaseSimpleTest
    -- TestLabel "parseCase parenthesis" parseCaseParenthesisTest TODO
  ]

{--- parseCase ---}

{- Case alternatives -}

caseAlternativeSimpleTest = TestCase (do
  let result1 = parse caseAlternative "<1> a1 -> 5"
  let expectedExpr1 = [((1, ["a1"], ENum 5), "")]
  assertEqual
    "Should parse alternatives with single variable '<1> a1 -> 5'"
    expectedExpr1 result1

  let result2 = parse caseAlternative "<1> a1 b2 -> 5"
  let expectedExpr2 = [((1, ["a1", "b2"], ENum 5), "")]
  assertEqual
    "Should parse alternatives with multiple variables '<1> a1 b2 -> 5'"
    expectedExpr2 result2

  let result3 = parse caseAlternative "<1> a1 -> 5;"
  let expectedExpr3 = [((1, ["a1"], ENum 5), ";")]
  assertEqual
    "Should parse alternatives with single variable '<1> a1 -> 5' with ';' left unconsumed"
    expectedExpr3 result3

  let result4 = parse caseAlternative "<1> -> 5"
  let expectedExpr4 = [((1, [], ENum 5), "")]
  assertEqual
    "Should parse alternatives without variables '<1> -> 5"
    expectedExpr4 result4
  
  let result5 = parse caseAlternative "<a> a1 -> 5"
  assertEqual
    "Should not parse alternatives with something different than a number"
    [] result5

  let result6 = parse caseAlternative "<1> b ->"
  assertEqual
    "Should not parse alternatives without an expression"
    [] result6

  let result7 = parse caseAlternative ""
  assertEqual
    "Should not parse empty input"
    [] result7)
  
caseAlternativeSpacedTest = TestCase (do
  let result1 = parse caseAlternative " < 1 > a1 -> 5 "
  let expectedExpr1 = [((1, ["a1"], ENum 5), "")]
  assertEqual
    "Should parse case with single variable ' < 1 > a1 -> 5 '"
    expectedExpr1 result1

  let result2 = parse caseAlternative "  < 1 >  a1  b2  ->  5  "
  let expectedExpr2 = [((1, ["a1", "b2"], ENum 5), "")]
  assertEqual
    "Should parse case with multiple variables '  < 1 >  a1  b2  ->  5  '"
    expectedExpr2 result2

  let result3 = parse caseAlternative " < 1 > a1 -> 5  ;  "
  let expectedExpr3 = [((1, ["a1"], ENum 5), ";  ")]
  assertEqual
    "Should parse case with single variable ' < 1 > a1 -> 5  ;  ' with ';  ' left unconsumed"
    expectedExpr3 result3
    

  let result4 = parse caseAlternative " < 1 > -> 5 "
  let expectedExpr4 = [((1, [], ENum 5), "")]
  assertEqual
    "Should parse alternatives without variables ' < 1 > -> 5 '"
    expectedExpr4 result4)

{- Case of -}

parseCaseSimpleTest = TestCase (do
  let result1 = parse parseCase "case 1 of <2> a1 -> 5"
  let expectedExpr1 = [((ECase (ENum 1) [(2, ["a1"], ENum 5)]), "")]
  assertEqual
    "Should parse case with single alternative 'case 1 of <1> a1 -> 5'"
    expectedExpr1 result1

  let result2 = parse parseCase "case 8 of <1> a1 -> 5; <2> a2 -> 6"
  let expectedExpr2 = [((ECase (ENum 8) [(1, ["a1"], ENum 5), (2, ["a2"], ENum 6)]), "")]
  assertEqual
    "Should parse case with multiple alternatives 'case 8 of <1> a1 -> 5; <2> a2 -> 6'"
    expectedExpr2 result2

  let result3 = parse parseCase "case 1 of <2> a1 -> 5;"
  let expectedExpr3 = [((ECase (ENum 1) [(2, ["a1"], ENum 5)]), ";")]
  assertEqual
    "Should parse case with single alternative 'case 1 of <2> a1 -> 5' with ';' left unconsumed"
    expectedExpr3 result3
  
  let result4 = parse parseCase "case 1 of"
  assertEqual
    "Should not parse case without alternatives"
    [] result4

  let result5 = parse parseCase ""
  assertEqual
    "Should not parse empty input"
    [] result5)