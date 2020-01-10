module Main where

import Test.HUnit

import Core
import ParseAExprSpec
import ParseLambdaSpec
import ParseCaseSpec
import ParseDefSpec
import Parse (
  Parser,
  parse, )

main = runTestTT $
  TestList [
    parseAExprTestList,
    parseLambdaGroupTest,
    parseCaseGroupTest,
    parseDefGroupTest
  ]