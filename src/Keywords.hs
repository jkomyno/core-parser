{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Keywords where

import Data.Set (
  Set,
  fromList,
  member, )

-- reserved keywords list
keywordList :: [String]
keywordList = [
  "case",
  "in",
  "let",
  "letrec",
  "of",
  "Pack"]

-- convert keywordList into a set for faster lookup
keywordSet :: Set String
keywordSet = fromList keywordList

-- utility that returns True iff the given string is
-- a Core lang keyword
isKeyword :: String -> Bool
isKeyword x = x `member` keywordSet
