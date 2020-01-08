{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

{-
contains the module Parse and in particular the definition of the Parser as a newtype, the definitions of the instances of
Parser as a member of Functor, Applicative and Monad and also Alternative.
Moreover, this module should also contain all the simple parsers of Chapter 13
of the book that are useful for defining the module ParseProg.
-}

module Parse (
    Parser (..),
    alpha,
    alphaNum,
    char,
    parse,
    string,
    token,
    symbol,
    natToken,
) where

import Control.Applicative (
  Alternative,
  (<|>),
  some,
  many,
  empty, )
import Data.Char (
  isSpace,
  isDigit,
  isAlpha,
  isAlphaNum, )

-- Parser consumes part of a string and returns the parsed value
-- and the unconsumed part of the input string. If the parse fails,
-- [] is returned.
newtype Parser a = P (String -> [(a, String)])

-- parser p x runs the parser p on the input x and returns the result
-- wrapped in a singleton. If the result is the empty list, it means that
-- an error occurred.
parse :: Parser a -> String -> [(a, String)]
parse (P p) x = p x

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\x -> case parse p x of
                        []        -> []
                        [(v, rest)] -> [(g v, rest)])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\x -> [(v, x)])

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\x -> case parse pg x of
                          [] -> []
                          [(g, rest)] -> parse (fmap g px) rest)

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\x -> case parse p x of
                        [] -> []
                        [(v, rest)] -> parse (f v) rest)

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\_ -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = P (\x -> case parse p1 x of
                  [] -> parse p2 x
                  p  -> p) 

-- parser that succeeds if there's a character to read
item :: Parser Char
item = let p' []     = []
           p' (x:xs) = [(x, xs)]
       in P (p')

-- parser that succeeds when the given predicate succeeds
predParser :: (Char -> Bool) -> Parser Char
predParser predicate = do
    x <- item
    if predicate x then return x else empty

{- Parser utils -}

-- parses an ASCII digit.
-- Returns the parsed character.
digit :: Parser Char
digit = predParser isDigit

-- char c parses a single character c.
-- Returns the parsed character (i.e. c).
char :: Char -> Parser Char
char c = predParser (== c)

-- parses an alphabetic Unicode characters.
-- Returns the parsed character.
alpha :: Parser Char
alpha = predParser isAlpha

-- parses an alphabetic or numeric Unicode characters.
-- Returns the parsed character.
alphaNum :: Parser Char
alphaNum = predParser isAlphaNum

-- parses a natural number (a positive whole number). 
-- Returns the value of the number
nat :: Parser Int
nat = do
    xs <- some digit
    return (read xs)

-- parses a white space character and returns it
space :: Parser Char
space = predParser isSpace

-- skips zero or more white space characters
spaces :: Parser ()
spaces = many space >> pure ()

-- string s parses a sequence of characters given by s.
-- Returns the parsed string (i.e. s).
string :: String -> Parser String
string [] = return []
string lst@(x:xs) = char x *> string xs *> pure lst

-- token p first applies the spaces parser, then the given parser p and then space parser again, returning the value of p.
-- Every lexical token is defined using token, this way every parse starts at a point without white space.
token :: Parser a -> Parser a
token p = spaces *> p <* spaces

-- parser that succeeds when it reads a certain token
symbol :: String -> Parser String
symbol x = token (string x)

-- parses a natural number (a positive whole number), possibly surrounded by whitespace. 
-- Returns the value of the number
natToken :: Parser Int
natToken = token nat
