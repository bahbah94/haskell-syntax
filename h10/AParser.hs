{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import Control.Applicative

import Data.Char
import Control.Applicative



-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------


--- exercise 1 ---
instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> 
        case p input of
            Nothing -> Nothing
            Just (x, rest) -> Just (f x, rest)

--- exercise 2 ---
instance Applicative Parser where
  --pure :: a -> Parser a
  pure x = Parser $ \input -> Just (x, input)
  -- <*> :: Parse (a -> b) -> Parser a -> Parser b
  Parser p1 <*> Parser p2 = Parser $ \input -> 
   case p1 input of 
    Nothing -> Nothing
    Just (f, rest) ->
     case p2 rest of 
      Nothing -> Nothing 
      Just (x, rest') -> Just (f x, rest')


--- exercise 3 ----

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\a b -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\x _ y -> [x,y]) <$> posInt <*> char ' ' <*> posInt


--- exercise 4 --

instance Alternative Parser where
 empty = Parser $ \input -> Nothing 
 Parser p1 <|> Parser p2 = Parser $ \input ->
  case p1 input of
   Nothing -> p2 input
   Just (x, rest) -> Just (x, rest)

--- exercsise 5 ---

intOrUppercase :: Parser ()
intOrUppercase = ((\_ -> ()) <$> posInt) <|> ((\_ -> ()) <$> satisfy isUpper)