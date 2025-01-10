{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where
import Data.Char (isUpper,isSpace,isAlpha,isAlphaNum)

import AParser
import Control.Applicative

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = ((:) <$> p <*>  zeroOrMore p) <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> (oneOrMore p <|> pure [])

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (++) <$> oneOrMore (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show


-- Helper functions for separating by spaces
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (p `sepBy1` sep) <|> pure []

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)
 where
  many x = zeroOrMore x

-- Atom parser
parseAtom :: Parser Atom
parseAtom = (N <$> posInt) <|> (I <$> ident)

-- Main S-expression parser
parseSExpr :: Parser SExpr 
parseSExpr = spaces *> ((A <$> parseAtom) <|> parseList) <* spaces
 where
  parseList = Comb <$> (char '(' *> 
                        spaces *> 
                        sepBy parseSExpr spaces <* 
                        spaces <* 
                        char ')')          -- Closing paren