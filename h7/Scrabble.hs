module Scrabble where
import Data.Monoid
import Data.Char
newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Semigroup Score where
    (<>) = (+)

instance Monoid Score where
 mempty  = Score 0
 mappend = (<>)


score :: Char -> Score
score c = Score $ 
    if not (isAlpha c) then 0
    else case toLower c of
        c' | c' `elem` "aeioulnstr" -> 1
           | c' `elem` "dg" -> 2
           | c' `elem` "bcmp" -> 3
           | c' `elem` "fhvwy" -> 4
           | c' == 'k' -> 5
           | c' `elem` "jx" -> 8
           | c' `elem` "qz" -> 10
           | otherwise -> 0

scoreString :: String -> Score
scoreString s = mconcat $ map score s