--{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module JoinList where
import Sized
import Data.Monoid
import Scrabble

data JoinList m a = Empty
                    | Single m a
                    | Append m (JoinList m a) (JoinList m a)
 deriving (Eq, Show)

--- exercise 1 ---

tag :: Monoid m => JoinList m a -> m 
tag Empty = mempty
tag (Single m _) = m 
tag (Append m _ _) = m 

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a 
jl1 +++ jl2 = Append (tag jl1 `mappend` tag jl2) jl1 jl2 


 --- exercise 2 ---


indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
-- Base cases
indexJ _ Empty = Nothing
indexJ i (Single _ x)
    | i == 0    = Just x      -- Found it!
    | otherwise = Nothing     -- Out of bounds

-- Recursive case for Append
indexJ i (Append _ l r)
    | i < 0     = Nothing     -- Negative index
    | i < leftSize = indexJ i l           -- Go left, keep i
    | otherwise = indexJ (i - leftSize) r -- Go right, subtract leftSize
    where 
        leftSize = getSize (size (tag l))  -- Get size of left subtree

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty 
dropJ n jl | n <= 0 = jl 
dropJ n (Single _ _) | n > 0 = Empty
dropJ n (Append m l r)
 | n >= leftSize = dropJ (n - leftSize) r
 | otherwise = (dropJ n l)  +++ r 
 where leftSize = getSize $ size $ tag l



takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ | n <= 0 = Empty
takeJ _ Empty = Empty
takeJ n s@(Single _ _) | n > 0 = s
takeJ n (Append m l r)
    | n > leftSize = l +++ takeJ (n - leftSize) r     -- take all left and some right
    | otherwise    = takeJ n l                        -- take some left only
    where leftSize = getSize $ size $ tag l



--- exercise 3 --
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s