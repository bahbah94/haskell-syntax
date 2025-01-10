module Fibonacci where

-- exercise 1 ---
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

fib1 ::[Integer]
fib1 = map fib [0..]

--exercise 2 --
fib2 :: [Integer]
fib2 = 0 : scanl (+) 1 fib2


--- exercise 3 --
data Stream a = Cons a (Stream a)

streamToList:: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
   show stream = "Stream " ++ show (take 20 (streamToList stream))


--- exercise 4 ----
streamRepeat :: a -> Stream a 
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))


-- exxercise 5 --

nats :: Stream Integer
nats = streamFromSeed (+1) 0


ruler :: Stream Integer
ruler = streamMap largestPowerOf2 nats
  where
    largestPowerOf2 :: Integer -> Integer
    largestPowerOf2 n
      | n == 0    = 0
      | otherwise = fromIntegral (countTrailingZeros n)

    countTrailingZeros :: Integer -> Int
    countTrailingZeros 0 = 0
    countTrailingZeros n = length (takeWhile even (iterate (`div` 2) n))

