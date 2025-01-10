module Golf where

import Data.List (group, sort)

skips :: [a] -> [[a]]
skips xs = [[x | (x,i) <- zip xs [1..], i `mod` n == 0] | n <- [1..length xs]]

localMaxima :: [Integer] -> [Integer]
localMaxima xs = [ y | (x,y,z) <- zip3 xs (tail xs) (tail (tail xs)), x < y && y > z]

histogram :: [Integer] -> String
histogram xs = unlines (rows ++ ["==========", "0123456789"])
  where
    counts = map length . group . sort $ xs
    maxHeight = maximum (counts ++ [0])
    rows = [row n | n <- [maxHeight,maxHeight-1..1]]
    row n = [if length (filter (==x) xs) >= n then '*' else ' ' | x <- [0..9]]
