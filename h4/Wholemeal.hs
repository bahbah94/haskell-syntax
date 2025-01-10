module Wholemeal where

fun1 :: [Integer] -> Integer
fun1 = product . map (\x -> x-2) . filter even

fun2 :: Integer -> Integer
fun2  = sum . filter even . takeWhile (>1) . iterate step 
  where
    step x = if even x then x `div` 2 else 3 * x + 1


--- exercise 2 --

data Tree a = Leaf
             | Node Integer (Tree a) a (Tree a)
    deriving (Show,Eq)


getHeight :: Tree a -> Integer
getHeight Leaf = 0
getHeight (Node h _ _ _) = h 

insertNode :: a -> Tree a -> Tree a 
insertNode x Leaf = Node 1 Leaf x Leaf
insertNode x (Node _ l v r)
  | getHeight l <= getHeight r = Node (1 + max (getHeight lNew) (getHeight r)) lNew x r
  | otherwise = Node (1 + max (getHeight l) (getHeight rNew)) l x rNew

  where 
    lNew = insertNode x l
    rNew = insertNode x r

foldTree :: [a] -> Tree a
foldTree = foldr insertNode Leaf


--- exercise 3 ---

xor :: [Bool] -> Bool
xor = foldr (\x acc -> if x then not acc else acc) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []


