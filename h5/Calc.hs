{-# LANGUAGE TypeSynonymInstances #-}
module Calc where
import ExprT
import Parser
import StackVM
import qualified StackVM as S
import qualified ExprT as E

eval :: ExprT -> Integer
eval (E.Lit n) = n
eval (E.Add e1 e2) = (eval e1) + (eval e2)
eval (E.Mul e1 e2) = (eval e1) * (eval e2)

--exercise 2--

evalStr:: String -> Maybe Integer
evalStr s = eval <$> parseExp E.Lit E.Add E.Mul s

-- exercise 3 --

class Expr a where 
 add :: a -> a -> a
 mul :: a -> a -> a
 lit :: Integer -> a


instance Expr ExprT where
  lit n = E.Lit n
  add e1 e2 = E.Add e1 e2
  mul e1 e2 = E.Mul e1 e2

reify:: ExprT -> ExprT
reify = id 


-- exercise 4 --

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
 lit n = n
 add e1 e2 = e1 + e2
 mul e1 e2 = e1 * e2

instance Expr Bool where
 lit x = x > 0
 add x y = x || y
 mul x y = x && y

instance Expr MinMax where
 lit x = MinMax x
 add (MinMax x) (MinMax y) = MinMax (max x y)  
 mul (MinMax x) (MinMax y) = MinMax (min x y) 

instance Expr Mod7 where
 lit x = Mod7 (x `mod` 7)
 add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
 mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)

-- exercise 5 --
instance Expr Program where
 lit x = [PushI x]
 add x y = x ++ y ++ [S.Add]
 mul x y =  x ++ y ++ [S.Mul]

 --- the above instance only handles Integers --

compile :: String -> Maybe Program
compile s = parseExp lit add mul s