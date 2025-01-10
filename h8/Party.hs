module Party where
import Employee
import Data.Monoid
import Data.Tree
import Data.List (sort)



--- exercise 1 ---

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) = GL (emp:emps) (fun + empFun emp)

instance Semigroup GuestList where
    (GL emp1 fun1) <> (GL emp2 fun2) = GL (emp1 ++ emp2) (fun1 + fun2)

instance Monoid GuestList where
 mempty = GL [] 0
 mappend = (<>)


moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 = if gl1 > gl2 then gl1 else gl2


--- exercise 2 ---


treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node val forest) = f val (map (treeFold f) forest)

-- exercise 3 --

type Results = (GuestList, GuestList)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel me childResults = (withBoss, withoutBoss)
 where
  withBoss = glCons me (mconcat $ map snd childResults)
  withoutBoss = mconcat $ map (uncurry moreFun) childResults


--- exercise 4 --

maxFun :: Tree Employee -> GuestList
maxFun tree = uncurry moreFun $ treeFold nextLevel tree


--- exercise 5 ---
parseCompany :: String -> Tree Employee
parseCompany = read 


formatGuestList :: GuestList -> String
formatGuestList (GL emps fun) = 
    "Total fun: " ++ show fun ++ "\n" ++
    unlines (sort $ map empName emps)



