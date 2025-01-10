-- exercise 1 ---
toDigits :: Integer -> [Integer]
toDigits n 
    | n <= 0 = []
    | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
    | n <= 0 = []
    | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

-- exercise 2 --
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (zipWith (*) (reverse xs) (cycle [1,2]))

-- Exercise 3: Sum all digits including those from doubled numbers
sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ concatMap splitDigits xs
  where
    splitDigits n
        | n < 10    = [n]
        | otherwise = splitDigits (n `div` 10) ++ [n `mod` 10]

-- exercise 4 --
validate :: Integer -> Bool
validate n = (sumDigits $ doubleEveryOther $ toDigits n) `mod` 10 == 0


type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n source dest temp
     | n <= 0 = []
     | n == 1 = [(source, dest)]
     | otherwise = hanoi (n-1) source dest temp ++ [(source,temp)] ++ hanoi (n-1) temp dest source

main :: IO ()
main = do
    --putStrLn "Enter number to convert to list"
    --num <- readLn :: IO Integer
    --putStrLn $ "Result: " ++ show (toDigitsRev num)
    --print (doubleEveryOther [1,2,3,4,5])
    --print (sumDigits [12,3,4,23])
    ---print (validate 4012888888881882)
    putStrLn "Enter number of discs:"
    n <- readLn :: IO Integer
    putStrLn $ "Moves to solve Tower of Hanoi with " ++ show n ++ " discs:"
    putStrLn $ show $ hanoi n "a" "b" "c"

