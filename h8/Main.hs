module Main where

import Employee
import Party
import Data.List (sort)  -- for sorting names

main :: IO ()
main = do
    companyStr <- readFile "company.txt"
    let company = parseCompany companyStr
        bestList = maxFun company
    putStrLn $ formatGuestList bestList