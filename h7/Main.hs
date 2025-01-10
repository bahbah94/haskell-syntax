module Main where

import Buffer
import JoinList
import Scrabble
import Sized
import Editor
import Data.Monoid
import StringBuffer

main :: IO ()
main = runEditor editor $ fromString $ unlines
    [ "This buffer is for notes you don't want to save,"
    , "and for evaluation of steam valve coefficients."
    , "To load a different file, type the character L"
    , "followed by the name of the file."
    ]