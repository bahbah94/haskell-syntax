--{-# OPTIONS_GHC -Wall #-}
--module LogAnalysis where
import System.IO
import Log

parseMessage :: String -> LogMessage
parseMessage mes = case words mes of  
     ("I":timestamp:message) -> LogMessage Info (read timestamp) (unwords message)
     ("E":category:timestamp:message) -> LogMessage (Error (read category)) (read timestamp) (unwords message)
     ("W":timestamp:message) -> LogMessage Warning (read timestamp) (unwords message)
     _ -> Unknown mes

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMsg Leaf = Node Leaf logMsg Leaf
insert logMsg@(LogMessage _ timestamp _) ((Node left nodeMsg@(LogMessage _ nodeTime _) right))
     | timestamp < nodeTime = Node (insert logMsg left) nodeMsg right
     | otherwise = Node left nodeMsg (insert logMsg right)

build :: [LogMessage] -> MessageTree
build messages = foldr insert Leaf messages

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right 

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = case inOrder (build messages) of
    [] -> []
    (LogMessage (Error sev) _ msg:xs) 
        | sev >= 50 -> msg : whatWentWrong xs
        | otherwise -> whatWentWrong xs
    (_:xs) -> whatWentWrong xs

main :: IO ()
main = do
    putStrLn "Testing parse function with sample.log:"
    messages <- testParse parse 10 "error.log"
    mapM_ print messages