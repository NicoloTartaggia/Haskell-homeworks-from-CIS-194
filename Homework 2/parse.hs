{-# OPTIONS_GHC -Wall #-}
module Parse where
import Log

-- Exercise 1
getLastWords :: [String] -> String
getLastWords xs = unwords xs

parseMessage :: String -> LogMessage
parseMessage xs 
            | splittedWords!!0 == "E" = LogMessage (Error (read (splittedWords!!1) :: Int)) (read (splittedWords!!2) :: Int) (getLastWords (drop 3 splittedWords))
            | splittedWords!!0 == "I" = LogMessage Info (read (splittedWords!!1) :: Int) (getLastWords (drop 2 splittedWords))
            | splittedWords!!0 == "W" = LogMessage Warning (read (splittedWords!!1) :: Int) (getLastWords (drop 2 splittedWords))
            | otherwise = Unknown (getLastWords (drop 0 splittedWords))
            where splittedWords = words xs

parse :: String -> [LogMessage]
parse xs = map (parseMessage) errorLines 
            where errorLines = lines xs 

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMsg Leaf = Node Leaf logMsg Leaf
insert (LogMessage mt1 ts1 s1) (Node left (LogMessage mt2 ts2 s2) right) 
                        | ts1 >= ts2 = (Node left (LogMessage mt2 ts2 s2) (insert (LogMessage mt1 ts1 s1) right))
                        | otherwise = (Node (insert (LogMessage mt1 ts1 s1) left) (LogMessage mt2 ts2 s2) right)
insert _ (Node _ (Unknown _) _) =
    error "Unknown messages are not allowed in MessageTree"

-- Exercise 3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logMsg right) = inOrder left ++ [logMsg] ++ inOrder right