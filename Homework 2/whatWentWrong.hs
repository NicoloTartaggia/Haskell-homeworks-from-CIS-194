-- Exercise 5
module WhatWentWrong where
import Parse
import Log

isRelevant :: LogMessage -> Bool
isRelevant (LogMessage (Error severity) _ _) = severity >= 50
isRelevant _                                 = False

filterMsg :: [LogMessage] -> [String]
filterMsg ((LogMessage mt ts s):xs)
            | isRelevant (LogMessage mt ts s) == True = [s] ++ filterMsg xs
            | otherwise = filterMsg xs
filterMsg (Unknown _:xs) = filterMsg xs 
filterMsg [] = []

whatWentWrong :: [LogMessage] -> [String]
    whatWentWrong logMsgs = filterMsg sortedMsgs
                        where sortedMsgs = inOrder (build logMsgs)