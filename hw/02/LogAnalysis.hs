{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
import Data.List hiding (insert)

-- Exercise 1

split :: Char -> String -> [String]
split c s = split' c s [] [] where
                split' :: Char -> String -> String -> [String] -> [String]
                split' _ [] [] l = reverse l
                split' _ [] w l = reverse ((reverse w):l)
                split' c' (x:xs) w l | c' == x = split' c' xs [] ((reverse w):l)
                                    | otherwise = split' c' xs (x:w) l

parseMessage :: String -> LogMessage
parseMessage s = let (x:xs) = split ' ' s in
                     case x of
                         "I" -> LogMessage Info 
                                           (read (head xs))
                                           (intercalate " " (tail xs))
                         "W" -> LogMessage Warning
                                           (read (head xs))
                                           (intercalate " " (tail xs))
                         "E" -> LogMessage (Error
                                                (read (head xs)))
                                           (read (head (tail xs)))
                                           (intercalate " " (tail $ tail xs))
                         _   -> Unknown s

parse :: String -> [LogMessage]
parse s = map parseMessage (split '\n' s)

-- Exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert lm@(LogMessage _ t _) Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ t _) (Node left lm'@(LogMessage _ t' _) right)
                            | t < t' = Node (insert lm left) lm' right
                            | t > t' = Node left lm' (insert lm right)
                            | otherwise = Node left lm right

-- Exercise 3

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

-- Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left m right) = (inOrder left) ++ [m] ++ (inOrder right)

-- Exercise 5

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = badlogs' where
                        badlogs :: [LogMessage]
                        badlogs = filter (\(LogMessage k _ _) 
                                          -> case k of 
                                               Error sev -> sev >= 50 
                                               _ -> False
                                       )
                                       logs
                        badlogs' :: [String]
                        badlogs' = map
                                   (\(LogMessage _ _ s) -> s)
                                   (inOrder $ build badlogs)
                                                                
