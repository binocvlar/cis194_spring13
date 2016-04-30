{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

{- ex1 (i) -}
{- Define a function which parses an individual message -}
parseMessage :: String -> LogMessage
parseMessage s = parseMessageList $ words s

-- FIXME: Note that 'x' in the below pattern matches MUST be an int. This function is 'incomplete' as a result.
--        I'm not supposed to change the return type, but if I could change it to "Maybe LogMessage", I'd be
--        able to express failure... I think.
parseMessageList :: [String] -> LogMessage
parseMessageList li = case li of
                       []           -> Unknown ""
                       ("E":x:y:zs) -> LogMessage (Error (read x)) (read y) (unwords zs)
                       ("W":x:ys)   -> LogMessage (Warning) (read x) (unwords ys)
                       ("I":x:ys)   -> LogMessage (Info) (read x) (unwords ys)
                       (x)          -> Unknown (unwords x)
                    -- a@(x)        -> Unknown (unwords a) -- works, but unnecessary

{- ex1 (ii) -}
{- Define a function which parses an entire log file at once -}
parse :: String -> [LogMessage]
parse s = parseList $ lines s

parseList :: [String] -> [LogMessage]
parseList l = map parseMessage l

{- ex2 (i) -}
{- Define a function which inserts a new LogMessage into
   an existing MessageTree, producing a new MessageTree -}
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree                  = tree
insert msgToInsert Leaf                  = Node Leaf msgToInsert Leaf
insert msgToInsert tree@(Node left nodeMsg right)
  | tsOfMsgToInsert < nodeTimeStamp = Node (insert msgToInsert left) nodeMsg right
  | tsOfMsgToInsert > nodeTimeStamp = Node left nodeMsg (insert msgToInsert right)
  | otherwise                       = tree
    where
      tsOfMsgToInsert = fetchTimeStamp msgToInsert
      nodeTimeStamp   = fetchTimeStamp nodeMsg

-- Helper function to pattern match the timestamp out of a LogMessage
fetchTimeStamp :: LogMessage -> Int
fetchTimeStamp (Unknown _)                 = (-1)
fetchTimeStamp (LogMessage (Error _) ts _) = ts
fetchTimeStamp (LogMessage _ ts _)         = ts

{- ex3 (i) -}
{- Define a function which builds up a message tree containing the messages in the list by successively 
   inserting the messages into a MessageTree (beginning with a leaf) -}
build :: [LogMessage] -> MessageTree
build logMessages = foldl (flip insert) Leaf logMessages

{- ex4 (i) -}
{- Define a function which takes a sorted message tree and produces a list of all the LogMessages it contains,
   sorted by timestamp from smallest to largest. This is called an 'in-order traversal' of the MessageTree -}
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                      = []
inOrder (Node left nodeMsg right) = inOrder left ++ (nodeMsg : inOrder right)

{- ex5 (i) -}
{- Write a function which takes an unsorted list of LogMessages, and returns a list of the
   messages corresponding to any errors with a severity of 50 or greater, sorted by timestamp -}
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMessages = map extractErrorText $ filter errorsOverFifty logMessages

-- Helper function for whatWentWrong
extractErrorText :: LogMessage -> String
extractErrorText (LogMessage (Error _) _ errorText) = errorText
extractErrorText _                                  = ""

-- Helper function for whatWentWrong
errorsOverFifty :: LogMessage -> Bool
errorsOverFifty (LogMessage (Error sev) _ _)
  | sev >= 40     = True
  | otherwise     = False
errorsOverFifty _ = False

-- Sample usage of mapM + a lambda func to run an IO action over every element of a list...
-- *LogAnalysis> foobar <- (testWhatWentWrong parse whatWentWrong "error.log")
-- *LogAnalysis> mapM (\x -> putStrLn x) foobar

