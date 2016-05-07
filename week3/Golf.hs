module Golf where

import Data.List

-- As requested, this code attempts to be as terse as possible, at the expense of readability
-- Also as requested, full explanations are provided for the "submitted" code.

{- ex1: Hopscotch -}
-- Add some type synonyms for legibility, as the two main ints being used were getting confusing
-- ... well, at least when this code was much (much) longer!
type Divisor   = Int
type ListIndex = Int

-- Helper function for 'skips'
-- This function starts by "zipping" a list of [1..n] (where n is the length of the input list) to
-- the input list. This is done in order to provide a list index per element in the list.
--
-- Once the two lists are zipped, a new list is returned which contains a list of tuples - each tuple
-- contains two elements, in the following format [(x,y),...] where 'x' is (list index + 1) and 'y' is
-- the element in the input list.
--
-- Next, a lambda function is used: we pattern match out the Divisor and the index of the list element.
-- We perform (li `mod` d), where li is the List Index (li), and d is the divisor. The lambda function
-- returns a boolean True for any element for which the modulo operation produces a '0'.
--
-- The filter function from GHC.List applies a function to every element in a list, and returns list
-- elements for which the function returns boolean True.
--
-- Unzip is then used to convert the list of tuples in the format [(2,"balloon"),(4,"horse")], to a
-- tuple of lists, in the format ([2,4],["balloon","horse"]).
--
-- Finally, 'snd', from Data.Tuple 'returns' the second of two elements inside the tuple - exactly
-- what we need. From the previous example - ["balloon","horse"].
getNths :: Divisor -> [a] -> [a]
getNths d xs = snd $ unzip $ filter ((\d (li,_) -> li `mod` d == 0) d) (zip [1..(length xs)] xs)

--  Skip simply maps "getNths inputList" (with flip, to revese the order of expected arguments) over
--  1..x, where 'x' is the length of the input list. I deliberately started at '1' rather than at the
--  more traditional '0', as treating the first element of a list as '1' is important for the logic of
--  this program.
--
-- The "algorithm" works by using the modulo operator to filter for list values (by list index+1) which
-- are cleanly divisible by 'x', where 'x' is what we want to find (x = 1) == every 1th element,
-- (x = 2) == every 2nd element, (x = 5) == every 5th element etc).
skips :: [a] -> [[a]]
skips inputList = map (flip getNths inputList) [1..(length inputList)]

{- ex2: Local maxima -}
-- This simple recursive function pattern-matches out the interesting elements of the input list,
-- then returns either:
-- a) a local maxima, 'consed' onto a call to localMaxima, which operates on the tail of the list
-- b) a call to localMaxima, which operates on the tail of the list
-- c) an empty list, in the event of an empty, one or two element input list,
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima (x:y:z:as)
  | x < y && y > z = y : localMaxima (y:z:as)
  | otherwise      = localMaxima (y:z:as)
localMaxima (x:xs) = []

{- ex3: Histogram -}
histogram :: [Int] -> String
histogram il = intercalate "" (processList il ++ ["==========\n0123456789\n"])

-- Let's get smarter and start doing much of the heavy lifting in this helper function
-- This function fills gaps in the list (artificially raising the counts of each individual digit
-- by one), then we map (-1 +) over the list to correct the inaccuracy. Finally, we pass the
-- generated list to buildHistogram
processList :: [Int] -> [String]
processList il = buildHistogram $ map (-1 +) (map length $ noGaps il)

-- Helper function which concatenates [0..9] onto input list to ensure that we don't have
-- any missing elements. This is important as we expect the zeroeth list element to contain
-- the number of zeros, and the fifth list element to contain the number of fives, etc.
noGaps :: [Int] -> [[Int]]
noGaps = \x -> group $ sort $ [0..9] ++ x

-- This will take theinput from processList
buildHistogram :: [Int] -> [String]
buildHistogram il
  | maximum il <= 0 = []
  | otherwise       = (buildHistogram (map (-1 +) il)) ++ ((map starOrSpace il) ++ ["\n"])

-- Use some simple boolean logic, which can be combined with the power of "map"
-- in order to change our positive-ints to "*" and zero or less to " "
starOrSpace :: Int -> String
starOrSpace i
  | i <= 0 = " "
  | i > 0  = "*"

