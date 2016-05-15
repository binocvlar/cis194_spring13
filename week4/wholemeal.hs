
{- ex1: Wholemeal Programming -}

-- Replace the following function with a more idiomatic version.
-- Use wholemeal programming practices, breaking each function into
-- a pipeline of incremental transformations to an entire data structure.

-- Supplied function, to be replaced
fun1 :: [Integer] -> Integer
fun1 []         = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

-- More idiomatic version
fun1' :: [Integer] -> Integer
fun1' = foldr (*) 1 . map (-2 +) . filter (\x -> x `mod` 2 == 0)


-- Supplied function, to be replaced
fun2 :: Integer -> Integer
fun2 1          = 0
fun2 n
    | even n    = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

-- More idiomatic version
fun2' :: Integer -> Integer
fun2' = sum . filter (\x -> x `mod` 2 == 0) . takeWhile (>1) . iterate (\x -> if x `mod` 2 == 0 then x `div` 2 else x * 3 + 1)

-- Not what the exercise explicitly called for, but this is a bit nicer to read.
fun2'' :: Integer -> Integer
fun2'' = sum . filter (\x -> x `mod` 2 == 0) . takeWhile (>1) . iterate halveOrTriple

halveOrTriple :: Integer -> Integer
halveOrTriple = (\x -> if x `mod` 2 == 0 then x `div` 2 else x * 3 + 1)
