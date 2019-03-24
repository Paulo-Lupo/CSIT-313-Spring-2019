{- 
   Haskell Programming Assignment 1 - mergeSort.hs
   Joao Paulo Dos Santos Ferreira
   CSIT 313_01 - Fundamentals of Programming Language
   Dr. James Benham
   March 23, 2019
-}

{- 
   This module defines a function mergeSort that 
   sorts a list by using the merge sort algorithm
-}

-- takes a list and returns a sorted list
mergeSort::(Ord a) => [a] -> [a]
-- base cases
mergeSort [] = []
mergeSort [a] = [a]

mergeSort ws = 
  -- implements merge using let
  let merge xs ys
        -- first 3 are base cases
        | xs == [] && ys == [] = []
        | xs == [] && ys /= [] = ys
        | xs /= [] && ys == [] = xs
        -- compares the first elements of the sub-lists, merges them recursively
        | (head xs < head ys) = (head xs):(merge (tail xs) (ys))
        | otherwise = (head ys):(merge (tail ys) (xs))
  in
    -- splits the list into two approximately equal sub-lists
    let
      halfLength = length ws `div` 2
      firstHalf = take halfLength ws
      secondHalf = drop halfLength ws
    in
    -- calls the "merge" to merge the smaller sub-lists
    merge (mergeSort firstHalf) (mergeSort secondHalf)
