{- 
   Haskell Programming Assignment 1 - otherMergeSort.hs
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

-- splits the list into two approximately equal sub-lists
mergeSort xs = 
  let 
    halfLength = length xs `div` 2
    firstHalf = take halfLength xs
    secondHalf = drop halfLength xs
  in
    -- calls the "merge" function to merge the smaller sub-lists
    merge (mergeSort firstHalf) (mergeSort secondHalf)


-- "merge" function to merge two sub-lists into a sorted sub-list
merge::(Ord a) => [a] -> [a] -> [a]
-- base cases 
merge [] [] = []
merge xs [] = xs 
merge [] ys = ys

-- merges the sub-lists while sorting them
merge (x:xs) (y:ys) =
  if x < y 
    then x:merge xs (y:ys) 
    else y:merge (x:xs) ys
