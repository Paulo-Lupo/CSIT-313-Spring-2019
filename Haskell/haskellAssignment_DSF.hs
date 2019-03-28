{- 
   Haskell Programming Assignment 1
   Joao Paulo Dos Santos Ferreira
   CSIT 313_01 - Fundamentals of Programming Language
   Dr. James Benham
   March 28, 2019
-}

{-
   The Data.Char module is used here to obtain the unicode value of a character
   which will be used in the second set of functions for problem 2 of this assignment
-}
import Data.Char

--------------------------------------------------------------------------------------

{- 
   mergeSort function (Problem 1)
   This function sorts a list by using the merge sort algorithm
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

--------------------------------------------------------------------------------------

{- 
   mHexDigit and mHex functions (Problem 2)
   These functions defines a hexdecimal to decimal converter
   by using string as an input and the unicode value of characters
-}

-- returns the decimal value of a single hexadecimal character
mHexDigit::Char -> Int 
mHexDigit ch 
  | ord ch == 48 = 0
  | ord ch == 49 = 1
  | ord ch == 50 = 2
  | ord ch == 51 = 3
  | ord ch == 52 = 4
  | ord ch == 53 = 5
  | ord ch == 54 = 6
  | ord ch == 55 = 7
  | ord ch == 56 = 8
  | ord ch == 57 = 9
  | ord ch == 65 || ord ch == 97 = 10
  | ord ch == 66 || ord ch == 98 = 11
  | ord ch == 67 || ord ch == 99 = 12
  | ord ch == 68 || ord ch == 100 = 13
  | ord ch == 69 || ord ch == 101 = 14
  | ord ch == 70 || ord ch == 102 = 15
  | otherwise = error "You entered a digit that is not hexdecimal."

-- takes a string in hexadecimal and gives its decimal representation
mHex::String -> Int
-- base case
mHex "" = 0

{- 
   recursively calls mHex on the list except the last element
   multiplying it by 16 every time. calls mHexDigit of last element
-}
mHex sl = (mHex (init sl))*16 + mHexDigit (last sl)


{- 
   nHex' function (Problem 2)
   This function is the same as mHex  
   but mHexDigit is defined inside this function
-}
mHex'::String -> Int
mHex' "" = 0
mHex' st = 
  let mHexDigit' ch
        | ord ch == 48 = 0
        | ord ch == 49 = 1
        | ord ch == 50 = 2
        | ord ch == 51 = 3
        | ord ch == 52 = 4
        | ord ch == 53 = 5
        | ord ch == 54 = 6
        | ord ch == 55 = 7
        | ord ch == 56 = 8
        | ord ch == 57 = 9
        | ord ch == 65 || ord ch == 97 = 10
        | ord ch == 66 || ord ch == 98 = 11
        | ord ch == 67 || ord ch == 99 = 12
        | ord ch == 68 || ord ch == 100 = 13
        | ord ch == 69 || ord ch == 101 = 14
        | ord ch == 70 || ord ch == 102 = 15
        | otherwise = error "You entered a number that is not hexdecimal."
  in (mHex' (init st))*16 + mHexDigit' (last st)

--------------------------------------------------------------------------------------

{- 
   varmap function (Problem 3)
   This function implements VARMAP function for program states.
   The values are assumed to be non-negative integers,
   the function returns -1 if the value is undefined
-}

varmap::String -> [(String, Int)] -> Int
-- base case (value not found)
varmap x [] = -1

varmap x (pair:pairs) = 
  -- checks the head of the list
  if (x == fst pair) 
    -- if the value is found, returns the varmap of the variable
    then snd pair
    -- if the value is not found, checks the rest of the list
    else varmap x pairs

--------------------------------------------------------------------------------------




