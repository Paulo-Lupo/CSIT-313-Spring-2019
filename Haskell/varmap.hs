{- 
   Haskell Programming Assignment 1 - varmap.hs
   Joao Paulo Dos Santos Ferreira
   CSIT 313_01 - Fundamentals of Programing Language
   Dr. James Benham
   March 23, 2019
-}

{- 
   This module defines the varmap function
   the values are assumed to be non-negative integers
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
