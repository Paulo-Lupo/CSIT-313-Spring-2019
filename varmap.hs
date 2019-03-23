{- 
   Haskell Programming Assignment 1 - varmap.hs
   Joao Paulo Dos Santos Ferreira
   CSIT 313_01 - Fundamentals of Programing Language
   Dr. James Benham
   March 30, 2019
-}

-- This module defines varmap, returns -1 if the value is undefined
varmap::String -> [(String, Int)] -> Int
-- base case (value not found)
varmap a [] = -1


varmap a (pair:pairs) = 
  -- checks the head of the list
  if (a == fst pair) 
    -- if the value is found, returns the varmap of the variable
    then snd pair
    -- if the value is not found, checks the rest of the list
    else varmap a pairs
