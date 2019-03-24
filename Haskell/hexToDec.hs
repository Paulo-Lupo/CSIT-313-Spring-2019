{- 
   Haskell Programming Assignment 1 - hexToDec.hs
   Joao Paulo Dos Santos Ferreira
   CSIT 313_01 - Fundamentals of Programing Language
   Dr. James Benham
   March 23, 2019
-}

{- 
   This module defines a hexdecimal to decimal converter
   by using string as an input and the unicode value of characters
-}

-- the Data.Char module is used here to obtain the unicode value of a character
import Data.Char

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
   this function is the same as mHex  
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
