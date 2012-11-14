module Main where

import Data.List

-- Finding Fibonacci numbers for which the first and last nine digits are pandigital.

------------------------------------------------------------------------------

fibs = 1 : scanl (+) 1 fibs

------------------------------------------------------------------------------

isPanDigital x = length (nub $ filter (/= '0') $ x) == 9

------------------------------------------------------------------------------

checkNumber x = (isPanDigital $ take 9 xs) && (isPanDigital $ drop (length xs - 9) xs)
	where xs = show x

findNumber n (fib:fibs)
	| checkNumber fib = n
	| otherwise = findNumber (n+1) fibs

------------------------------------------------------------------------------

main = do
	print findNumber 1 fibs
