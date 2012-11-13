module Main where

import Data.List(find, sort)

-- Find the largest palindrome made from the product of two 3-digit numbers.

------------------------------------------------------------------------------

isPalindrome x = y == reverse y
	where y = show x

------------------------------------------------------------------------------

numbers = [x * y | x <- [100..999], y <- [100..999]]

------------------------------------------------------------------------------

problem4 = find isPalindrome $ reverse $ sort numbers

------------------------------------------------------------------------------

main = do
	print problem4
