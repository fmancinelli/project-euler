module Main where

-- By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.

------------------------------------------------------------------------------

-- Recursive definition of an infinite list containing the fibonacci sequence. Here how it works:
-- Step0) 0
-- Step1) 0 : scanl (+) 1 [0] = 0 : [1, 1] = [0, 1, 1]
-- Step2) 0 : scanl (+) 1 [0, 1, 1] = 0 : [1, 1, 2, 3] = [0, 1, 1, 2, 3]
-- Step3) 0 : scanl (+) 1 [0, 1, 1, 2, 3] = 0 : [1, 1, 2, 3, 5, 8] = [0, 1, 1, 2, 3, 5, 8]
-- ...
fibonacci  = 0 : scanl (+) 1 fibonacci

------------------------------------------------------------------------------

-- We use $ in order to remove some parenthesis. $ is equivalent to function application
-- but it's right associative. Without $, we should have written the following expression in this way:
-- problem2 = foldr (+) 0 (filter (\x -> x `rem` 2 == 0) (takeWhile (< 4000000) fibonacci))
problem2  = foldr (+) 0 $ filter (\x -> x `rem` 2 == 0) $ takeWhile (< 4000000) fibonacci

------------------------------------------------------------------------------

main = do
     print problem2
          
