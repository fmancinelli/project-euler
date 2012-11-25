module Main where

import Data.Char

-- Find the sum of digits in 100!

------------------------------------------------------------------------------

problem20 = sum $ map (\d -> (ord d) - (ord '0')) $ show $ product [1..100]

------------------------------------------------------------------------------

main = do
  print problem20
