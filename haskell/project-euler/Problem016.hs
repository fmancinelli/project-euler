module Main where

import Data.Char

-- What is the sum of the digits of the number 21000?

------------------------------------------------------------------------------

problem16 = sum $ map (\d -> (ord d) - (ord '0')) $ show $ 2^1000

------------------------------------------------------------------------------

main = do
  print problem16
