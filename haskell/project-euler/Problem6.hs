module Main where

------------------------------------------------------------------------------

n = 100

------------------------------------------------------------------------------

problem6 = (sum [1..n])^2 - (sum $ map (^2) [1..n])
-- Or slightly optimized version:
-- problem6 (n * (n + 1) / 2) - (sum $ map (^2) [1..n])

------------------------------------------------------------------------------

main = do
	print problem6
