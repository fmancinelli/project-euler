import Data.List

-- What is the smallest number divisible by each of the numbers 1 to 20?

------------------------------------------------------------------------------

-- Returns true if x is divisible by [1..n]
checkFactors x n
	| n == 1 = True
	| (x `rem` n) == 0 = checkFactors x (n - 1)
	| otherwise = False

------------------------------------------------------------------------------

problem5 = find (flip checkFactors 20) [1..]

------------------------------------------------------------------------------

main = do
	print problem5
