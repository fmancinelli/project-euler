module Main where

-- Find the only Pythagorean triplet, {a, b, c}, for which a + b + c = 1000.

------------------------------------------------------------------------------

pitagoreanTriplets = [(a, b, c) | c <- [1..1000], b <- [1..(1000 - c)], a <- [1..(1000 - (b + c))], (a ^ 2 + b ^ 2) == c ^ 2, (a + b + c) == 1000, a < b, b < c]

------------------------------------------------------------------------------

tripletProduct (a, b, c) = a * b * c

------------------------------------------------------------------------------

main = do
	print $ tripletProduct $ head $ take 1 pitagoreanTriplets

