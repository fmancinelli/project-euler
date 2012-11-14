module Main where

-- Calculate the sum of all the primes below two million.

------------------------------------------------------------------------------

isPrime::Integer -> Bool
isPrime x 
	| x <= 3 = True
	| x > 3  = not $ any (`divides` x) $ factorsToCheck
	where		
		factorsToCheck = [2..ceiling $ sqrt $ fromIntegral x]
		divides x y = y `rem` x == 0

------------------------------------------------------------------------------

primes = filter isPrime [2..2000000]

------------------------------------------------------------------------------

main = do
	print $ foldl (+) 0 primes
