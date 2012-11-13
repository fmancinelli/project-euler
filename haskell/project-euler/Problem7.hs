module Main where

-- Find the 10001st prime.

------------------------------------------------------------------------------

-- A function that returns True if its parameter is a prime number
isPrime::Integer -> Bool
isPrime x 
	| x <= 3 = True
	| x > 3  = not $ any (`divides` x) $ factorsToCheck
	where		
		factorsToCheck = [2..ceiling $ sqrt $ fromIntegral x]
		divides x y = y `rem` x == 0

------------------------------------------------------------------------------

-- The list of prime numbers
primes = filter isPrime [2..]

------------------------------------------------------------------------------

problem7 = last $ take 10001 primes

------------------------------------------------------------------------------

main = do
	print problem7
