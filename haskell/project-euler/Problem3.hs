module Main where

-- Problem 3: What is the largest prime factor of the number 600851475143 ?

------------------------------------------------------------------------------

number = 600851475143::Integer

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
primes = filter isPrime [1..]

------------------------------------------------------------------------------

-- findLargestPrimeFactor definition.

-- This function tries to find the largest prime factor of the number n by recursively trying to divide n by the numbers
-- passed in the list fs. At some point n will be reduced to 1 and m will contain its largest factor

-- Base case: n == 1, we're done.
findLargestPrimeFactor::Integer -> Integer -> [Integer] -> Integer
findLargestPrimeFactor 1 m fs = m 

-- Base case: we have consumed the list of factors. Return what we have found so far.
--            This should not happen because we pass infinite list, so this case is 
--            redundant wrt to our problem3 definition (see below)
findLargestPrimeFactor n m [] = m 

findLargestPrimeFactor n m (f:fs)
        -- If the current factor to test, head of the list, is 1 then skip.
        -- Every number is divisible by 1. This also avoid indefinite looping on 1 in the next case.
        | f == 1 = findLargestPrimeFactor n 1 fs 

        -- If the current number to test, head of the list, divides n, then we have found a factor.
        -- Call recursively findLargestPrimeFactor, on (n div f), setting f as
        -- the highest factor found, but keeping f in the list.
        -- This is done because a factor can be a power of a given number (e.g. 50 = 2 * 5^2)
        -- so we need to keep divinding by f 
	| (n `rem` f) == 0 = findLargestPrimeFactor (n `div` f) f (f:fs) 

        -- If the current number to test, head of the list, doesn't divide n, then we must move on.
        -- Call recursivelyfindLargestPrimeFactor, on the current number, keeping the current highest factor found, but
        -- discarding the head of the list containing the numbers to test.
        | (n `rem` f) /= 0 = findLargestPrimeFactor n m fs 

------------------------------------------------------------------------------

-- Compute problem 3 solution
problem3::Integer
problem3 = findLargestPrimeFactor number 1 primes

------------------------------------------------------------------------------

main = do
	print problem3
