module Main where

-- What is the largest prime factor of the number 600851475143 ?

number = 600851475143::Integer

-- A function that returns True if its parameter is a prime number
isPrime x 
	| x <= 3 = True
	| x > 3  = not $ any (`divides` x) $ factorsToCheck
	where		
		factorsToCheck = [2..ceiling $ sqrt $ fromIntegral x]
		divides x y = y `rem` x == 0

-- The list of prime numbers
primes = filter isPrime [1..]

-- Reduce tries to find the largest prime factor of the number n by recursively trying to divide n by the numbers
-- passed in the list fs. At some point n will be reduced to 1 and m will contain its largest factor
reduce 1 m fs = m -- Base case: n == 1, we're done.
reduce n m [] = m -- Base case: we have consumed the list of factors. Return what we have found so far.
                  --            This should not happen because we pass infinite list, so this case is 
                  --            redundant wrt to our problem3 definition (see below)
reduce n m (f:fs)
        | f == 1 = reduce n 1 fs -- If the current factor to test, head of the list, is 1 then skip.
                                 -- Every number is divisible by 1.                       
	| (n `rem` f) == 0 = reduce (n `div` f) f (f:fs) -- If the current number to test, head of the list, divides n, then we have found a factor.
                                                         -- Call recursively reduce, on (n div f), setting f as
                                                         -- the highest factor found, but keeping f in the list.
                                                         -- This is done because a factor can be a power of a given number (e.g. 50 = 2 * 5^2)
                                                         -- so we need to keep divinding by f 
        | (n `rem` f) /= 0 = reduce n m fs -- If the current number to test, head of the list, doesn't divide n, then we must move on.
                                           -- Call recursively reduce, on the current number, keeping the current highest factor found, but
                                           -- discarding the head of the list containing the numbers to test.

problem3 = reduce number 1 primes

main = do
	print (reduce number 1 primes)
