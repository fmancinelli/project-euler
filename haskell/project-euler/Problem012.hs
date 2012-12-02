module Main where

import Data.List

-- What is the value of the first triangle number to have over five hundred divisors?

------------------------------------------------------------------------------

-- Generate an infinite sequence of triangle numbers.
triangleNumbers = triangleNumbers' 1 0
triangleNumbers' n s = (n + s):triangleNumbers' (n + 1)(n + s)

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

{- 
Find the prime divisors of a number. If the number is divisible for a power
of a prime factor p, then p is repeated n times. (e.g., divisors 8 = [2,2,2])
-}
primeDivisors x = primeDivisors' x primes []
  where primeDivisors' x (p:ps) ds
          | p == 1 = primeDivisors' x ps ds -- Just skip 1, otherwise infinite looping!
          | x == 1 = ds
          | (x `rem` p) == 0 = primeDivisors' (x `div` p) (p:ps) (p:ds)
          | (x `rem` p) /= 0 =  primeDivisors' x ps ds

------------------------------------------------------------------------------

-- Return the powerset of a set
powerset [] = [[]]
powerset (x:xs) = map (x:) (powerset xs) ++ powerset xs

------------------------------------------------------------------------------

{- 
This does the trick. In order to find the number of divisors of a number
we first find its prime factorization. Then we take the powerset of the
result in order to obtain all possible way of multiplying its prime factors
to obtain all the divisors. We then take the products of these combinations
and we remove the duplicates. The cardinality of the resulting list is
the number of the divisors for x.

Example: x = 28
         primeFactors = [7, 2, 2]
         powerset [7, 2, 2] = [[7,2,2],[7,2],[7,2],[7],[2,2],[2],[2],[]]
         we then remove the duplicates and we get = [[7,2,2],[7,2],[7],[2,2],[2],[]]
         which corresponds to the 6 divisors [28, 14, 7, 4 , 2, 1]
-}
divisors x = nub $ powerset $ primeDivisors x

------------------------------------------------------------------------------

problem12 = find (\n -> (length $ divisors n) > 500) triangleNumbers

------------------------------------------------------------------------------

main = do
  print problem12
  
{-
Final note: I used this because using the "simple" solution for getting the divisors:
divisors x = filter (\n -> (x `rem` n) == 0) [1..x]
would have taken forever to get to compute the result.

The optimization in this file computes the results almost immediately.
-}
