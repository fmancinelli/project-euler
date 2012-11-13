module Main where

-- Add all the natural numbers below one thousand that are multiples of 3 or 5.

------------------------------------------------------------------------------

isMultipleOf3Or5 :: Integer -> Bool
isMultipleOf3Or5 x
	| x `rem` 3 == 0 	= True
	| x `rem` 5 == 0 	= True
	| otherwise 		= False

------------------------------------------------------------------------------

problem1 = foldr (+) 0 (filter isMultipleOf3Or5 [0..999])

------------------------------------------------------------------------------

main = do
	print problem1
