module Main where

isMultipleOf3Or5 :: Integer -> Bool
isMultipleOf3Or5 x
	| x `rem` 3 == 0 	= True
	| x `rem` 5 == 0 	= True
	| otherwise 		= False

problem1 = foldr (+) 0 (filter isMultipleOf3Or5 [0..999])

main = do
	print problem1
