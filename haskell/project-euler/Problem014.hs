module Main where

-- Find the longest sequence using a starting number under one million.

------------------------------------------------------------------------------

sq n = sq' n []
sq' n s
  | n == 1 = (1 : s)
  | even n = sq' (n `div` 2) (n : s)
  | otherwise = sq' (3 * n + 1) (n : s)
  
------------------------------------------------------------------------------

problem14 = foldl (\acc@(x,y) elem@(x',y') -> if y > y' then acc else elem) (0, 0) $ map (\x -> (x, length $ sq x)) [1..999999]

------------------------------------------------------------------------------

main = do
  print $ fst problem14
