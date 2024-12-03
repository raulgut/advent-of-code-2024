-----------------------------------------------------------------------------
-- | Day 1 - Part 2
-- 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List (sort)
import Data.MultiSet (MultiSet, fromList, occur)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let (list1, list2) = (rearrange . map (map read . words . filter (/= '\r')) . lines $ input) :: ([Int],[Int])
  putStrLn . show . sum $ map (getSimilarityScore (fromList list2)) (sort list1)

-- | Rearrange the lists
rearrange :: [[a]] -> ([a],[a])
rearrange [] = ([],[])
rearrange ([x,y]:zs) = let (xs, ys) = rearrange zs 
                       in (x:xs, y:ys)

-- | Compute teh similarity score
getSimilarityScore :: MultiSet Int -> Int -> Int
getSimilarityScore ms x = let times = occur x ms 
                          in x * times