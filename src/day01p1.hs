-----------------------------------------------------------------------------
-- | Day 1 - Part 1
-- 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List (sort)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let (list1, list2) = (rearrange . map (map read . words . filter (/= '\r')) . lines $ input) :: ([Int],[Int])
  --let entries = (map sum . map (map read) . filter (/= [""]) . groupBy (\x y -> (not . null $ x) && (not . null $ y)) . map (filter (/= '\r')) . lines $ input) :: [Int]
  putStrLn . show . sum $ zipWith (\x y -> abs(x - y)) (sort list1) (sort list2)

-- | Rearrange the lists
rearrange :: [[a]] -> ([a],[a])
rearrange [] = ([],[])
rearrange ([x,y]:zs) = let (xs, ys) = rearrange zs 
                       in (x:xs, y:ys)