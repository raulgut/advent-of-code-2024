-----------------------------------------------------------------------------
-- | Day 2 - Part 1
-- 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List (sort, partition)

-- | We are considering all increasing or all decreasing reports 
data Level = Increasing
           | Decreasing
           deriving (Show)

-- | Result of the report
data ReportState = Safe
                 | Unsafe Int
                 deriving (Show,Eq)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map (map read . words . filter (/= '\r')) . lines $ input) :: [[Int]]
  let (safe1, unsafe1) = partition (\x -> snd x==Safe) . zip entries . map (process Nothing 0) $ entries  
  let unsafeEntries1 = map (\(xs,Unsafe p) -> let (ys,zs) = splitAt p xs in ys ++ tail zs)  $ unsafe1
  let (safe2, unsafe2) = partition (\x -> snd x==Safe) . zip unsafe1 . map (process Nothing 0) $ unsafeEntries1
  let unsafeEntries2 = map (\((xs,Unsafe p),_) -> let (ys,zs) = splitAt (p + 1) xs in ys ++ tail zs)  $ unsafe2  
  let (safe3, unsafe3) = partition (\x -> snd x==Safe) . zip (map fst unsafe2) . map (process Nothing 0) $ unsafeEntries2
  let unsafeEntries3 = map (\((xs,Unsafe p),_) -> let (ys,zs) = splitAt (p - 1) xs in ys ++ tail zs)  $ unsafe3  
  let (safe4, unsafe4) = partition (\x -> snd x==Safe) . zip (map fst unsafe3) . map (process Nothing 0) $ unsafeEntries3
  putStrLn . show $ length safe1 + length safe2 + length safe3 + length safe4

-- | process the reports, Unsafe returns problematic position
process :: Maybe Level -> Int -> [Int] -> ReportState
process _ _ [] = Safe
process _ _ [x] = Safe
process Nothing pos (x:y:xs) 
  = if (x > y) && (x - y < 4) then
      process (Just Decreasing) (pos + 1) (y:xs)
    else if (y > x) && (y - x < 4) then
      process (Just Increasing) (pos + 1) (y:xs)
    else
      Unsafe pos
process (Just Decreasing) pos (x:y:xs) 
  = if (x > y) && (x - y < 4) then
      process (Just Decreasing) (pos + 1) (y:xs)
    else 
      Unsafe pos
process (Just Increasing) pos (x:y:xs) 
  = if (y > x) && (y - x < 4) then
      process (Just Increasing) (pos + 1) (y:xs)
    else
      Unsafe pos
