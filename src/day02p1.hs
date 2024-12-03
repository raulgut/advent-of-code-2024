-----------------------------------------------------------------------------
-- | Day 2 - Part 1
-- 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List (sort)

-- | We are considering all increasing or all decreasing reports 
data Level = Increasing
           | Decreasing
           deriving (Show)

-- | Result of the report
data ReportState = Safe
                 | Unsafe
                 deriving (Show,Eq)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map (map read . words . filter (/= '\r')) . lines $ input) :: [[Int]]
  putStrLn . show . length . filter (==Safe) . map (process Nothing) $ entries

-- | process the reports
process :: Maybe Level -> [Int] -> ReportState
process _ [] = Safe
process _ [x] = Safe
process Nothing (x:y:xs) 
  = if (x > y) && (x - y < 4) then
      process (Just Decreasing) (y:xs)
    else if (y > x) && (y - x < 4) then
      process (Just Increasing) (y:xs)
    else
      Unsafe
process (Just Decreasing) (x:y:xs) 
  = if (x > y) && (x - y < 4) then
      process (Just Decreasing) (y:xs)
    else 
      Unsafe
process (Just Increasing) (x:y:xs) 
  = if (y > x) && (y - x < 4) then
      process (Just Increasing) (y:xs)
    else
      Unsafe
