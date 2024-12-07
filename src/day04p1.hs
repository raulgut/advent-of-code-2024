-----------------------------------------------------------------------------
-- | Day 4 - Part 1
-- 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.Matrix (Matrix, fromLists, ncols, nrows, getElem)

-- | Possible directions
data Direction = NORTH
               | SOUTH
               | EAST
               | WEST
               | NORTHWEST
               | NORTHEAST
               | SOUTHWEST
               | SOUTHEAST

main = do
  args <- getArgs
  input <- readFile . head $ args
  let matrix = (fromLists . map (filter (/= '\r')) . lines $ input) :: (Matrix Char)
  let positions = [(i,j) | i <- [1..nrows matrix], j <- [1..ncols matrix], getElem i j matrix == 'X']
  putStrLn . show . foldl (+) 0 . map (getXMAS matrix) $ positions

-- | Check how many XMAS are in a position
getXMAS :: Matrix Char -> (Int,Int) -> Int
getXMAS matrix position
  = length . filter (== True) $ [getXMASDirection NORTH matrix position 
    ,getXMASDirection SOUTH matrix position
    ,getXMASDirection EAST matrix position
    ,getXMASDirection WEST matrix position
    ,getXMASDirection NORTHWEST matrix position
    ,getXMASDirection NORTHEAST matrix position 
    ,getXMASDirection SOUTHWEST matrix position
    ,getXMASDirection SOUTHEAST matrix position]

-- | Check if there is a MAS
getXMASDirection :: Direction -> Matrix Char -> (Int,Int) -> Bool
getXMASDirection NORTH matrix (i,j)
  = (i - 3 >= 1) 
    && (getElem (i - 1) j matrix == 'M')
    && (getElem (i - 2) j matrix == 'A')
    && (getElem (i - 3) j matrix == 'S')
getXMASDirection SOUTH matrix (i,j)
  = (i + 3 <= nrows matrix) 
    && (getElem (i + 1) j matrix == 'M')
    && (getElem (i + 2) j matrix == 'A')
    && (getElem (i + 3) j matrix == 'S')
getXMASDirection WEST matrix (i,j)
  = (j - 3 >= 1) 
    && (getElem i (j - 1) matrix == 'M')
    && (getElem i (j - 2) matrix == 'A')
    && (getElem i (j - 3) matrix == 'S')
getXMASDirection EAST matrix (i,j)
  = (j + 3 <= ncols matrix) 
    && (getElem i (j + 1) matrix == 'M')
    && (getElem i (j + 2) matrix == 'A')
    && (getElem i (j + 3) matrix == 'S')
getXMASDirection NORTHWEST matrix (i,j)
  = (i - 3 >= 1) && (j - 3 >= 1) 
    && (getElem (i - 1) (j - 1) matrix == 'M')
    && (getElem (i - 2) (j - 2) matrix == 'A')
    && (getElem (i - 3) (j - 3) matrix == 'S')
getXMASDirection NORTHEAST matrix (i,j)
  = (i - 3 >= 1) && (j + 3 <= ncols matrix) 
    && (getElem (i - 1) (j + 1) matrix == 'M')
    && (getElem (i - 2) (j + 2) matrix == 'A')
    && (getElem (i - 3) (j + 3) matrix == 'S')
getXMASDirection SOUTHWEST matrix (i,j)
  = (i + 3 <= nrows matrix) && (j - 3 >= 1) 
    && (getElem (i + 1) (j - 1) matrix == 'M')
    && (getElem (i + 2) (j - 2) matrix == 'A')
    && (getElem (i + 3) (j - 3) matrix == 'S')
getXMASDirection SOUTHEAST matrix (i,j)
  = (i + 3 <= nrows matrix) && (j + 3 <= ncols matrix) 
    && (getElem (i + 1) (j + 1) matrix == 'M')
    && (getElem (i + 2) (j + 2) matrix == 'A')
    && (getElem (i + 3) (j + 3) matrix == 'S')
