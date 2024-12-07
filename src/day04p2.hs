-----------------------------------------------------------------------------
-- | Day 4 - Part 2
-- 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.Matrix (Matrix, fromLists, ncols, nrows, getElem)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let matrix = (fromLists . map (filter (/= '\r')) . lines $ input) :: (Matrix Char)
  let positions = [(i,j) | i <- [1..nrows matrix], j <- [1..ncols matrix], getElem i j matrix == 'A']
  putStrLn . show . length . filter (==True) . map (getXMAS matrix) $ positions

-- | Check if there is an X-MAS
getXMAS :: Matrix Char -> (Int,Int) -> Bool
getXMAS matrix (i,j)
  = (i - 1 >= 1) && (i + 1 <= nrows matrix)
    && (j - 1 >= 1) && (j + 1 <= ncols matrix)
    && (((getElem (i - 1) (j - 1) matrix == 'M') && (getElem (i + 1) (j + 1) matrix == 'S')) 
        || ((getElem (i - 1) (j - 1) matrix == 'S') && (getElem (i + 1) (j + 1) matrix == 'M')))
    && (((getElem (i - 1) (j + 1) matrix == 'M') && (getElem (i + 1) (j - 1) matrix == 'S')) 
        || ((getElem (i - 1) (j + 1) matrix == 'S') && (getElem (i + 1) (j - 1) matrix == 'M')))
