-----------------------------------------------------------------------------
-- | Day 3 - Part 2
-- 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (Parser, many, try)
import Text.Parsec.Error(ParseError (..))
import Text.Parsec.Char (digit, string, char, anyChar)
import Text.Parsec.Combinator (many1, manyTill)
import Text.Parsec.Prim (parse, (<|>))
import Data.List (isPrefixOf)
{-- Debug
import Text.Parsec (ParsecT, getParserState, stateInput)
import Data.Functor.Identity (Identity)
import Debug.Trace (trace)
--}

data Instruction = Mul Int Int
                 | Do 
                 | Don't
                 deriving Show
main = do
  args <- getArgs
  input <- readFile . head $ args
  let multiplications = (extractMultiplications . removeDonts $ input) :: [Instruction] 
  putStrLn . show . foldl (+) 0 . map (\(Mul x y) -> x * y) $ multiplications 

-- | Keep the string until find a dont, then remove untill find a do
removeDonts [] = []
removeDonts (c:str) 
  = if isPrefixOf "don't()" (c:str) then
      dropTillDo (drop 6 str)
    else
      (c:removeDonts str)

-- | Drop elements from the string until find a do, then call removeDonts
dropTillDo [] = []
dropTillDo (c:str) 
  = if isPrefixOf "do()" (c:str) then
      removeDonts (drop 3 str)
    else
      dropTillDo str

-- | Parses multiplications
extractMultiplications :: String -> [Instruction]
extractMultiplications str
  = case (parse multiplicationsParser "" str) of
      Left err -> error . show $ err
      Right muls -> muls

-- | parses multiplications
multiplicationsParser :: Parser [Instruction]
multiplicationsParser = many (try multiplicationParser)

-- | multiplication
multiplicationParser :: Parser Instruction
multiplicationParser 
  = do manyTill anyChar (try (string "mul("))
       --seeNext 10
       try (multiplicationParser1) <|> (do return $ Mul 0 0)

multiplicationParser1 :: Parser Instruction
multiplicationParser1 
  = do value1 <- (many1 digit)
       char ','
       value2 <- (many1 digit)
       char ')'
       return $ Mul (read value1) (read value2)

-------------
-- Debugging 
-------------

{-- | print trace message
println msg = trace (show msg) $ return ()

-- | Use seeNext 10 shows next 10 characters to be consumed
seeNext :: Int -> ParsecT String u Identity ()
seeNext n = do
  s <- getParserState
  let out = take n (stateInput s)
  println out
--}