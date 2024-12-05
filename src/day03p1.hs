-----------------------------------------------------------------------------
-- | Day 3 - Part 1
-- 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (Parser, many, try)
import Text.Parsec.Error(ParseError (..))
import Text.Parsec.Char (digit, string, char, anyChar)
import Text.Parsec.Combinator (many1, manyTill)
import Text.Parsec.Prim (parse, (<|>))
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
  let multiplications = (extractMultiplications input) :: [Instruction] 
  putStrLn . show . foldl (+) 0 . map (\(Mul x y) -> x * y) $ multiplications 

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