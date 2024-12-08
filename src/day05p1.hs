-----------------------------------------------------------------------------
-- | Day 5 - Part 1
-- 
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (Parser, many, try)
import Text.Parsec.Error(ParseError (..))
import Text.Parsec.Char (digit, string, char, anyChar)
import Text.Parsec.Combinator (many1, manyTill, sepBy1, eof)
import Text.Parsec.Prim (parse, (<|>), (<?>))
{-- Debug
import Text.Parsec (ParsecT, getParserState, stateInput)
import Data.Functor.Identity (Identity)
import Debug.Trace (trace)
--}


main = do
  args <- getArgs
  input <- readFile . head $ args
  let (constraints,manuals) = (extractConstraintsAndManuals input) :: ([(Int,Int)],[[Int]])
  putStrLn . show $ (constraints, manuals) 

-- | Parses constraints and manuals
extractConstraintsAndManuals :: String -> ([(Int,Int)],[[Int]])
extractConstraintsAndManuals str
  = case (parse constraintsAndManualsParser "" str) of
      Left err -> error . show $ err
      Right csms -> csms

-- | constraints and manuals
constraintsAndManualsParser :: Parser ([(Int,Int)],[[Int]])
constraintsAndManualsParser 
  = do cs <- many (constraintsParser) 
       eol
       ms <- many (manualsParser)
       return (cs,ms)

-- | contraints
constraintsParser :: Parser (Int,Int)
constraintsParser 
  = do value1 <- natural
       char '|'
       value2 <- natural
       eol
       return $ (read value1,read value2)

-- | manuals
manualsParser :: Parser [Int]
manualsParser 
  = do ms <- natural `sepBy1` (char ',')
       eol
       return . map read $ ms

natural :: Parser String
natural = many1 digit

eol :: Parser String
eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"
  <|> (do {eof ; return ""}) 
  <?> "end of line"
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