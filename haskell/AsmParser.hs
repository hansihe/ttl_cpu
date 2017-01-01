module AsmParser where

import Text.ParserCombinators.Parsec
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Number as ParsecNumber
import Debug.Trace
import Control.Monad (liftM)

data AsmItem = Instruction String String (Maybe String) Int
             | Assignment String Integer Int
             deriving (Show)

type P ret = Parsec.Parsec String () ret

items :: P [AsmItem]
items = do
  _ <- comments
  parsedItems <- Parsec.many $ item
  _ <- Parsec.eof
  return parsedItems

item :: P AsmItem
item = do
  a <- assignmentItem <|> instructionItem
  _ <- comments
  return a

instructionItem :: P AsmItem
instructionItem = do
  labelName <- optionMaybe $ try $ do
    l <- symbol
    _ <- Parsec.char ':'
    return l

  _ <- comments
  instrPos <- getSourceLine
  name <- symbol

  _ <- Parsec.spaces

  arguments <- Parsec.many $ Parsec.noneOf "\n\r"

  return $ Instruction name arguments labelName instrPos

assignmentItem :: P AsmItem
assignmentItem = do
  assignPos <- getSourceLine
  name <- try $ do
    n <- symbol
    _ <- Parsec.spaces
    _ <- Parsec.char '='
    _ <- Parsec.spaces
    return n
  num <- number
  return $ Assignment name num assignPos

comments :: P ()
comments = do
  _ <- Parsec.many $ (comment >> newline >> return ()) <|> (newline >> return ())
  return ()
comment :: P ()
comment = Parsec.string "//" >> Parsec.many (Parsec.noneOf "\r\n") >> return ()

symbol :: P String
symbol = Parsec.many1 (alphaNum <|> (char '_')) >>= return

number :: P Integer
number =
  let hex = try $ Parsec.string "0x" >> ParsecNumber.hexnum
      bin = try $ Parsec.string "0b" >> ParsecNumber.binary
      oct = ParsecNumber.decimal
  in Parsec.choice [hex, bin, oct]

parseAsm :: String -> Either ParseError [AsmItem]
parseAsm text = Parsec.parse items "(source)" text

getSourceLine :: Monad m => Parsec.ParsecT s u m Int
getSourceLine = sourceLine `liftM` statePos `liftM` getParserState
