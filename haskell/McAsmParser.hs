module McAsmParser where

import Data.Maybe (isNothing)
import Data.Functor ((<$>))

import Text.ParserCombinators.Parsec
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Number as ParsecNumber

data AsmItem = Command String [CommandArg] [CommandStage]
             deriving (Show)

data CommandArg = NumberArg Integer
                | SymbolArg String
                deriving (Show)

data CommandStage = Stage Integer [StageFlag]
                  deriving (Show)

data StageFlag = AlwaysFlag FlagType
               | ConditionalFlag FlagCondition FlagType
               deriving (Show)

data FlagCondition = Condition [(Char, Bool)]
                   deriving (Show)

data FlagType = AssertFlag String
              | GotoFlag Integer
              deriving (Show)

type P ret = Parsec.Parsec String () ret

items :: P [AsmItem]
items = do
  parsedItems <- Parsec.many $ comments >> item
  comments
  Parsec.eof
  return parsedItems
item :: P AsmItem
item = command >>= return

comments :: P ()
comments = do
  _ <- Parsec.many $ (comment >> newline >> return ()) <|> (newline >> return ())
  return ()
comment :: P ()
comment = Parsec.char '#' >> Parsec.many (Parsec.noneOf "\r\n") >> return ()

command :: P AsmItem
command = do
  (name, args) <- commandHeader
  stages <- commandStages
  return $ Command name args stages

commandHeader :: P (String, [CommandArg])
commandHeader = do
  _ <- Parsec.char ':'
  name <- symbol
  _ <- Parsec.spaces
  args <- Parsec.sepBy commandArg Parsec.spaces
  Parsec.spaces
  _ <- Parsec.char ':'
  comments
  return (name, args)

commandStages :: P [CommandStage]
commandStages = Parsec.many commandStage >>= return

commandStage :: P CommandStage
commandStage = do
  from <- stageComeFrom
  comments
  flags <-  Parsec.many commandStageFlag
  return $ Stage from flags

commandStageFlag :: P StageFlag
commandStageFlag = do
  cond <- Parsec.optionMaybe commandStageFlagConditional
  action <- commandStageFlagType
  comments
  case cond of
    Nothing -> return $ AlwaysFlag action
    Just conds -> return $ ConditionalFlag conds action

commandStageFlagConditional :: P FlagCondition
commandStageFlagConditional = do
  _ <- Parsec.char '@'
  conds <- Parsec.manyTill conditionalElement (try $ Parsec.char ' ')
  return $ Condition conds

conditionalElement :: P (Char, Bool)
conditionalElement = do
  invert <- Parsec.optionMaybe $ Parsec.char '!'
  cond <- Parsec.letter
  return $ (cond, isNothing invert)

commandStageFlagType :: P FlagType
commandStageFlagType = stageGoto <|> stageAssert

stageComeFrom :: P Integer
stageComeFrom = Parsec.char '<' >> number

stageGoto :: P FlagType
stageGoto = Parsec.char '>' >> number >>= return . GotoFlag
stageAssert :: P FlagType
stageAssert = symbol >>= (\s -> return $ AssertFlag s)

commandArg :: P CommandArg
commandArg = NumberArg <$> number <|> SymbolArg <$> symbol

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
