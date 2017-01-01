
import Mappings

import System.Environment (getArgs)
import AsmParser
import Debug.Trace
import Data.Foldable (toList)
import qualified Data.Yaml as Yaml
import Data.Either.Utils (maybeToEither, eitherToMonadError)
import Data.List (find, mapAccumL, isSuffixOf)
import qualified Text.Parsec as Parsec
import Data.Maybe (fromJust)
import Data.Foldable (asum)
import Control.Monad (liftM)
--import Control.Monad.Except (Except, runExcept)
import Data.Word (Word8)
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Bits as Bits
import System.IO (openFile, hClose, IOMode(WriteMode))
import Data.ByteString.Builder (word8, hPutBuilder)
import System.Process (readProcess)
import qualified Data.Text as Text

data AsmEnv = AsmEnv { assignments :: [(String, Integer)]
                     , mappings :: MappingData
                     } deriving (Show)

data AsmInstr = AsmInstr {
  -- Data from parser
  mnemonic :: String,
  arguments :: String,
  label :: Maybe String,
  line :: Int,

  -- Merged data from mapping (stage 1)
  mapping :: Maybe InstructionVariantData,
  parsedArgs :: Maybe [ArgParserRet],
  instrData :: [(Word8, Word8)],
  size :: Int,

  -- Stage 2
  offset :: Int
  } deriving (Show)

main :: IO ()
main = do
  res <- runExceptT inner
  print $ show res
  return ()

parseMappings :: IO (Either Yaml.ParseException MappingData)
parseMappings = Yaml.decodeFileEither "../microcode/mappings.yml"

inner :: ExceptT String IO ()
inner = do
  [inFileName, outFileName] <- liftIO getArgs

  -- Parse AST.
  --inData <- liftIO $ readFile inFileName
  inData <- liftIO $ readProcess "gpp" ["-C", inFileName] []
  ast <- throwLeft $ parseAsm inData

  -- Parse mappings.
  pMappings <- (liftIO parseMappings) >>= throwLeft

  let env = envFromAst ast pMappings

  -- Actual compiler passes follow:

  -- Transform the AST into a list of instructions
  let parsedStage = fromSyntax ast
  -- Match every instruction with a pattern from the mapping file.
  -- If no pattern matches, fail with an error message.
  matchedStage <- throwLeft $ mapM (matchInstrMapping env) parsedStage
  -- Now that we know the types (therefore the sizes) of all instructions,
  -- we can assign every instruction its offset in memory.
  let offsetStage = assignOffsets matchedStage
  let labelOffsets = collectLabels offsetStage
  -- Apply the formatting for every instruction as specified in the mappings file.
  -- This needs to be done after label collect because we also calculate jumps in this stage.
  let formattedStage = map (applyInstrFormattings labelOffsets) offsetStage
  --liftIO $ print ast
  --liftIO $ print formattedStage
  -- Collect the data from all instructions into a final array.
  let finalData = collectResult formattedStage

  -- Write data out to destination file
  liftIO $ writeBinaries finalData outFileName
  liftIO $ print finalData

  return ()

writeBinaries :: [(Word8, Word8)] -> String -> IO ()
writeBinaries dat outBaseName = do
  _ <- writeToFile (\(i, _) -> i) dat (outBaseName ++ ".0")
  _ <- writeToFile (\(_, i) -> i) dat (outBaseName ++ ".1")
  return ()

writeToFile :: (a -> Word8) -> [a] -> String -> IO ()
writeToFile fun itms fileName = do
  file <- openFile fileName WriteMode
  _ <- mapM (writeItem file) (map fun itms)
  _ <- hClose file
  return ()
  where writeItem file val = hPutBuilder file $ word8 val

type LabelOffsets = HashMap.HashMap String Int

throwLeft :: (Show a, Monad m) => Either a b -> ExceptT String m b
throwLeft i = case i of
  Left err -> throwE $ show err
  Right ret -> return ret

collectLabels :: [AsmInstr] -> LabelOffsets
collectLabels instrs = foldl collectLabel HashMap.empty instrs
  where collectLabel labelMap instr =
          case label instr of
            Nothing -> labelMap
            Just labelStr -> HashMap.insert labelStr (offset instr) labelMap

applyInstrFormattings :: LabelOffsets -> AsmInstr -> AsmInstr
applyInstrFormattings labelOffsets instr =
  let mapp = zip (format $ fromJust $ mapping instr) (fromJust $ parsedArgs instr)
  in foldl (applyInstrFormatting labelOffsets) instr mapp

applyInstrFormatting :: LabelOffsets -> AsmInstr -> (FormatEntryData, ArgParserRet) -> AsmInstr
applyInstrFormatting _ instr ((FormatEntryMarker _), _) = instr
applyInstrFormatting _ instr ((FormatEntrySetter "mem" [pos]), IntegerArg int) =
  instrSetDataAux instr pos (fromIntegral int)
applyInstrFormatting _ instr ((FormatEntrySetter "int" [pos]), IntegerArg int) =
  instrSetDataAux instr pos (fromIntegral int)
applyInstrFormatting _ instr ((FormatEntrySetter "reg" [pos]), IntegerArg int) =
  instrSetDataAux instr pos (fromIntegral int)
applyInstrFormatting _ instr ((FormatEntrySetter "d_reg" [pos]), IntegerArg int) =
  instrSetDataAux instr pos (fromIntegral int)
applyInstrFormatting _ instr ((FormatEntrySetter "io_mem" [pos]), IntegerArg int) =
  instrSetDataAux instr pos (fromIntegral int)
applyInstrFormatting _ instr ((FormatEntrySetter "io_d_acc" [pos]), IntegerArg int) =
  instrSetDataAux instr pos (fromIntegral int)
applyInstrFormatting _ instr ((FormatEntrySetter "io_d_reg" [pos]), IntegerArg int) =
  instrSetDataAux instr pos (fromIntegral int)
applyInstrFormatting labelOffsets instr ((FormatEntrySetter "label" [upper, lower]), LabelArg label) =
  let labelOffset =
        case HashMap.lookup label labelOffsets of
          Nothing -> error $ "Undefined label " ++ label ++ " in instruction " ++ (show instr)
          Just res -> res
  in (\a -> instrSetDataAux a lower (fromIntegral labelOffset))
     $ instrSetDataAux instr upper (fromIntegral $ Bits.shift labelOffset 8)
applyInstrFormatting _ _ (fmt, _) = error $ (show fmt) ++ " is not a valid formatter."

instrSetDataAux :: AsmInstr -> Int -> Word8 -> AsmInstr
instrSetDataAux instr pos dat = let checkIdx (idx, val@(opc, _)) =
                                      if idx == pos then (opc, dat) else val
                                in instr { instrData = map checkIdx $ zip [0..] (instrData instr) }

collectResult :: [AsmInstr] -> [(Word8, Word8)]
collectResult instrs = concat $ map instrData instrs

assignOffsets :: [AsmInstr] -> [AsmInstr]
assignOffsets instrs =
  let (_, a) = mapAccumL assignOffset 0 $ instrs :: (Int, [AsmInstr])
  in a

assignOffset :: Int -> AsmInstr -> (Int, AsmInstr)
assignOffset currOffset instr = (currOffset + size instr, instr { offset = currOffset })

fromSyntax :: [AsmItem] -> [AsmInstr]
fromSyntax ast = toList $ foldl collectInstr [] (reverse ast)
  where collectInstr acc (Instruction mne args lab srcLine) = AsmInstr { mnemonic = mne
                                                                       , arguments = args
                                                                       , label = lab
                                                                       , line = srcLine
                                                                       , mapping = Nothing
                                                                       , parsedArgs = Nothing
                                                                       , instrData = []
                                                                       , size = 0
                                                                       , offset = 0
                                                                       } : acc
        collectInstr acc _ = acc

envFromAst :: [AsmItem] -> MappingData -> AsmEnv
envFromAst ast mappings = AsmEnv { assignments = []
                                 , mappings = mappings
                                 }

matchInstrMapping :: AsmEnv -> AsmInstr -> Either String AsmInstr
matchInstrMapping env instr = do
  variants <- maybeToEither (mnemonicNotDefinedError instr) $
    getInstrByMnemonic (mappings env) (mnemonic instr)
  (parsedArgsI, variant) <-
    maybeToEither (noMatchingInstrVariantError instr) $
    asum $ map raiseTupleMaybe $ zip (map (parseVariantArgs $ arguments instr) variants) variants
  return $ instr { size = length $ opcodes variant
                 , mapping = Just variant
                 , parsedArgs = Just parsedArgsI
                 , instrData = (zip (matchOpcodeNames env $ opcodes variant)
                                (opcodeAux variant ++ repeat 0))
                 }

matchOpcodeNames :: AsmEnv -> [String] -> [Word8]
matchOpcodeNames env instrs = map (lookupItem $ mappings env) instrs
  where lookupItem mapping a = fromJustMsg ("Opcode id " ++ a ++ " not found.")
          $ HashMap.lookup a (opcode_ids mapping)

parseVariantArgs :: String -> InstructionVariantData -> Maybe [ArgParserRet]
parseVariantArgs args variant = do
  let argParsers = map (getArgTypeParser . formatEntryGetTypeName) (format variant) :: [ArgParser]
  (argsParsed, remaining) <- case foldl foldParser (Right ([], args)) argParsers of
    Left err -> fail err
    Right res -> return res
  _ <- if remaining == ""
    then return ()
    else fail ("Instruction variant " ++ (show variant) ++ " with args " ++ args ++ " has unmatched arguments")
  return argsParsed
    where foldParser :: Either String ([ArgParserRet], String) -> ArgParser -> Either String ([ArgParserRet], String)
          foldParser acc parser = do
            (parsed, argsLeft) <- acc
            (resp, argsLeftN) <- parser argsLeft
            return (parsed ++ [resp], argsLeftN)

-- ASM Arguments

type Parser a = Parsec.Parsec String () a
type ArgParser = String -> Either String (ArgParserRet, String)

data ArgParserRet = VoidArg
                  | IntegerArg Integer
                  | LabelArg String
                  deriving (Show)

getArgTypeParser :: String -> ArgParser
getArgTypeParser "int" = parseIntArg
getArgTypeParser "acc" = parseAccArg
getArgTypeParser "d_acc" = parseDeferAccArg
getArgTypeParser "reg" = parseRegArg
getArgTypeParser "d_reg" = parseDerefRegArg
getArgTypeParser "mem" = parseMemArg
getArgTypeParser "io_mem" = parseIoMemArg
getArgTypeParser "io_d_reg" = parseIoDerefRegArg
getArgTypeParser "io_d_acc" = parseIoDerefAccArg
getArgTypeParser "label" = parseLabelArg
getArgTypeParser typ = error $ "Invalid arg type '" ++ typ ++ "' in mapping"

parseAccArg :: ArgParser
parseAccArg i = makeArgParser i (Parsec.string "acc" >> return VoidArg)

parseDeferAccArg :: ArgParser
parseDeferAccArg i = makeArgParser i (Parsec.string "[acc]" >> return VoidArg)

parseIntArg :: ArgParser
parseIntArg i = makeArgParser i parseNumArg

parseRegArg :: ArgParser
parseRegArg i = makeArgParser i (Parsec.string "%" >> parseNumArg)

parseDerefRegArg :: ArgParser
parseDerefRegArg i = makeArgParser i $ do
  _ <- Parsec.string "[%"
  num <- parseNumArg
  _ <- Parsec.string "]"
  return num

parseMemArg :: ArgParser
parseMemArg i = makeWrappedParser i ((Parsec.string "["), (Parsec.string "]")) parseNumArg

parseIoMemArg :: ArgParser
parseIoMemArg i = makeWrappedParser i ((Parsec.string ">["), (Parsec.string "]")) parseNumArg

parseIoDerefRegArg :: ArgParser
parseIoDerefRegArg i = makeArgParser i $ do
  _ <- Parsec.string ">[%"
  num <- parseNumArg
  _ <- Parsec.string "]"
  return num

parseIoDerefAccArg :: ArgParser
parseIoDerefAccArg i = makeArgParser i (Parsec.string ">[acc]" >> return VoidArg)

parseLabelArg :: ArgParser
parseLabelArg i = makeArgParser i (symbol >>= return . LabelArg)

makeWrappedParser :: String -> (Parser b, Parser c) -> Parser a -> Either String (a, String)
makeWrappedParser input (left, right) innerParse = makeArgParser input parser
  where parser = do
          _ <- lexeme left
          res <- lexeme innerParse
          _ <- lexeme right
          return res

parseNumArg :: Parser ArgParserRet
parseNumArg = number >>= (return . IntegerArg)

makeArgParser :: String -> Parser a -> Either String (a, String)
makeArgParser input parseFun = showLeft $ Parsec.parse parser input input
  where parser = do
          ret <- lexeme parseFun
          rest <- parseRest
          return (ret, rest)

parseRest :: Parser String
parseRest = Parsec.manyTill Parsec.anyChar Parsec.eof

-- Error messages
makeInstrError :: (AsmInstr -> String) -> AsmInstr -> String
makeInstrError fun instr = "Error at line " ++ (show $ line instr) ++ ":\r\n" ++ (fun instr)

mnemonicNotDefinedError :: AsmInstr -> String
mnemonicNotDefinedError =
  makeInstrError $ (\i ->"Mnemonic '" ++ (mnemonic i) ++ "' is not defined as an instruction")

noMatchingInstrVariantError :: AsmInstr -> String
noMatchingInstrVariantError =
  makeInstrError $ (\i -> "No variant of opcode '" ++ (mnemonic i) ++
  "' matches given arguents arguments " ++ (show $ arguments i))

-- Utils
showLeft :: Show a => Either a b -> Either String b
showLeft i = either (\a -> Left $ show a) (\b -> Right b) i

lexeme :: Parsec.Parsec String () a -> Parsec.Parsec String () a
lexeme parser = do
  val <- parser
  _  <- Parsec.spaces
  return val

raiseTupleMaybe :: (Maybe a, b) -> Maybe (a, b)
raiseTupleMaybe (Just a, b) = Just (a, b)
raiseTupleMaybe (Nothing, _) = Nothing

raiseTupleEither :: (Either a b, c) -> Either a (b, c)
raiseTupleEither (Right a, b) = Right (a, b)
raiseTupleEither (Left a, _) = Left a

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right x) = return x
eitherToMaybe (Left _) = fail ""

fromJustMsg :: String -> Maybe a -> a
fromJustMsg m d = case d of
  Nothing -> error m
  Just a -> a
