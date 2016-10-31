import McAsmParser
import Mappings

import System.Environment (getArgs)
import qualified Data.Sequence as Seq
import qualified Data.List as List
import Data.Foldable (toList)
import Data.Bits
import Debug.Trace
import Control.Monad.Reader
import qualified Data.Yaml as Yaml
import System.IO (openFile, hClose, IOMode(WriteMode))
import Data.Ix (range)

import Data.Word (Word64, Word8)
import Data.ByteString.Builder (word8, hPutBuilder)

data CompileError = None
  deriving (Show)

main :: IO ()
main = do
  [inFileName, outBaseName] <- getArgs
  inData <- readFile inFileName
  let parsed = parseAsm inData
  mappings <- Yaml.decodeFileEither "../microcode/mappings.yml"
  case mappings of
    Left err -> print err
    Right mappingsInner ->
      case parsed of
        Left err -> print err
        Right parsedInner ->
          case compile mappingsInner parsedInner of
            Left err -> print err
            Right result -> writeBinaries result outBaseName

writeBinaries :: [Word64] -> String -> IO ()
writeBinaries dat outBaseName = do
  files <- mapM (\num -> openFile (outBaseName ++ "." ++ show num) WriteMode) (range (0, 7))
  _ <- mapM (writeEntry files) dat
  _ <- mapM hClose files
  return ()
  where writeEntry files piece = mapM (writeEntryPart piece) (zip files (range (0, 7))) >> return ()
        writeEntryPart piece (handle, num) =
          let bitNum = num * 8
              out = (fromIntegral $ ((shiftR piece bitNum) .&. 0xff) :: Word8)
          in hPutBuilder handle $ word8 $ out


compile :: MappingData -> [AsmItem] -> Either CompileError Output
compile mappings parsedData =
  let opcodeMap = sortOpcodes parsedData
      --filledDefaultStages = fillDefaultStages parsedData opcodeMap
      compiled = compileOpcodes opcodeMap -- filledDefaultStages
  in Right $ runReader compiled mappings

-- Sort pass

type OpcodeMap = Seq.Seq OpcodeStageMap

emptyOpcodeMap :: OpcodeMap
emptyOpcodeMap = Seq.fromList $ take 256 $ repeat emptyOpcodeStageMap

applyItemOpcodeMap :: OpcodeMap -> AsmItem -> OpcodeMap
applyItemOpcodeMap m (Command (Symbol "OPCODE") [NumberArg opcodeNum] d) =
  Seq.update (fromIntegral opcodeNum) (sortStages d) m
applyItemOpcodeMap m _ = m

sortOpcodes :: [AsmItem] -> OpcodeMap
sortOpcodes items = foldl applyItemOpcodeMap emptyOpcodeMap items

type OpcodeStageMap = Seq.Seq [StageFlag]

emptyOpcodeStageMap :: OpcodeStageMap
emptyOpcodeStageMap = Seq.fromList $ take 16 $ repeat []

applyStageOpcodeStageMap :: OpcodeStageMap -> CommandStage -> OpcodeStageMap
applyStageOpcodeStageMap m (Stage n f) = Seq.update (fromIntegral n) f m

sortStages :: [CommandStage] -> OpcodeStageMap
sortStages stages = foldl applyStageOpcodeStageMap emptyOpcodeStageMap stages

-- Compile pass

flagOrder :: [[(Char, Bool)] -> Bool]
flagOrder =
  map (\template -> evalConds template) [
  "", "Z", "S", "ZS", "C", "CZ", "CS", "CSZ",
  "", "Z", "S", "ZS", "C", "CZ", "CS", "CSZ"
  ]

type Output = [Word64]
type MappingR a = Reader MappingData a

compileOpcodes :: OpcodeMap -> MappingR Output
compileOpcodes m = mapM compileStages m >>= return . concat

compileStages :: OpcodeStageMap -> MappingR Output
compileStages m = mapM compileStage m >>= return . concat

compileStage :: [StageFlag] -> MappingR Output
compileStage flags = mapM (compileFlags flags) flagOrder

compileFlags :: [StageFlag] -> ([(Char, Bool)] -> Bool) -> MappingR Word64
compileFlags flags evalCondsB = do
  mappingData <- ask
  let iv = initialValue mappingData
  foldM (\acc x -> applyFlag evalCondsB x acc) iv flags

applyFlag :: ([(Char, Bool)] -> Bool) -> StageFlag -> Word64 -> MappingR Word64
applyFlag evalCondsB (ConditionalFlag (Condition conds) flag) val =
  if evalCondsB conds
  then applyFlagInner flag val >>= return
  else return val
applyFlag _ (AlwaysFlag flag) val = applyFlagInner flag val >>= return

applyFlagInner :: FlagType -> Word64 -> MappingR Word64
applyFlagInner flag out = do
  mappingData <- ask
  return $ setFlag mappingData flag out

evalConds :: String -> [(Char, Bool)] ->  Bool
evalConds template conds = all (\(char, inverse) ->
                                  let s = List.isInfixOf [char] template
                                  in if inverse then s else not s
                               ) conds
