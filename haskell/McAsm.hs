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
import Data.List (find)
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HashMap
import System.Process (readProcess)

import Data.Word (Word64, Word8)
import Data.ByteString.Builder (word8, hPutBuilder)

data CompileError = None
  deriving (Show)

main :: IO ()
main = do
  [inFileName, outBaseName] <- getArgs
  --inData <- readFile inFileName
  inData <- readProcess "gpp" ["-T", inFileName] []
  mappings <- Yaml.decodeFileEither "../microcode/mappings.yml"
  case mappings of
    Left err -> print err
    Right mappingsInner ->
      case parseAsm inData of
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
  let opcodeMap = compileOpcodes ((fillDefaultOpcode parsedData) ((sortOpcodes mappings) parsedData))
  in Right $ runReader opcodeMap mappings

-- Sort pass

type OpcodeMap = [OpcodeStageMap]
type OpcodeMapSeq = Seq.Seq OpcodeStageMapSeq

emptyOpcodeMap :: (Seq.Seq (Seq.Seq [StageFlag]))
emptyOpcodeMap = Seq.fromList $ take 256 $ repeat emptyOpcodeStageMap

applyItemOpcodeMap :: MappingData -> (Seq.Seq (Seq.Seq [StageFlag])) -> AsmItem -> (Seq.Seq (Seq.Seq [StageFlag]))
applyItemOpcodeMap _ m (Command "OPCODE" [NumberArg opcodeNum] d) =
  Seq.update (fromIntegral opcodeNum) (sortStages d) m
applyItemOpcodeMap mapping m (Command "OPCODE" [SymbolArg opcodeId] d) =
  Seq.update (fromIntegral $ fromJustMsg ("Opcode id " ++ opcodeId ++ " not in mapping.") $
              HashMap.lookup opcodeId $ opcode_ids mapping) (sortStages d) m
applyItemOpcodeMap _ m _ = m

sortOpcodes :: MappingData -> [AsmItem] -> [[[StageFlag]]]
sortOpcodes mappings items = map toList $ toList $
  foldl (applyItemOpcodeMap mappings) emptyOpcodeMap items

type OpcodeStageMap = [[StageFlag]]
type OpcodeStageMapSeq = Seq.Seq [StageFlag]

emptyOpcodeStageMap :: OpcodeStageMapSeq
emptyOpcodeStageMap = Seq.fromList $ take 16 $ repeat []

applyStageOpcodeStageMap :: OpcodeStageMapSeq -> CommandStage -> OpcodeStageMapSeq
applyStageOpcodeStageMap m (Stage n f) = Seq.update (fromIntegral n) f m

sortStages :: [CommandStage] -> OpcodeStageMapSeq
sortStages stages = foldl applyStageOpcodeStageMap emptyOpcodeStageMap stages

-- Fill default opcode pass

fillDefaultOpcode :: [AsmItem] -> OpcodeMap ->  OpcodeMap
fillDefaultOpcode items opcodeMap =
  case (find findDefaultDecl items) of
    Nothing -> opcodeMap
    Just (Command _ _ stages) ->
      let defStages = toList $ sortStages stages
      in map (checkItem defStages) opcodeMap
  where findDefaultDecl (Command "DEFAULT_OPCODE" _ _) = True
        findDefaultDecl _ = False
        checkItem a [[], [], [], [], [], [], [], [], [], [], [], [], [], [], [], []] = a
        checkItem _ b = b

-- Fill default stage pass

--fillDefaultStage :: [AsmItem] -> OpcodeMap -> OpcodeMap
--fillDefaultStage items opcodeMap =
--  let defCommands = filter findDefaultDecl items
--  in foldl applyDefault opcodeMap defCommands
--  where findDefaultDecl (Command "STAGE_DEFAULT" _ _) = True
--        findDefaultDecl _ = False
--        applyDefault acc defDecl = acc

-- Compile pass

flagOrder :: [[(Char, Bool)] -> Bool]
flagOrder =
  map (\template -> evalConds template) [
  "Z", "", "ZS", "S", "ZC", "C", "ZCS", "CS",
  "Z", "", "ZS", "S", "ZC", "C", "ZCS", "CS"
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
  then applyFlagInner flag val
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

fromJustMsg :: String -> Maybe a -> a
fromJustMsg m d = case d of
  Nothing -> error m
  Just a -> a
