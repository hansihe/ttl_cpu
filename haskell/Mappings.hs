{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BinaryLiterals #-}
module Mappings where

import McAsmParser (FlagType(GotoFlag, AssertFlag))

import GHC.Generics
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Yaml

import Data.Word (Word8, Word64)
import Data.Bits
import Data.Char
import Debug.Trace
import Data.Text (unpack)

import Data.Ix (range)
import qualified Data.Vector as Vector

data MappingData = MappingData { flags :: HashMap.HashMap String [Int]
                               , flag_defaults :: [Int]
                               , aliases :: HashMap.HashMap String [String]
                               , opcode_ids :: HashMap.HashMap String Word8
                               , instructions :: HashMap.HashMap String [InstructionVariantData]
                               } deriving (Show, Generic)
instance FromJSON MappingData

data InstructionVariantData = InstructionVariantData { opcodes :: [String]
                                                     , opcodeAux :: [Word8]
                                                     , format :: [FormatEntryData]
                                                     } deriving (Show, Generic)
instance FromJSON InstructionVariantData

data FormatEntryData = FormatEntryMarker String
                     | FormatEntrySetter String [Int]
                     deriving (Show)
instance FromJSON FormatEntryData where
  parseJSON val@(Array _) = do
    (typ, dst) <- liftSuccess $ fromJSON val :: Parser (String, [Int])
    return $ FormatEntrySetter typ dst
  parseJSON str@(String _) = liftSuccess $ fromJSON str >>= return . FormatEntryMarker
  parseJSON e = fail $ "Invalid FormatEntryData: " ++ (show e)

liftSuccess :: Monad b => Result a -> b a
liftSuccess i = case i of
  Success a -> return a
  Error a -> fail a

initialValue :: MappingData -> Word64
initialValue mapping = foldl applyBit 0 (zip (flag_defaults mapping) (range (0, 63)))
  where applyBit acc (state, num) =
          if state == 0
          then acc
          else setBit acc num

setFlag :: MappingData -> FlagType -> Word64 -> Word64
setFlag _ (GotoFlag num) dat = ((fromIntegral num) .&. 0b1111) .|. ((complement 0b1111) .&. dat)
setFlag mapping (AssertFlag flagName) dat =
  case resolveFlagBits mapping flagName of
    Nothing -> error $ "Flag not resolvable: " ++ flagName
    Just bits -> setBits bits dat

setBits :: [Int] -> Word64 -> Word64
setBits bits dat = foldl applyBit dat bits
  where applyBit acc x =
          if x > 0 then setBit acc (x-1)
          else if x < 0 then clearBit acc (-x-1)
          else error "Bit cannot be 0"

resolveFlagBits :: MappingData -> String -> Maybe [Int]
resolveFlagBits mapping flagNameOrig =
  let flagName = map toLower flagNameOrig
  in case HashMap.lookup flagName (flags mapping) of
    Just a -> Just a
    Nothing ->
      case HashMap.lookup flagName (aliases mapping) of
        Nothing -> Nothing
        Just a ->
          case sequence $ map (resolveFlagBits mapping) a of
            Nothing -> Nothing
            Just b -> Just $ concat b

getInstrByMnemonic :: MappingData -> String -> Maybe [InstructionVariantData]
getInstrByMnemonic mapping mnem = HashMap.lookup (map toLower mnem) (instructions mapping)

formatEntryGetTypeName :: FormatEntryData -> String
formatEntryGetTypeName (FormatEntryMarker t) = t
formatEntryGetTypeName (FormatEntrySetter t _) = t
