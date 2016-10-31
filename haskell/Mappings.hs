{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BinaryLiterals #-}
module Mappings where

import McAsmParser (FlagType(GotoFlag, AssertFlag), Symbol(Symbol))

import GHC.Generics
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap

import Data.Word (Word64)
import Data.Bits
import Data.Char
import Debug.Trace

import Data.Ix (range)

data MappingData = MappingData { flags :: HashMap.HashMap String [Int]
                               , flag_defaults :: [Int]
                               , aliases :: HashMap.HashMap String [String]
                               }
                 deriving (Show, Generic)
instance FromJSON MappingData

initialValue :: MappingData -> Word64
initialValue mapping = foldl applyBit 0 (zip (flag_defaults mapping) (range (0, 63)))
  where applyBit acc (state, num) =
          if state == 0
          then acc
          else setBit acc num

setFlag :: MappingData -> FlagType -> Word64 -> Word64
setFlag _ (GotoFlag num) dat = ((fromIntegral num) .&. 0b1111) .|. dat
setFlag mapping (AssertFlag (Symbol flagName)) dat =
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
