{-# LANGUAGE OverloadedStrings #-}

module Parser.NBT where

import qualified Data.Map as M
import Data.Binary.Get
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B (readFile)
import Codec.Compression.GZip
import Data.Int
import Data.Bits
import Control.Applicative
import Control.Monad

type NBT = M.Map ByteString NBTType
data NBTType = NBTByte Int8
  | NBTShort Int16
  | NBTInt Int32
  | NBTLong Int64
  | NBTFloat Float
  | NBTDouble Double
  | NBTByteArray [Int8]
  | NBTString ByteString
  | NBTList [NBTType]
  | NBTCompound NBT
  | NBTIntArray [Int32]
  | NBTLongArray [Int64]
  deriving (Show, Eq)

getString :: Get ByteString
getString = getInt16be >>= getLazyByteString . fromIntegral

getByType :: Int8 -> Get NBTType
getByType 1 = NBTByte <$> getInt8
getByType 2 = NBTShort <$> getInt16be
getByType 3 = NBTInt <$> getInt32be
getByType 4 = NBTLong <$> getInt64be
getByType 5 = NBTFloat <$> getFloatbe
getByType 6 = NBTDouble <$> getDoublebe
getByType 7 = NBTByteArray <$> getList getInt8
getByType 8 = NBTString <$> getString
getByType 9 = NBTList <$> (getInt8 >>= getList . getByType)
getByType 10 = NBTCompound . M.fromList <$> (many getNBTType <* skip 1)
getByType 11 = NBTIntArray <$> getList getInt32be
getByType 12 = NBTLongArray <$> getList getInt64be
getByType _ = empty

getList :: Get a -> Get [a]
getList getter = do
  len <- getInt32be
  replicateM (fromIntegral len) getter

getNBTType :: Get (ByteString, NBTType)
getNBTType = do
  tagtype <- getInt8
  guard $ tagtype /= 0
  name <- getString
  (,) name <$> getByType tagtype

nbtContents :: String -> IO NBTType
nbtContents fname = do
  cont <- B.readFile fname
  let ("", nbt) = runGet getNBTType (decompress cont)
  return nbt

(!) :: NBTType -> ByteString -> NBTType
(NBTCompound nbt) ! bs = nbt M.! bs

registers :: NBTType -> (M.Map ByteString Int32)
registers nbt = M.fromList [(name, value) | let (NBTList scores) = nbt ! "data" ! "PlayerScores", score <- scores, score ! "Objective" == NBTString "registers", let (NBTString name) = score ! "Name", let (NBTInt value) = score ! "Score"]

memory :: NBTType -> M.Map Int32 Int32
memory nbt = makeMemory [] $ nbt ! "data" ! "contents" ! "memory" ! "memory"
  where
    makeMemory :: [Int32] -> NBTType -> M.Map Int32 Int32
    makeMemory ranks nbt = let (NBTInt rank) = nbt ! "rank"; (NBTInt value) = nbt ! "value"; (NBTList children) = nbt ! "children"; ranks' = if rank == 31 then ranks else rank : ranks in M.insert (foldr (\c a -> setBit a (fromIntegral $ 30 - c)) 0 ranks') value (M.unions $ map (makeMemory ranks') children)