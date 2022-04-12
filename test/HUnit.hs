{-# LANGUAGE OverloadedStrings #-}

module HUnit where

import Main
import Parser.NBT

import Network.Socket
import Network.Socket.ByteString.Lazy
import Test.HUnit.Base
import Test.HUnit.Text
import Data.Int
import Data.ByteString.Lazy.Char8 (ByteString, pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BL (length)
import Data.ByteString.Builder
import Control.Monad
import System.FilePath
import System.Directory
import qualified Data.Map as M

connectToServer :: HostName -> ServiceName -> ByteString -> IO Socket
connectToServer host port password = do
  (addr : _) <- getAddrInfo (Just $ defaultHints {addrSocketType = Stream}) (Just host) (Just port)
  s <- openSocket addr
  connect s (addrAddress addr)
  sendRcon 0 3 password s
  return s

-- gives socket back for >>=-chaining
sendRcon :: Int32 -> Int32 -> ByteString -> Socket -> IO Socket
sendRcon mId mType msg s = do
  send s (toLazyByteString $ int32LE (fromIntegral $ BL.length msg + 10) <> int32LE mId <> int32LE mType <> lazyByteString msg <> int16LE 0)
  s <$ recv s 2048

makeTest :: FilePath -> Bool -> [(ByteString, Int32)] -> [(Int32, Int32)] -> FilePath -> Socket -> Test
makeTest mainFun reset reg mem serverFolder s = mainFun ~: do
  when reset $ do
    sendRcon 0 2 "function assembler:library/nuke" s
    void $ sendRcon 0 2 "function assembler:library/init" s
  sendRcon 0 2 ("function assembler:tests/" <> pack (mainFun </> mainFun)) s
  sendRcon 0 2 "save-all" s
  when (not $ null reg) $ do
    registers <- registers <$> nbtContents (serverFolder </> "world/data/scoreboard.dat")
    foldMap (\(key, val) -> assertEqual (unpack key) val (M.findWithDefault 0 key registers)) reg
  when (not $ null mem) $ do
    memory <- memory <$> nbtContents (serverFolder </> "world/data/command_storage_assembler.dat")
    foldMap (\(key, val) -> assertEqual ("memory @ " ++ show key) val (M.findWithDefault 0 key memory)) mem

runTests :: HostName -> ServiceName -> ByteString -> FilePath -> IO ()
runTests host port password serverFolder = let funDir = serverFolder </> "world/datapacks/assembler/data/assembler/functions/tests" in do
  createDirectoryIfMissing False funDir
  s <- connectToServer host port password
  let (assembleAll, testCases) = traverse (\(testFile, reset, reg, mem) -> let mainFun = takeBaseName testFile in (assemble (funDir </> mainFun) [testFile], makeTest mainFun reset reg mem serverFolder s)) tests
  assembleAll
  sendRcon 0 2 "reload" s
  runTestTT $ TestList testCases
  close s
  removeDirectoryRecursive funDir
  where
    tests = [
        ("test/tests/test_mov.txt", True, [("eax", 24), ("edx", 0x1234)], [(5, 0x12cd00ab), (4, 0x1234), (3, 0x43210000)]),
        ("test/tests/test_mov_overwrite.txt", False, [("eax", 19), ("edx", 0x1256)], [(5, 0x12abcdab), (4, 0x1234), (3, 0x13572468)]),
        ("test/tests/test_add.txt", True, [("eax", 0x178)], [(3, 0x133)]),
        ("test/tests/test_add2.txt", False, [("eax", 0x167), ("edx", 0x789abe11)], []),
        ("test/tests/test_sub.txt", True, [("edx", 0x3a1c66f6)], [(4, 0x233f), (3, 0x2f680000)]),
        ("test/tests/test_imul.txt", True, [("eax", 24), ("edx", 18), ("ecx", 120), ("ebx", 72)], []),
        ("test/tests/test_idiv.txt", True, [("ebx", -2), ("eax", 65279), ("edx", 0)], []),
        ("test/tests/test_lea.txt", True, [("eax", 224), ("ebx", 220), ("ecx", -540)], [])
      ]