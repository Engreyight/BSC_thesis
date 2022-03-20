{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

import Types
import Parser.Base
import Parser.Intel
import Builder
import Assembler

import Text.Parsec hiding ((<|>), some, many, optional)
import Text.Parsec.ByteString (parseFromFile)
import Control.Applicative
import Control.Monad.Trans.RWS.CPS
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import System.IO
import System.Environment

test :: Parser Instruction -> ByteString -> IO ()
test p input = either (fail . show) (\instr -> hPutBuilder stdout $ snd $ evalRWS (processInstruction instr) () ()) $ parse (p <* eof) "" input

main = do
  (fname : _) <- getArgs
  Right instr <- parseFromFile (sepEndBy1 parseInstruction newline) fname
  let (_, res) = evalRWS (traverse processInstruction instr) () ()
  withFile (fname ++ ".mcfunction") WriteMode $ \h -> hPutBuilder h res