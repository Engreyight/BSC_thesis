{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

import Types
import Parser.Base
import Parser.Intel
import Assembler

import Text.Parsec hiding ((<|>), some, many, optional)
import Text.Parsec.ByteString (parseFromFile)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.RWS.CPS
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import System.IO
import System.Environment
import Data.List

test :: Parser Instruction -> ByteString -> IO ()
test p input = either (fail . show) (\instr -> hPutBuilder stdout $ snd $ evalRWS (processInstruction instr) () ()) $ parse (p <* eof) "" input

main = do
  (fname : _) <- getArgs
  instr <- either (fail . show) splitFunctions <$> parseFromFile (sepEndBy1 parseInstruction (endOfLine <* spaces) <* eof) fname
  mapM_ (\(label, list) -> withFile (label ++ ".mcfunction") WriteMode $ \h -> hPutBuilder h (snd (evalRWS (traverse processInstruction list) () ()))) instr