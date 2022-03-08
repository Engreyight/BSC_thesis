{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

import Types
import Parser.Base
import Parser.Intel
import Builder
import Assembler

import Text.Parsec hiding ((<|>), some, many, optional)
import Control.Applicative
import Control.Monad.Trans.RWS.CPS
import Data.ByteString.Builder
import System.IO

test :: Parser Instruction -> String -> IO ()
test p input = either (fail . show) (\instr -> hPutBuilder stdout $ snd $ evalRWS (processInstruction instr) () ()) $ parse (p <* eof) "" input