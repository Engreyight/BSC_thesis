{-# LANGUAGE FlexibleContexts #-}
module Parser.AMD where

import Types
import Parser.Base

import Text.Parsec hiding ((<|>), some, many, optional)
import Control.Applicative

parseOperand :: Parser Operand
parseOperand = (char '%' *> parseRegister) <|> parseMemory <|> (char '$' *> parseImmediate)

parseMemory :: Parser Operand
parseMemory = fail "Cannot handle memory locations yet"

