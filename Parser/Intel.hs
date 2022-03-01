{-# LANGUAGE FlexibleContexts #-}
module Parser.Intel where

import Types
import Parser.Base

import Text.Parsec hiding ((<|>), some, many, optional)
import Control.Applicative

parseOperand :: Parser Operand
parseOperand = parseRegister <|> parseMemory <|> parseImmediate

parseMemory :: Parser Operand
parseMemory = fail "Cannot handle memory locations yet"

parseAdd :: Parser Instruction
parseAdd = do
  string "add"
  spaces
  ops <- sepBy1 parseOperand comma
  case ops of
    [op1, op2]
      | isRegister op1
      || (isMemory op1 && not (isMemory op2))
      -> return $ Add op1 op2
    _ -> fail "Invalid operands for add"

-- TODO: choose appropriate register size
parseImul :: Parser Instruction
parseImul = do
  string "imul"
  spaces
  ops <- sepBy1 parseOperand comma
  case ops of
    [op1]
      | isRegister op1 || isMemory op1
      -> return $ Imul eax eax op1
    [op1, op2]
      | isRegister op1
      -> return $ Imul op1 op1 op2
    [op1, op2, op3]
      | isRegister op1
      && isRegister op2 || isMemory op2
      && isImmediate op3
      -> return $ Imul op1 op2 op3
    _ -> fail "Invalid operands for imul"