module Types where

data Operand = Register Int String | Memory | Immediate Int deriving (Show)
data Instruction = Add Operand Operand  -- r rmi / m ri (rm rmi but only one m)
  | Imul Operand Operand Operand  -- rm / r rmi / r rm i
  deriving (Show)

isRegister :: Operand -> Bool
isRegister (Register _ _) = True
isRegister _ = False

isMemory :: Operand -> Bool
isMemory (Memory) = True
isMemory _ = False

isImmediate :: Operand -> Bool
isImmediate (Immediate _) = True
isImmediate _ = False