module Types where
--                                          size     index      scale      base       disp
data Operand = Register Int String | Memory Int (Maybe Operand) Int (Maybe Operand) Int | Immediate Int deriving (Show, Eq)
data Instruction = Add Operand Operand  -- r rmi / m ri (rm rmi but only one m)
  | Sub Operand Operand  -- r rmi / m ri (rm rmi but only one m)
  | Imul Operand Operand Operand  -- rm / r rmi / r rm i
  | ExtIdiv Operand  -- rm
  | Mov Operand Operand  -- r rmi / m ri (rm rmi but only one m)
  deriving (Show)

isRegister :: Operand -> Bool
isRegister (Register _ _) = True
isRegister _ = False

isMemory :: Operand -> Bool
isMemory (Memory _ _ _ _ _) = True
isMemory _ = False

isImmediate :: Operand -> Bool
isImmediate (Immediate _) = True
isImmediate _ = False