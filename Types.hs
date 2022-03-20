module Types where

import Control.Monad.Fail (MonadFail)

--                                                 size     index      scale      base     disp
data Operand = Register Int String String | Memory Int (Maybe Operand) Int (Maybe Operand) Int | Immediate Int deriving (Show, Eq)
data Instruction = Add Operand Operand  -- r rmi / m ri (rm rmi but only one m)
  | Sub Operand Operand  -- r rmi / m ri (rm rmi but only one m)
  | Imul Operand Operand Operand  -- rm / r rmi / r rm i
  | ExtIdiv Operand  -- rm
  | Mov Operand Operand  -- r rmi / m ri (rm rmi but only one m)
  | Lea Operand Operand  -- r m
  deriving (Show)

isRegister :: Operand -> Bool
isRegister (Register _ _ _) = True
isRegister _ = False

isMemory :: Operand -> Bool
isMemory (Memory _ _ _ _ _) = True
isMemory _ = False

isImmediate :: Operand -> Bool
isImmediate (Immediate _) = True
isImmediate _ = False

getSize :: Operand -> Maybe Int
getSize (Register s _ _) = Just s
getSize (Memory s _ _ _ _) | s /= 0 = Just s
getSize _ = Nothing

assertSize :: MonadFail m => Int -> Operand -> m Operand
assertSize s r@(Register s' _ _) | s == s' = return r
assertSize s m@(Memory s' i sc b d)
  | s' == 0 = return $ Memory s i sc b d
  | s == s' = return m
assertSize _ i@(Immediate _) = return i
assertSize _ _ = fail "operand size mismatch"

eax :: Int -> Operand
eax 1 = Register 1 "eax" "al"
eax 2 = Register 2 "eax" "ax"
eax 4 = Register 4 "eax" "eax"

edx :: Int -> Operand
edx 1 = Register 1 "edx" "dl"
edx 2 = Register 2 "edx" "dx"
edx 4 = Register 4 "edx" "edx"