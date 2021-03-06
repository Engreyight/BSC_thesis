{-# LANGUAGE OverloadedStrings #-}

module Types where

import Control.Monad.Fail (MonadFail)
import Data.ByteString.Builder (Builder, toLazyByteString)

data Operand = Register Int String | Memory Int (Maybe Operand) Int (Maybe Operand) Int | Immediate Int deriving (Show, Eq)
data Instruction = Add Operand Operand  -- r rmi / m ri (rm rmi but only one m)
  | Sub Operand Operand  -- r rmi / m ri (rm rmi but only one m)
  | Imul Operand Operand Operand  -- rm / r rmi / r rm i
  | ExtIdiv Operand  -- rm
  | Mov Bool Operand Operand  -- r rmi / m ri (rm rmi but only one m) / r rm for ext
  | Lea Operand Operand  -- r m
  | Label String
  | Ret
  | Call String
  | Jmp String
  | Jcc String String Conditional
  | Cmovcc Operand Operand Conditional  -- r16/32 rm16/32
  | Setcc Operand Conditional  -- rm8
  | Push Operand  -- rmi16/32
  | Pop Operand  -- rm16/32
  | Leave
  deriving (Show, Eq)

data Conditional = Conditional Operand Operand Comparison Condition deriving (Show, Eq)  -- r rmi / m ri (rm rmi but only one m)
data Comparison = Cmp | Test deriving (Show, Eq)
data Condition = E | NE | G | GE | L | LE deriving (Show, Eq, Bounded, Enum)

instance Show Builder where
    show = show . toLazyByteString

isRegister :: Operand -> Bool
isRegister (Register _ _) = True
isRegister _ = False

isMemory :: Operand -> Bool
isMemory (Memory _ _ _ _ _) = True
isMemory _ = False

isImmediate :: Operand -> Bool
isImmediate (Immediate _) = True
isImmediate _ = False

getSize :: Operand -> Maybe Int
getSize (Register s _) = Just s
getSize (Memory s _ _ _ _) | s /= 0 = Just s
getSize _ = Nothing

assertSize :: MonadFail m => Int -> Operand -> m Operand
assertSize s r@(Register s' _) | s == s' = return r
assertSize s m@(Memory s' i sc b d)
  | s' == 0 = return $ Memory s i sc b d
  | s == s' = return m
assertSize _ i@(Immediate _) = return i
assertSize _ _ = fail "operand size mismatch"

eax :: Int -> Operand
eax 1 = Register 1 "al"
eax 2 = Register 2 "ax"
eax 4 = Register 4 "eax"

edx :: Int -> Operand
edx 1 = Register 1 "dl"
edx 2 = Register 2 "dx"
edx 4 = Register 4 "edx"

espMinus :: Int -> Operand
espMinus n = Memory n Nothing 1 (Just (Register 4 "esp")) (-n)

realName :: Int -> String -> String
realName 2 name = 'e' : name
realName 1 [a, 'l'] = ['e', a, 'x']
realName 1 [a, b, 'l'] = ['e', a, b]

sizedRegister :: Int -> String -> String
sizedRegister 4 name = name
sizedRegister 2 ('e' : name) = name
sizedRegister 1 ['e', a, 'x'] = [a, 'l']
sizedRegister 1 ['e', a, b] = [a, b, 'l']