{-# LANGUAGE OverloadedStrings #-}
module Assembler where

import Types
import Builder

import Data.ByteString.Builder
import Control.Monad.Trans.RWS.CPS
import Control.Monad

type Env = RWS () Builder ()

getScore :: Operand -> Env (Builder, Builder)
getScore (Register _ name) = return ("registers", stringUtf8 name)
-- getScore (Memory) = error "Memory locations not currently implemented"
getScore (Immediate i) = tell (scoreboardSet i) *> return ("registers", "imm")

processInstruction :: Instruction -> Env ()
processInstruction (Add op1 op2) = do
  sc1 <- getScore op1
  sc2 <- getScore op2
  tell $ scoreboardOperation "+=" sc1 sc2
processInstruction (Sub op1 op2) = do
  sc1 <- getScore op1
  sc2 <- getScore op2
  tell $ scoreboardOperation "-=" sc1 sc2
processInstruction (Imul op1 op2 op3) = do
  sc1 <- getScore op1
  sc2 <- getScore op2
  sc3 <- getScore op3
  when (op1 /= op2) $ tell $ scoreboardOperation "=" sc1 sc2
  tell $ scoreboardOperation "*=" sc1 sc3
processInstruction (ExtIdiv op1) = do
  sc1 <- getScore op1
  eax <- getScore (Register 4 "eax")
  edx <- getScore (Register 4 "edx")
  tell $ scoreboardOperation "=" edx eax
  tell $ scoreboardOperation "/=" eax sc1
  tell $ scoreboardOperation "%=" edx sc1
processInstruction (Mov op1 op2) = do
  sc1 <- getScore op1
  sc2 <- getScore op2
  tell $ scoreboardOperation "=" sc1 sc2
