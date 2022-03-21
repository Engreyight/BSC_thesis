{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Assembler where

import Types
import Builder

import Data.ByteString.Builder
import Control.Monad.Trans.RWS.CPS
import Control.Monad

type Env = RWS () Builder ()

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust ma f = maybe (return ()) f ma

calculateAddress :: Operand -> Env ()
calculateAddress (Memory size index scale base displacement) = do
  tell $ scoreboardSet ("index", "memory") 0
  whenJust index $ \r -> do
    r' <- getScore r True
    tell $ scoreboardOperation ("index", "memory") "+=" r'
    when (scale > 1) $ do
      tell $ scoreboardSet ("imm", "memory") scale
      tell $ scoreboardOperation ("index", "memory") "*=" ("imm", "memory")
  whenJust base $ \r -> getScore r True >>= tell . scoreboardOperation ("index", "memory") "+="
  when (displacement /= 0) $ tell $ "scoreboard players" <+> (if displacement > 0 then "add" else "remove") <+> "index memory" <+> intDec (abs displacement) <> "\n"
calculateAddress _ = error "invalid argument to calculateAddress"

getScore :: Operand -> Bool -> Env (Builder, Builder)
getScore (Register size realName name) _
  | size == 4 = return (name, "registers")
  | otherwise = do
    tell $ scoreboardOperation (name, "registers") "=" (realName, "registers")
    tell $ scoreboardOperation (name, "registers") "%=" (intDec size <> "B", "constants")
    tell $ scoreboardOperation (realName, "registers") "-=" (name, "registers")
    return (name, "registers")
getScore m@(Memory size _ _ _ _) readonly = do
  calculateAddress m
  tell $ scoreboardSet ("size", "memory") size
  tell $ scoreboardSet ("readonly", "memory") (fromEnum readonly)
  tell $ "function assembler:library/unzip" <> "\n"
  return ("mem", "registers")
getScore (Immediate i) _ = ("imm", "registers") <$ tell (scoreboardSet ("imm", "registers") i)

cleanup :: Operand -> Env ()
cleanup (Register size realName name) | size /= 4 = do
      tell $ scoreboardOperation (name, "registers") "%=" (intDec size <> "B", "constants")
      tell $ scoreboardOperation (realName, "registers") "+=" (name, "registers")
cleanup (Memory size index scale base displacement) = tell $ "function assembler:library/zip" <> "\n"
cleanup _ = return ()

signExtend :: Operand -> Env ()
signExtend op@(getSize -> Just size) | size /= 4 = let name = (case op of (Register _ _ name') -> name'; _ -> "mem"); bits = (2 ^ (size * 8)) in tell $ "execute if score" <+> name <+> "registers matches" <+> intDec (bits `div` 2) <> ".. run scoreboard players remove" <+> name <+> "registers" <+> intDec bits <> "\n"
signExtend _ = fail "cannot sign extend operand"

processInstruction :: Instruction -> Env ()
processInstruction (Add op1 op2) = do
  sc1 <- getScore op1 False
  sc2 <- getScore op2 True
  tell $ scoreboardOperation sc1 "+=" sc2
  cleanup op1
processInstruction (Sub op1 op2) = do
  sc1 <- getScore op1 False
  sc2 <- getScore op2 True
  tell $ scoreboardOperation sc1 "-=" sc2
  cleanup op1
processInstruction (Imul op1 op2 op3) = do
  sc1 <- getScore op1 False
  sc2 <- getScore op2 True
  sc3 <- getScore op3 True
  tell $ scoreboardOperation sc1 "=" sc2
  tell $ scoreboardOperation sc1 "*=" sc3
  cleanup op1
processInstruction (ExtIdiv op1) = do
  sc1 <- getScore op1 True
  let Just size = getSize op1
  eax' <- getScore (eax size) False
  edx' <- getScore (edx size) False
  tell $ scoreboardOperation edx' "=" eax'
  tell $ scoreboardOperation eax' "/=" sc1
  tell $ scoreboardOperation edx' "%=" sc1
  cleanup $ eax size
  cleanup $ edx size
processInstruction (Mov ext op1 op2) = do
  sc1 <- getScore op1 False
  sc2 <- getScore op2 True
  when ext $ signExtend op2
  tell $ scoreboardOperation sc1 "=" sc2
  cleanup op1
processInstruction (Lea op1 op2) = do
  sc1 <- getScore op1 False
  sc2 <- calculateAddress op2
  tell $ scoreboardOperation sc1 "=" ("mem", "registers")
  cleanup op1
  
