{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Assembler where

import Types

import Data.ByteString.Builder
import Control.Monad.Trans.RWS.CPS
import Control.Monad

splitFunctions :: [Instruction] -> [(String, [Instruction])]
splitFunctions instr = let (_, acc, res) = foldr f (0, [], []) instr in if null acc then res else ("main", acc) : res
  where
    f (Label new) (n, acc, res) = (n, [], (new, acc) : res)
    f cur (n, acc, res) = (n, cur : acc, res)

type Env = RWS () Builder ()

infixr 6 <+>
(<+>) :: Builder -> Builder -> Builder
a <+> b = a <> " " <> b

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust ma f = maybe (return ()) f ma

tellNL :: Builder -> Env ()
tellNL = tell . (<> "\n")

calculateAddress :: Operand -> Env ()
calculateAddress (Memory size index scale base displacement) = do
  tellNL $ "scoreboard players set index memory 0"
  whenJust index $ \r -> do
    r' <- getScore r True
    tellNL $ "scoreboard players operation index memory +=" <+> r'
    when (scale > 1) $ do
      tellNL $ "scoreboard players set imm memory" <+> intDec scale
      tellNL $ "scoreboard players operation index memory *= imm memory"
  whenJust base $ \r -> getScore r True >>= tellNL . ("scoreboard players operation index memory +=" <+>)
  when (displacement /= 0) $ tellNL $ "scoreboard players" <+> (if displacement > 0 then "add" else "remove") <+> "index memory" <+> intDec (abs displacement)
calculateAddress _ = error "invalid argument to calculateAddress"

getScore :: Operand -> Bool -> Env Builder
getScore (Register size realName name) readonly
  | size == 4 = return $ name <+> "registers"
  | otherwise = do
    tellNL $ "scoreboard players operation" <+> name <+> "registers =" <+> realName <+> "registers"
    tellNL $ "scoreboard players operation" <+> name <+> "registers %=" <+> intDec size <> "B constants"
    when (not readonly) $ tellNL $ "scoreboard players operation" <+> realName <+> "registers -=" <+> name <+> "registers"
    return $ name <+> "registers"
getScore m@(Memory size _ _ _ _) readonly = do
  calculateAddress m
  tellNL $ "scoreboard players set size memory" <+> intDec size
  tellNL $ "scoreboard players set readonly memory" <+> intDec (fromEnum readonly)
  tellNL $ "function assembler:library/unzip"
  return $ "mem registers"
getScore (Immediate i) _ = "imm registers" <$ tellNL ("scoreboard players set imm registers" <+> intDec i)

cleanup :: Operand -> Env ()
cleanup (Register size realName name) | size /= 4 = do
      tellNL $ "scoreboard players operation" <+> name <+> "registers %=" <+> intDec size <> "B constants"
      tellNL $ "scoreboard players operation" <+> realName <+> "registers +=" <+> name <+> "registers"
cleanup (Memory size index scale base displacement) = tellNL $ "function assembler:library/zip"
cleanup _ = return ()

signExtend :: Operand -> Env ()
signExtend op@(getSize -> Just size) | size /= 4 = let
    name = case op of 
      (Register _ _ name') -> name'
      _ -> "mem"
    bits = (2 ^ (size * 8))
  in tellNL $ "execute if score" <+> name <+> "registers matches" <+> intDec (bits `div` 2) <> ".. run scoreboard players remove" <+> name <+> "registers" <+> intDec bits
signExtend _ = fail "cannot sign extend operand"

processInstruction :: Instruction -> Env ()
processInstruction (Add op1 op2) = do
  sc1 <- getScore op1 False
  sc2 <- getScore op2 True
  tellNL $ "scoreboard players operation" <+> sc1 <+> "+=" <+> sc2
  cleanup op1
processInstruction (Sub op1 op2) = do
  sc1 <- getScore op1 False
  sc2 <- getScore op2 True
  tellNL $ "scoreboard players operation" <+> sc1 <+> "-=" <+> sc2
  cleanup op1
processInstruction (Imul op1 op2 op3) = do
  sc1 <- getScore op1 False
  sc2 <- getScore op2 True
  sc3 <- getScore op3 True
  tellNL $ "scoreboard players operation" <+> sc1 <+> "=" <+> sc2
  tellNL $ "scoreboard players operation" <+> sc1 <+> "*=" <+> sc3
  cleanup op1
processInstruction (ExtIdiv op1) = do
  sc1 <- getScore op1 True
  let Just size = getSize op1
  eax' <- getScore (eax size) False
  edx' <- getScore (edx size) False
  tellNL $ "scoreboard players operation" <+> edx' <+> "=" <+> eax'
  tellNL $ "scoreboard players operation" <+> eax' <+> "/=" <+> sc1
  tellNL $ "scoreboard players operation" <+> edx' <+> "%=" <+> sc1
  cleanup $ eax size
  cleanup $ edx size
processInstruction (Mov ext op1 op2) = do
  sc1 <- getScore op1 False
  sc2 <- getScore op2 True
  when ext $ signExtend op2
  tellNL $ "scoreboard players operation" <+> sc1 <+> "=" <+> sc2
  cleanup op1
processInstruction (Lea op1 op2) = do
  sc1 <- getScore op1 False
  calculateAddress op2
  tellNL $ "scoreboard players operation" <+> sc1 <+> "= index memory"
  cleanup op1
processInstruction _ = error "Invalid instruction"
