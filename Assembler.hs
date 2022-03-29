{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Assembler where

import Types

import Data.ByteString.Builder
import Control.Monad.Trans.RWS.CPS
import Control.Monad

splitFunctions :: [Instruction] -> [(String, [Instruction])]
splitFunctions instr = let (_, acc, res) = foldr f (0, [], []) instr in ("main", acc) : res
  where
    f (Label new) (n, acc, res) = (n, [Jmp new], (new, acc) : res)
    f Ret (n, _, res) = (n, [Ret], res)
    f j@(Jmp _) (n, _, res) = (n, [j], res)
    f (Jcc trueLabel _ cond) (n, acc, res) = let falseLabel = "tmp_" ++ show n in (n + 1, [Jcc trueLabel falseLabel cond], (falseLabel, acc) : res)
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

testDisjoint :: Builder -> Builder -> Env ()
testDisjoint sc1 sc2 = do
  tellNL $ "scoreboard players operation op1 variables =" <+> sc1
  tellNL $ "scoreboard players operation op2 variables =" <+> sc2
  tellNL $ "funtion assembler:library/disjoint"

getConditional :: Conditional -> Env [Builder]
getConditional (Conditional op1 op2 comp) = do
  sc1 <- getScore op1 True
  sc1 <- if isMemory op1 then "mem variables" <$ tellNL "scoreboard players operation mem variables = mem registers" else return sc1
  sc2 <- getScore op2 True
  sc2 <- if isMemory op2 then "mem variables" <$ tellNL "scoreboard players operation mem variables = mem registers" else return sc2
  getComparison comp sc1 sc2

getComparison :: Comparison -> Builder -> Builder -> Env [Builder]
getComparison (Cmp E) sc1 sc2 = return $ ["execute if score" <+> sc1 <+> "=" <+> sc2]
getComparison (Cmp NE) sc1 sc2 = return $ ["execute unless score" <+> sc1 <+> "=" <+> sc2]
getComparison (Cmp G) sc1 sc2 = return $ ["execute if score" <+> sc1 <+> ">" <+> sc2]
getComparison (Cmp GE) sc1 sc2 = return $ ["execute if score" <+> sc1 <+> ">=" <+> sc2]
getComparison (Cmp L) sc1 sc2 = return $ ["execute if score" <+> sc1 <+> "<" <+> sc2]
getComparison (Cmp LE) sc1 sc2 = return $ ["execute if score" <+> sc1 <+> "<=" <+> sc2]
getComparison (Test E) sc1 sc2 = ["execute if score disjoint variables matches 1"] <$ testDisjoint sc1 sc2
getComparison (Test NE) sc1 sc2 = ["execute if score disjoint variables matches 0"] <$ testDisjoint sc1 sc2
getComparison (Test G) sc1 sc2 = ["execute if score disjoint variables matches 0 if score" <+> sc1 <+> "matches 0..", "execute if score disjoint variables matches 0 if score" <+> sc1 <+> "matches ..-1 if score" <+> sc2 <+> "matches 0.."] <$ testDisjoint sc1 sc2
getComparison (Test GE) sc1 sc2 = return ["execute if score" <+> sc1 <+> "matches 0..", "execute if score" <+> sc1 <+> "matches ..-1 if score" <+> sc2 <+> "matches 0.."]
getComparison (Test L) sc1 sc2 = return ["execute if score" <+> sc1 <+> "matches ..-1 if score" <+> sc2 <+> "matches ..-1"]
getComparison (Test LE) sc1 sc2 = ["execute if score disjoint variables matches 1", "execute if score disjoint variables matches 0 if score" <+> sc1 <+> "matches ..-1 if score" <+> sc2 <+> "matches ..-1"] <$ testDisjoint sc1 sc2

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
processInstruction (Call label) = do
  tellNL $ "scoreboard players remove esp registers 4"
  tellNL $ "function assembler:" <> stringUtf8 label
processInstruction (Jmp label) = tellNL $ "function assembler:" <> stringUtf8 label
processInstruction (Jcc trueLabel falseLabel cond) = do
  conds <- getConditional cond
  tellNL $ "scoreboard players set condition variables 0"
  tellNL $ "data modify storage assembler:memory conditions prepend value 0"
  traverse (tellNL . (<+> "store result storage assembler:memory conditions[0] int 1 run scoreboard players set condition variables 1")) conds
  tellNL $ "execute if score condition variables matches 1 run function assembler:" <> stringUtf8 trueLabel
  tellNL $ "execute store result score condition variables run data get storage assembler:memory conditions[0]"
  tellNL $ "data remove storage assembler:memory conditions[0]"
  tellNL $ "execute if score condition variables matches 0 run function assembler:" <> stringUtf8 falseLabel
processInstruction (Cmovcc op1 op2 cond) = do
  conds <- getConditional cond
  sc1 <- getScore op1 False
  sc2 <- getScore op2 True
  traverse (tellNL . (<+> "run scoreboard players operation" <+> sc1 <+> "=" <+> sc2)) conds
  cleanup op1
processInstruction (Setcc op cond) = do
  conds <- getConditional cond
  tellNL $ "scoreboard players set bool variables 0"
  traverse (tellNL . (<+> "run scoreboard players set bool variables 1")) conds
  sc <- getScore op False
  tellNL $ "scoreboard players operation" <+> sc <+> "= bool variables"
  cleanup op
processInstruction Ret = tellNL $ "scoreboard players add esp registers 4"
processInstruction (Push op) = do
  sc2 <- getScore op True
  sc2 <- if isMemory op then "mem2 registers" <$ tellNL "scoreboard players operation mem2 registers = mem registers" else return sc2
  let size = maybe 4 id (getSize op)
  sc1 <- getScore (espMinus size) False
  tellNL $ "scoreboard players operation" <+> sc1 <+> "=" <+> sc2
  cleanup (espMinus size)
  tellNL $ "scoreboard players remove esp registers" <+> intDec size
processInstruction (Pop op) = do
  let size = maybe 4 id (getSize op)
  tellNL $ "scoreboard players add esp registers" <+> intDec size
  sc2 <- getScore (espMinus size) True
  sc2 <- if isMemory op then "mem2 registers" <$ tellNL "scoreboard players operation mem2 registers = mem registers" else return sc2
  sc1 <- getScore op True
  tellNL $ "scoreboard players operation" <+> sc1 <+> "=" <+> sc2
  cleanup op
processInstruction Leave = do
  tellNL $ "scoreboard players operation esp registers = ebp registers"
  processInstruction (Pop (Register 4 "ebp" "ebp"))
processInstruction (Label _) = error "Invalid instruction"
