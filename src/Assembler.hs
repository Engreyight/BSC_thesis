{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Assembler where

import Types

import Data.ByteString.Builder
import Control.Monad.Trans.Writer.CPS
import Control.Monad

splitFunctions :: String -> [Instruction] -> [(String, [Instruction])]
splitFunctions mainName instr = let (_, acc, res) = foldr f (0, [], []) instr in (mainName, acc) : res
  where
    f (Label new) (n, acc, res) = (n, [Jmp new], (new, acc) : res)
    f Ret (n, _, res) = (n, [Ret], res)
    f j@(Jmp _) (n, _, res) = (n, [j], res)
    f (Jcc trueLabel _ cond) (n, acc, res) = let falseLabel = mainName ++ "_tmp_" ++ show n in (n + 1, [Jcc trueLabel falseLabel cond], (falseLabel, acc) : res)
    f cur (n, acc, res) = (n, cur : acc, res)

type Env = Writer Builder

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
    r' <- getScore r True True
    tellNL $ "scoreboard players operation index memory +=" <+> r'
    when (scale > 1) $ do
      tellNL $ "scoreboard players set imm memory" <+> intDec scale
      tellNL $ "scoreboard players operation index memory *= imm memory"
  whenJust base $ \r -> getScore r True True >>= tellNL . ("scoreboard players operation index memory +=" <+>)
  when (displacement /= 0) $ tellNL $ "scoreboard players" <+> (if displacement > 0 then "add" else "remove") <+> "index memory" <+> intDec (abs displacement)
calculateAddress _ = error "invalid argument to calculateAddress"

getScore :: Operand -> Bool -> Bool -> Env Builder
getScore (Register size name) readonly extend
  | size == 4 = return $ stringUtf8 name <+> "registers"
  | otherwise = do
    let realsc = (stringUtf8 $ realName size name) <+> "registers"
    let sc = stringUtf8 name <+> "registers"
    tellNL $ "scoreboard players operation" <+> sc <+> "=" <+> realsc
    tellNL $ "scoreboard players operation" <+> sc <+> "%=" <+> intDec size <> "B constants"
    when (not readonly) $ tellNL $ "scoreboard players operation" <+> realsc <+> "-=" <+> sc
    when extend $ signExtend size sc
    return sc
getScore m@(Memory size _ _ _ _) readonly extend = do
  calculateAddress m
  tellNL $ "scoreboard players set size memory" <+> intDec size
  tellNL $ "scoreboard players set readonly memory" <+> intDec (fromEnum readonly)
  tellNL $ "function assembler:library/unzip"
  when (extend && size < 4) $ signExtend size "mem registers"
  return "mem registers"
getScore (Immediate i) _ _ = "imm registers" <$ tellNL ("scoreboard players set imm registers" <+> intDec i)

cleanup :: Operand -> Env ()
cleanup (Register size name) | size /= 4 = do
  let realName' = stringUtf8 $ realName size name
  let name' = stringUtf8 name
  tellNL $ "scoreboard players operation" <+> name' <+> "registers %=" <+> intDec size <> "B constants"
  tellNL $ "scoreboard players operation" <+> realName' <+> "registers +=" <+> name' <+> "registers"
cleanup (Memory size index scale base displacement) = tellNL $ "function assembler:library/zip"
cleanup _ = return ()

signExtend :: Int -> Builder -> Env()
signExtend size sc = do
  tellNL $ "scoreboard players operation" <+> sc <+> "*=" <+> intDec (4 - size) <> "B constants"
  tellNL $ "scoreboard players operation" <+> sc <+> "/=" <+> intDec (4 - size) <> "B constants"

testDisjoint :: Builder -> Builder -> Env ()
testDisjoint sc1 sc2 = do
  tellNL $ "scoreboard players operation op1 variables =" <+> sc1
  tellNL $ "scoreboard players operation op2 variables =" <+> sc2
  tellNL $ "function assembler:library/disjoint"

getConditional :: Conditional -> Env [Builder]
getConditional (Conditional op1 op2 comp cond) = do
  sc1 <- getScore op1 True True
  sc1 <- if isMemory op1 then "mem variables" <$ tellNL "scoreboard players operation mem variables = mem registers" else return sc1
  sc2 <- getScore op2 True True
  sc2 <- if isMemory op2 then "mem variables" <$ tellNL "scoreboard players operation mem variables = mem registers" else return sc2
  getComparison comp cond sc1 sc2

getComparison :: Comparison -> Condition -> Builder -> Builder -> Env [Builder]
getComparison Cmp E sc1 sc2 = return $ ["execute if score" <+> sc1 <+> "=" <+> sc2]
getComparison Cmp NE sc1 sc2 = return $ ["execute unless score" <+> sc1 <+> "=" <+> sc2]
getComparison Cmp G sc1 sc2 = return $ ["execute if score" <+> sc1 <+> ">" <+> sc2]
getComparison Cmp GE sc1 sc2 = return $ ["execute if score" <+> sc1 <+> ">=" <+> sc2]
getComparison Cmp L sc1 sc2 = return $ ["execute if score" <+> sc1 <+> "<" <+> sc2]
getComparison Cmp LE sc1 sc2 = return $ ["execute if score" <+> sc1 <+> "<=" <+> sc2]
getComparison Test E sc1 sc2 = ["execute if score disjoint variables matches 1"] <$ testDisjoint sc1 sc2
getComparison Test NE sc1 sc2 = ["execute if score disjoint variables matches 0"] <$ testDisjoint sc1 sc2
getComparison Test G sc1 sc2 = ["execute if score disjoint variables matches 0 if score" <+> sc1 <+> "matches 0..", "execute if score disjoint variables matches 0 if score" <+> sc1 <+> "matches ..-1 if score" <+> sc2 <+> "matches 0.."] <$ testDisjoint sc1 sc2
getComparison Test GE sc1 sc2 = return ["execute if score" <+> sc1 <+> "matches 0..", "execute if score" <+> sc1 <+> "matches ..-1 if score" <+> sc2 <+> "matches 0.."]
getComparison Test L sc1 sc2 = return ["execute if score" <+> sc1 <+> "matches ..-1 if score" <+> sc2 <+> "matches ..-1"]
getComparison Test LE sc1 sc2 = ["execute if score disjoint variables matches 1", "execute if score disjoint variables matches 0 if score" <+> sc1 <+> "matches ..-1 if score" <+> sc2 <+> "matches ..-1"] <$ testDisjoint sc1 sc2

processInstruction :: Builder -> Instruction-> Env ()
processInstruction _ (Add op1 op2) = do
  sc2 <- getScore op2 True True
  sc1 <- getScore op1 False True
  tellNL $ "scoreboard players operation" <+> sc1 <+> "+=" <+> sc2
  cleanup op1
processInstruction _ (Sub op1 op2) = do
  sc2 <- getScore op2 True True
  sc1 <- getScore op1 False True
  tellNL $ "scoreboard players operation" <+> sc1 <+> "-=" <+> sc2
  cleanup op1
processInstruction _ (Imul op1 op2 op3) = do
  sc2 <- getScore op2 True True
  sc3 <- getScore op3 True True
  sc1 <- getScore op1 False False
  when (op1 /= op2) $ tellNL $ "scoreboard players operation" <+> sc1 <+> "=" <+> sc2
  tellNL $ "scoreboard players operation" <+> sc1 <+> "*=" <+> sc3
  cleanup op1
processInstruction _ (ExtIdiv op1) = do
  sc1 <- getScore op1 True True
  let Just size = getSize op1
  eax' <- if (size == 1) then do
    sc <- getScore (eax 2) False False
    tellNL $ "scoreboard players operation" <+> sc <+> "%= 1B constants"
    sc <$ signExtend 1 sc
  else getScore (eax size) False True
  modReg <- if size == 1 then return "mod registers" else getScore (edx size) False False
  tellNL $ "scoreboard players operation" <+> modReg <+> "=" <+> eax'
  tellNL $ "scoreboard players operation" <+> eax' <+> "/=" <+> sc1
  tellNL $ "scoreboard players operation" <+> modReg <+> "%=" <+> sc1
  tellNL $ "execute if score" <+> eax' <+> "matches ..-1 unless score" <+> modReg <+> "matches 0 run scoreboard players add" <+> eax' <+> "1"
  tellNL $ "execute if score" <+> eax' <+> "matches ..-1 unless score" <+> modReg <+> "matches 0 run scoreboard players operation" <+> modReg <+> "-=" <+> sc1
  if size == 1 then do
    tellNL $ "scoreboard players operation" <+> eax' <+> "%= 1B constants"
    tellNL $ "scoreboard players operation" <+> modReg <+> "*= 1B constants"
    tellNL $ "scoreboard players operation" <+> eax' <+> "+=" <+> modReg
    cleanup $ eax 2
  else do
    cleanup $ edx size
    cleanup $ eax size
processInstruction _ (Mov ext op1 op2) = do
  sc2 <- getScore op2 True ext
  sc1 <- getScore op1 False False
  tellNL $ "scoreboard players operation" <+> sc1 <+> "=" <+> sc2
  cleanup op1
processInstruction _ (Lea op1 op2) = do
  sc1 <- getScore op1 False False
  calculateAddress op2
  tellNL $ "scoreboard players operation" <+> sc1 <+> "= index memory"
  cleanup op1
processInstruction prefix (Call label) = do
  tellNL $ "scoreboard players remove esp registers 4"
  tellNL $ "function" <+> prefix <> stringUtf8 label
processInstruction prefix (Jmp label) = tellNL $ "function" <+> prefix <> stringUtf8 label
processInstruction prefix (Jcc trueLabel falseLabel cond) = do
  conds <- getConditional cond
  tellNL $ "scoreboard players set condition variables 0"
  tellNL $ "data modify storage assembler:memory conditions prepend value 0"
  traverse (tellNL . (<+> "store result storage assembler:memory conditions[0] int 1 run scoreboard players set condition variables 1")) conds
  tellNL $ "execute if score condition variables matches 1 run function" <+> prefix <> stringUtf8 trueLabel
  tellNL $ "execute store result score condition variables run data get storage assembler:memory conditions[0]"
  tellNL $ "data remove storage assembler:memory conditions[0]"
  tellNL $ "execute if score condition variables matches 0 run function" <+> prefix <> stringUtf8 falseLabel
processInstruction _ (Cmovcc op1 op2 cond) = do
  conds <- getConditional cond
  sc2 <- getScore op2 True True
  sc1 <- getScore op1 False False
  traverse (tellNL . (<+> "run scoreboard players operation" <+> sc1 <+> "=" <+> sc2)) conds
  cleanup op1
processInstruction _ (Setcc op cond) = do
  conds <- getConditional cond
  tellNL $ "scoreboard players set bool variables 0"
  traverse (tellNL . (<+> "run scoreboard players set bool variables 1")) conds
  sc <- getScore op False False
  tellNL $ "scoreboard players operation" <+> sc <+> "= bool variables"
  cleanup op
processInstruction _ Ret = tellNL $ "scoreboard players add esp registers 4"
processInstruction _ (Push op) = do
  sc2 <- getScore op True True
  sc2 <- if isMemory op then "mem2 registers" <$ tellNL "scoreboard players operation mem2 registers = mem registers" else return sc2
  let size = maybe 4 id (getSize op)
  sc1 <- getScore (espMinus size) False False
  tellNL $ "scoreboard players operation" <+> sc1 <+> "=" <+> sc2
  cleanup (espMinus size)
  tellNL $ "scoreboard players remove esp registers" <+> intDec size
processInstruction _ (Pop op) = do
  let size = maybe 4 id (getSize op)
  tellNL $ "scoreboard players add esp registers" <+> intDec size
  sc2 <- getScore (espMinus size) True True
  sc2 <- if isMemory op then "mem2 registers" <$ tellNL "scoreboard players operation mem2 registers = mem registers" else return sc2
  sc1 <- getScore op False False
  tellNL $ "scoreboard players operation" <+> sc1 <+> "=" <+> sc2
  cleanup op
processInstruction _p Leave = do
  tellNL $ "scoreboard players operation esp registers = ebp registers"
  processInstruction _p (Pop (Register 4 "ebp"))
processInstruction _ (Label _) = error "Invalid instruction"

