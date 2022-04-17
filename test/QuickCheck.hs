{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}

module QuickCheck where

import Types
import Parser.Base
import Parser.Intel

import Text.Parsec hiding ((<|>), some, many, optional, label)
import Data.ByteString.Builder
import Data.Maybe
import Data.Char

import Test.QuickCheck hiding (getSize)

genRegister :: [Int] -> Gen Operand
genRegister sizes = do
  size <- elements sizes
  name <- elements ["eax", "ebx", "ecx", "edx", "edi", "esi", "esp", "ebp"]
  pure $ Register size (sizedRegister size name)

genImmediate :: Gen Operand
genImmediate = Immediate <$> arbitrary

genMemory :: [Int] -> Gen Operand
genMemory sizes = do
    size <- elements sizes
    indexScale <- liftArbitrary $ liftArbitrary2 (genRegister [4]) (elements [1, 2, 4, 8])
    base <- liftArbitrary (genRegister [4])
    disp <- arbitrary
    pure $ maybe (Memory size Nothing 1 base disp) (\(index, scale) -> if scale == 1 && isNothing base then Memory size Nothing 1 (Just index) disp else Memory size (Just index) scale base disp) indexScale
  `suchThat` (\(Memory _ index _ base disp) -> isJust index || isJust base || disp /= 0)

genOperand :: [Int] -> Gen Operand
genOperand sizes = oneof [genRegister $ filter (/= 0) sizes, genMemory sizes, genImmediate]

instance Arbitrary Operand where
  arbitrary = genOperand [0, 1, 2, 4]

testOperand :: Operand -> Property
testOperand op = either (\pe -> counterexample (show pe) False) (=== op) $ parse parseOperand "" (toLazyByteString $ showOperand op)

showOperand :: Operand -> Builder
showOperand r@(Register _ name) = stringUtf8 name
showOperand i@(Immediate val) = intDec val
showOperand m@(Memory size index scale base disp) = showSize <> "[" <> showDisp <> showBase <> showIndex <> showScale <> "]"
  where
    showSize = case size of
      0 -> ""
      1 -> "BYTE PTR "
      2 -> "WORD PTR "
      4 -> "DWORD PTR "
    showDisp = if disp == 0 then mempty else intDec disp
    showBase = foldMap (mappend "+" . showOperand) base
    showIndex = foldMap (mappend "+" . showOperand) index
    showScale = if scale == 1 then mempty else "*" <> intDec scale

genrmrmi :: (Operand -> Operand -> a) -> Gen a
genrmrmi f = do
  size <- elements [1, 2, 4]
  frequency [(3, f <$> genRegister [size] <*> genOperand [size]), (2, f <$> genMemory [size] <*> oneof [genRegister [size], genImmediate])]

genAdd :: Gen Instruction
genAdd = genrmrmi Add

genSub :: Gen Instruction
genSub = genrmrmi Sub

genImul :: Gen Instruction
genImul = do
  size <- elements [2, 4]
  frequency [(1, Imul (eax size) (eax size) <$> oneof [genRegister [size], genMemory [size]]), (3, (\r -> Imul r r) <$> genRegister [size] <*> genOperand [size]), (2, Imul <$> genRegister [size] <*> oneof [genRegister [size], genMemory [size]] <*> genImmediate)]

genExtIdiv :: Gen Instruction
genExtIdiv = ExtIdiv <$> oneof [genRegister [1, 2, 4], genMemory [1, 2, 4]]

genMov :: Gen Instruction
genMov = frequency [(1, genrmrmi (Mov False)), (2, do
    ext <- arbitrary
    (s1, s2) <- elements [(4, 2), (4, 1), (2, 1)]
    Mov ext <$> genRegister [s1] <*> oneof [genRegister [s2], genMemory [s2]]
  )]

genLea :: Gen Instruction
genLea = Lea <$> genRegister [2, 4] <*> genMemory [1, 2, 4]

genLabelName :: Gen String
genLabelName = do
  let lowerNum = [(26, chooseEnum ('a', 'z')), (10, chooseEnum ('0', '9'))]
  first <- frequency lowerNum
  rest <- listOf $ frequency $ (2, elements "._") : lowerNum
  pure $ first : rest

genLabel :: Gen Instruction
genLabel = Label <$> genLabelName

genRet :: Gen Instruction
genRet = pure Ret

genCall :: Gen Instruction
genCall = Call <$> genLabelName

genJmp :: Gen Instruction
genJmp = Jmp <$> genLabelName

genConditional :: Gen Conditional
genConditional = genrmrmi Conditional <*> elements [Cmp, Test] <*> arbitraryBoundedEnum

genJcc :: Gen Instruction
genJcc = Jcc <$> genLabelName <*> pure "" <*> genConditional

genCmovcc :: Gen Instruction
genCmovcc = do
  size <- elements [2, 4]
  Cmovcc <$> genRegister [size] <*> oneof [genRegister [size], genMemory [size]] <*> genConditional

genSetcc :: Gen Instruction
genSetcc = Setcc <$> genRegister [1] <*> genConditional

genPush :: Gen Instruction
genPush = Push <$> genOperand [2, 4]

genPop :: Gen Instruction
genPop = Pop <$> oneof [genRegister [2, 4], genMemory [2, 4]]

genLeave :: Gen Instruction
genLeave = pure Leave

instance Arbitrary Instruction where
  arbitrary = oneof [genAdd, genSub, genImul, genExtIdiv, genMov, genLea, genLabel, genRet, genCall, genJmp, genJcc, genCmovcc, genSetcc, genPush, genPop, genLeave]

testInstruction :: Instruction -> Property
testInstruction ins = either (\pe -> counterexample (show pe) False) (=== ins) $ parse parseInstruction "" (toLazyByteString $ showInstruction ins)

showInstruction :: Instruction -> Builder
showInstruction (Add op1 op2) = "add " <> showOperand op1 <> ", " <> showOperand op2
showInstruction (Sub op1 op2) = "sub " <> showOperand op1 <> ", " <> showOperand op2
showInstruction (Imul op1 op2 op3) = "imul " <> (if op1 /= op2 then showOperand op1 <> ", " else "") <> (case op2 of Register size name | name == sizedRegister size "eax" && not (isImmediate op3) -> ""; _ -> showOperand op2 <> ", ") <> showOperand op3
showInstruction (ExtIdiv op1) = (case fromJust $ getSize op1 of 1 -> "cbw"; 2 -> "cwd"; 4 -> "cdq") <> "\nidiv " <> showOperand op1
showInstruction (Mov ext op1 op2) = "mov" <> (if fromMaybe False ((>) <$> getSize op1 <*> getSize op2) then (if ext then "sx " else "zx ") else " ") <> showOperand op1 <> ", " <> showOperand op2
showInstruction (Lea op1 op2) = "lea " <> showOperand op1 <> ", " <> showOperand op2
showInstruction (Label label) = stringUtf8 label <> ":"
showInstruction Ret = "ret"
showInstruction (Call label) = "call " <> stringUtf8 label
showInstruction (Jmp label) = "jmp " <> stringUtf8 label
showInstruction (Jcc trueLabel _ cond) = showConditional "j" cond <> stringUtf8 trueLabel
showInstruction (Cmovcc op1 op2 cond) = showConditional "cmov" cond <> showOperand op1 <> ", " <> showOperand op2
showInstruction (Setcc op1 cond) = showConditional "set" cond <> showOperand op1
showInstruction (Push op1) = "push " <> showOperand op1
showInstruction (Pop op1) = "pop " <> showOperand op1
showInstruction Leave = "leave"

showConditional :: Builder -> Conditional -> Builder
showConditional ins (Conditional op1 op2 comp cond) = (stringUtf8 $ map toLower $ show comp) <> " " <> showOperand op1 <> ", " <> showOperand op2 <> "\n" <> ins <> (stringUtf8 $ map toLower $ show cond) <> " "

runTests :: Int -> IO ()
runTests cnt = mappend <$> quickCheck (withMaxSuccess cnt testOperand) <*> quickCheck (withMaxSuccess cnt testInstruction)