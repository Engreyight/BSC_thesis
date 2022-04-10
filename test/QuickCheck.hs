{-# LANGUAGE OverloadedStrings #-}

module QuickCheck where

import Types
import Parser.Base
import Parser.Intel

import Text.Parsec hiding ((<|>), some, many, optional)
import Data.ByteString.Char8 (ByteString, pack)
import Data.Maybe

import Test.QuickCheck

genRegister :: Gen Operand
genRegister = elements $ concat (zipWith (map . Register) [1, 2, 4] [["al", "bl", "cl", "dl", "dil", "sil", "spl", "bpl"], ["ax", "bx", "cx", "dx", "di", "si", "sp", "bp"], ["eax", "ebx", "ecx", "edx", "edi", "esi", "esp", "ebp"]])

genRegister4 :: Gen Operand
genRegister4 = Register 4 <$> elements ["eax", "ebx", "ecx", "edx", "edi", "esi", "esp", "ebp"]

genImmediate :: Gen Operand
genImmediate = Immediate <$> arbitrary

genMemory :: Gen Operand
genMemory = ((\a b c d -> maybe (Memory a Nothing 1 c d) (\(b', b'') -> if b'' == 1 && isNothing c then Memory a Nothing 1 (Just b') d else Memory a (Just b') b'' c d) b) <$> elements [0, 1, 2, 4] <*> liftArbitrary (liftArbitrary2 genRegister4 (elements [1, 2, 4, 8])) <*> liftArbitrary genRegister4 <*> arbitrary) `suchThat` (\(Memory _ index _ base disp) -> isJust index || isJust base || disp /= 0)

instance Arbitrary Operand where
  arbitrary = oneof [genRegister, genImmediate, genMemory]

testEqWithParser :: (Show a, Eq a) => Parser a -> a -> ByteString -> Property
testEqWithParser p v = either (\pe -> counterexample (show pe) False) (=== v) . parse p ""

testOperand :: Operand -> Property
testOperand r@(Register _ name) = testEqWithParser parseOperand r (pack name)
testOperand i@(Immediate val) = testEqWithParser parseOperand i (pack $ show val)
testOperand m@(Memory size index scale base disp) = testEqWithParser parseOperand m (pack $ showSize ++ "[" ++ showDisp ++ showBase ++ showIndex ++ "]")
  where
    showSize = case size of
      0 -> ""
      1 -> "BYTE PTR "
      2 -> "WORD PTR "
      4 -> "DWORD PTR "
    showDisp = if disp > 0 then '+' : show disp else if disp < 0 then show disp else ""
    showBase = maybe "" (\(Register _ name) -> '+' : name) base
    showIndex = maybe "" (\(Register _ name) -> '+' : name ++ if scale > 1 then '*' : show scale else "") index

runTests :: Int -> IO ()
runTests cnt = quickCheck $ withMaxSuccess cnt testOperand