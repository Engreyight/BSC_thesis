module Parser.Intel where

import Types
import Parser.Base

import Text.Parsec hiding ((<|>), some, many, optional)
import Control.Applicative
import Control.Monad
import Parser.Permutations

parseOperand :: Parser Operand
parseOperand = parseRegister <|> parseMemory <|> parseImmediate

parseMemory :: Parser Operand
parseMemory = (parseNumber >>= \a -> option (Memory 4 Nothing 1 Nothing a) (bracesPart (pure (\b (c, d) -> Memory 4 c d b a))))<|> bracesPart ((\a b (c, d) -> Memory 4 c d b a) <$> dispPerm)
  where
    dispPerm :: Permutation Parser Int
    dispPerm = toPermutationWithDefault 0 (try (parseNumber <* notFollowedBy (char '*')))
    
    basePerm :: Permutation Parser (Maybe Operand)
    basePerm = toPermutationWithDefault Nothing (Just <$> try (parseRegister <* notFollowedBy (char '*')))
    
    indexScalePerm :: Permutation Parser (Maybe Operand, Int)
    indexScalePerm = toPermutationWithDefault (Nothing, 1) (intercalateEffect (char '*') $ (,) <$> indexPerm <*> scalePerm)
      where
        indexPerm :: Permutation Parser (Maybe Operand)
        indexPerm = toPermutation (Just <$> try parseRegister)
        
        scalePerm :: Permutation Parser Int
        scalePerm = toPermutationWithDefault 1 (mfilter (`elem` [1, 2, 4, 8]) (try parseNumber))
    
    bracesNonEmpty :: Parser a -> Parser a
    bracesNonEmpty p = do
      char '['
      pos <- getPosition
      res <- p
      newpos <- getPosition
      guard $ pos /= newpos
      char ']'
      return res

    bracesPart :: Permutation Parser (Maybe Operand -> (Maybe Operand, Int) -> Operand) -> Parser Operand
    bracesPart starterPerm = bracesNonEmpty $ intercalateEffect (lookAhead $ oneOf "+-") $ starterPerm <*> basePerm <*> indexScalePerm

rmrmi :: String -> (Operand -> Operand -> Instruction) -> Parser Instruction
rmrmi str instr = do
  string str
  spaces
  ops <- sepBy1 parseOperand comma
  case ops of
    [op1, op2]
      | isRegister op1
      || (isMemory op1 && not (isMemory op2))
      -> return $ instr op1 op2
    _ -> fail $ "Invalid operands for " ++ str

parseAdd :: Parser Instruction
parseAdd = rmrmi "add" Add

parseSub :: Parser Instruction
parseSub = rmrmi "sub" Sub

parseMov :: Parser Instruction
parseMov = rmrmi "mov" Mov

-- TODO: choose appropriate register size
parseImul :: Parser Instruction
parseImul = do
  string "imul"
  spaces
  ops <- sepBy1 parseOperand comma
  case ops of
    [op1]
      | isRegister op1 || isMemory op1
      -> return $ Imul eax eax op1
    [op1, op2]
      | isRegister op1
      -> return $ Imul op1 op1 op2
    [op1, op2, op3]
      | isRegister op1
      && isRegister op2 || isMemory op2
      && isImmediate op3
      -> return $ Imul op1 op2 op3
    _ -> fail "Invalid operands for imul"

parseExtIdiv :: Parser Instruction
parseExtIdiv = do
  string "cdq"  -- TODO: allow for other extends as well
  newline
  string "idiv"
  spaces
  ops <- sepBy1 parseOperand comma
  case ops of
    [op1]
      | isRegister op1 || isMemory op1
      -> return $ ExtIdiv op1
    _ -> fail "Invalid operand for idiv"
  