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
parseMemory = do
  size <- (4 <$ string "DWORD PTR ") <|> (2 <$ string "WORD PTR ") <|> (1 <$ string "BYTE PTR ") <|> return 0
  try (parseNumber >>= \a -> bracesPart (pure (\b (c, d) -> Memory size c d b a))) <|> bracesPart ((\a b (c, d) -> Memory size c d b a) <$> dispPerm)
  where
    dispPerm :: Permutation Parser Int
    dispPerm = toPermutationWithDefault 0 (try (parseNumber <* notFollowedBy (char '*')))
    
    basePerm :: Permutation Parser (Maybe Operand)
    basePerm = toPermutationWithDefault Nothing (Just <$> mfilter ((== Just 4) . getSize) (try (parseRegister <* notFollowedBy (char '*'))))
    
    indexScalePerm :: Permutation Parser (Maybe Operand, Int)
    indexScalePerm = toPermutationWithDefault (Nothing, 1) (intercalateEffect (char '*') $ (,) <$> indexPerm <*> scalePerm)
      where
        indexPerm :: Permutation Parser (Maybe Operand)
        indexPerm = toPermutation (Just <$> mfilter ((== Just 4) . getSize) (try parseRegister))
        
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
  size <- maybe (fail "ambigous operand sizes") return (foldr ((<|>) . getSize) Nothing ops)
  ops <- traverse (assertSize size) ops
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

parseImul :: Parser Instruction
parseImul = do
  string "imul"
  spaces
  ops <- sepBy1 parseOperand comma
  size <- maybe (fail "ambigous operand sizes") return (foldr ((<|>) . getSize) Nothing ops)
  ops <- traverse (assertSize size) ops
  case ops of
    [op1]
      | isRegister op1 || isMemory op1
      -> return $ Imul (eax size) (eax size) op1
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
  size <- (4 <$ try (string "cdq")) <|> (2 <$ try (string "cwd")) <|> (1 <$ try (string "cbw"))
  newline
  string "idiv"
  spaces
  ops <- sepBy1 parseOperand comma
  ops <- traverse (assertSize size) ops
  case ops of
    [op1]
      | isRegister op1 || isMemory op1
      -> return $ ExtIdiv op1
    _ -> fail "Invalid operand for idiv"

parseLea :: Parser Instruction
parseLea = do
  string "lea"
  spaces
  ops <- sepBy1 parseOperand comma
  case ops of
    [op1, op2]
      | isRegister op1 && isMemory op2
      -> return $ Lea op1 op2
    _ -> fail "Invalid operands for lea"

parseInstruction :: Parser Instruction
parseInstruction = choice [parseAdd, parseSub, parseMov, parseImul, parseExtIdiv, parseLea]