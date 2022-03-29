module Parser.Intel where

import Types
import Parser.Base

import Text.Parsec hiding ((<|>), some, many, optional)
import Control.Applicative
import Control.Monad
import Data.Char
import Parser.Permutations

parseOperand :: Parser Operand
parseOperand = (parseRegister <|> try parseMemory <|> parseImmediate) <* many ws

parseMemory :: Parser Operand
parseMemory = do
  size <- option 0 $ ((4 <$ string "DWORD") <|> (2 <$ string "WORD") <|> (1 <$ string "BYTE")) <* some ws <* string "PTR" <* some ws
  extraDisp <- option 0 parseNumber
  bracesNonEmpty $ intercalateEffect (lookAhead $ oneOf "+-") $ (\a b (c, d) -> Memory size c d b (a + extraDisp)) <$> dispPerm <*> basePerm <*> indexScalePerm
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

parseLabelName :: Parser String
parseLabelName = do
  name@(x : xs) <- some (satisfy (\c -> isAlphaNum c && isAscii c || c `elem` ['_', '.']))
  if (isLower x || isDigit x) && all (not . isUpper) xs then return name else fail "label name is not a valid function name"

parseLabel :: Parser Instruction
parseLabel = Label <$> parseLabelName <* char ':' <* many ws

rmrmi :: String -> (Operand -> Operand -> Instruction) -> Parser Instruction
rmrmi str instr = do
  string str
  some ws
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
parseMov = do
  string "mov"
  extra <- option Nothing $ (Just True <$ string "sx") <|> (Just False <$ string "zx")
  some ws
  ops <- sepBy1 parseOperand comma
  ops <- maybe (do 
      size <- maybe (fail "ambigous operand sizes") return (foldr ((<|>) . getSize) Nothing ops)
      traverse (assertSize size) ops
    ) (\_ -> do
      maybe (fail "invalid operand sizes for movsx/zx") (\_ -> return ops) $ foldM (\a -> mfilter (< a) . getSize) 5 ops
    ) extra
  case (ops, extra) of
    ([op1, op2], Nothing)
      | isRegister op1
      || (isMemory op1 && not (isMemory op2))
      -> return $ Mov False op1 op2
    ([op1, op2], Just extend)
      | isRegister op1 && not (isImmediate op2)
      -> return $ Mov extend op1 op2
    _ -> fail $ "Invalid operands for mov(sx/zx)"

parseImul :: Parser Instruction
parseImul = do
  string "imul"
  some ws
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
  nextLine
  string "idiv"
  some ws
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
  some ws
  ops <- sepBy1 parseOperand comma
  case ops of
    [op1, op2]
      | isRegister op1 && isMemory op2
      -> return $ Lea op1 op2
    _ -> fail "Invalid operands for lea"

parseRet :: Parser Instruction
parseRet = Ret <$ string "ret"

parseCall :: Parser Instruction
parseCall = do
  string "call"
  some ws
  Call <$> parseLabelName

parseJmp :: Parser Instruction
parseJmp = do
  string "jmp"
  some ws
  Jmp <$> parseLabelName

parseConditional :: Parser (Condition -> Conditional)
parseConditional = do
  mode <- (Cmp <$ string "cmp") <|> (Test <$ string "test")
  some ws
  ops <- sepBy1 parseOperand comma
  size <- maybe (fail "ambigous operand sizes") return (foldr ((<|>) . getSize) Nothing ops)
  ops <- traverse (assertSize size) ops
  case ops of
    [op1, op2]
      | isRegister op1
      || (isMemory op1 && not (isMemory op2))
      -> return $ Conditional op1 op2 . mode
    _ -> fail $ "Invalid operands for cmp/test"

parseCondition :: Parser Condition
parseCondition = choice $ zipWith (\a b -> a <$ foldr ((<|>) . try . (<* lookAhead ws) . string) empty b) [E, NE, G, GE, L, LE] [["e", "z"], ["ne", "nz"], ["g", "nle"], ["ge", "nl"], ["l", "nge"], ["le", "ng"]]

parseJcc :: Parser Instruction
parseJcc = do
  condal <- parseConditional
  nextLine
  char 'j'
  cond <- parseCondition
  some ws
  label <- parseLabelName
  return $ Jcc label "" (condal cond)

parseCmovcc :: Parser Instruction
parseCmovcc = do
  condal <- parseConditional
  nextLine
  string "cmov"
  cond <- parseCondition
  some ws
  ops <- sepBy1 parseOperand comma
  size <- maybe (fail "ambigous operand sizes") return (foldr ((<|>) . getSize) Nothing ops)
  when (size == 1) $ fail "opernad sizes are too small"
  ops <- traverse (assertSize size) ops
  case ops of
    [op1, op2]
      | isRegister op1 && not (isImmediate op2)
      -> return $ Cmovcc op1 op2 (condal cond)
    _ -> fail $ "Invalid operands for cmovcc"

parseSetcc :: Parser Instruction
parseSetcc = do
  condal <- parseConditional
  nextLine
  string "set"
  cond <- parseCondition
  some ws
  op <- parseOperand
  when (isImmediate op) $ fail "invalid operand for setcc"
  op <- assertSize 1 op
  return $ Setcc op (condal cond)

parsePush :: Parser Instruction
parsePush = do
  string "push"
  some ws
  op <- parseOperand
  let size = maybe 4 id $ getSize op
  when (size == 1) $ fail "invalid operand size for push"
  op <- assertSize size op
  return $ Push op

parsePop :: Parser Instruction
parsePop = do
  string "pop"
  some ws
  op <- parseOperand
  when (isImmediate op) $ fail "invalid operand for pop"
  let size = maybe 4 id $ getSize op
  when (size == 1) $ fail "invalid operand size for pop"
  op <- assertSize size op
  return $ Pop op

parseLeave :: Parser Instruction
parseLeave = Leave <$ string "leave"

parseInstruction :: Parser Instruction
parseInstruction = choice [try parseLabel, parseAdd, parseSub, parseMov, parseImul, parseExtIdiv, try parseLea, parseRet, try parseCall, parseJmp, try parseJcc, try parseCmovcc, parseSetcc, try parsePush, parsePop, parseLeave]