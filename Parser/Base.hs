{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Parser.Base where

import Types

import Text.Parsec hiding ((<|>), some, many, optional)
import Control.Applicative
import Data.Char

type Parser a = forall s u m. Stream s m Char => ParsecT s u m a

parseRegister :: Parser Operand
parseRegister = Register 4 <$> foldr ((<|>) . try . string) empty ["eax", "ebx", "ecx", "edx", "edi", "esi", "r8d", "r9d"]

parseImmediate :: Parser Operand
parseImmediate = Immediate <$> ((negate <$ char '-' <|> id <$ optional (char '+')) <*> (parseHex <|> parseBin <|> parseOct <|> parseDec))
  where
    parseHex :: Parser Int
    parseHex = try (char '0' *> oneOf "xX") *> (readBase 16 <$> some hexDigit)

    parseBin :: Parser Int
    parseBin = try (char '0' *> oneOf "bB") *> (readBase 2 <$> some (oneOf "01"))

    parseOct :: Parser Int
    parseOct = char '0' *> (readBase 8 <$> some octDigit)

    parseDec :: Parser Int
    parseDec = read <$> some digit

    readBase :: Int -> String -> Int
    readBase n = foldl (\a c -> a * n + digitToInt c) 0

eax :: Operand
eax = Register 4 "eax"

comma :: Parser ()
comma = char ',' *> spaces