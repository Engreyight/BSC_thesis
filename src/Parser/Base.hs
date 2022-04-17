{-# LANGUAGE OverloadedStrings #-}
module Parser.Base where

import Types

import Text.Parsec hiding ((<|>), some, many, optional)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder (stringUtf8)
import Control.Applicative
import Data.Char

type Parser = Parsec ByteString ()

isValidLabel :: String -> Bool
isValidLabel (x : xs) = (isLower x || isDigit x) && all (not . isUpper) xs

parseRegister :: Parser Operand
parseRegister = many (char '+') *> choice (concat (zipWith (\size -> map (\name -> Register size name <$ try (string name))) [1, 2, 4] [["al", "bl", "cl", "dl", "dil", "sil", "spl", "bpl"], ["ax", "bx", "cx", "dx", "di", "si", "sp", "bp"], ["eax", "ebx", "ecx", "edx", "edi", "esi", "esp", "ebp"]]))

parseImmediate :: Parser Operand
parseImmediate = Immediate <$> parseNumber

parseNumber :: Parser Int
parseNumber = sign <*> (parseHex <|> parseBin <|> parseOct <|> parseDec)
  where
    sign :: Parser (Int -> Int)
    sign = liftA2 (.) (negate <$ char '-' ) sign <|> char '+' *> sign <|> return id
  
    parseHex :: Parser Int
    parseHex = try (char '0' *> oneOf "xX") *> (readBase 16 <$> some hexDigit)

    parseBin :: Parser Int
    parseBin = try (char '0' *> oneOf "bB") *> (readBase 2 <$> some (oneOf "01"))

    parseOct :: Parser Int
    parseOct = try (char '0' *> (readBase 8 <$> some octDigit))

    parseDec :: Parser Int
    parseDec = read <$> some digit

    readBase :: Int -> String -> Int
    readBase n = foldl (\a c -> a * n + digitToInt c) 0

ws :: Parser Char
ws = satisfy (\c -> isSpace c && not (c `elem` ['\r', '\n']))

comma :: Parser Char
comma = char ',' <* many ws

nextLine :: Parser Char
nextLine = many ws *> endOfLine <* spaces