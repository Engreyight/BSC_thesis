{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Types
import Parser.Base
import Parser.Intel
import Assembler

import Text.Parsec hiding ((<|>), some, many, optional)
import Text.Parsec.Error
import Text.Parsec.ByteString.Lazy (parseFromFile)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Writer.CPS
import Data.ByteString.Builder
import System.IO
import Options.Applicative hiding (Parser, ParseError)
import System.Directory
import System.FilePath
import Data.List

main = join $ customExecParser (prefs $ disambiguate <> showHelpOnError <> showHelpOnEmpty) $ info (helper <*> options) fullDesc
  where
    options = assemble
      <$> strOption (long "output-folder" <> short 'o' <> metavar "FOLDER" <> value "functions" <> showDefault <> help "The folder where the assembled functions go")
      <*> strOption (long "prefix" <> short 'p' <> metavar "NAMESPACE:[FOLDERS]" <> value "assembler:" <> showDefault <> help "The prefix for function names")
      <*> some (strArgument (metavar "FILES" <> help "Input files to assemble"))

filterMessage :: ParseError -> ParseError
filterMessage pe = maybe pe (flip newErrorMessage (errorPos pe)) $ find (\case Message _ -> True; _ -> False) (errorMessages pe)

assemble :: FilePath -> Builder -> [FilePath] -> IO ()
assemble outFolder prefix inputFiles = do
  instr <- foldM (\a c -> let mainName = takeBaseName c in if isValidLabel mainName then parseFromFile (sepEndBy1 parseInstruction nextLine <* eof) c >>= either (\pe -> a <$ print (filterMessage pe)) (\res -> return $ splitFunctions mainName res ++ a) else a <$ putStrLn ("Filename invalid as label name: " ++ c)) [] inputFiles
  createDirectoryIfMissing True outFolder
  withCurrentDirectory outFolder $ mapM_ (\(label, list) -> withBinaryFile (label <.> "mcfunction") WriteMode $ \h -> hPutBuilder h (execWriter (traverse (processInstruction prefix) list))) instr