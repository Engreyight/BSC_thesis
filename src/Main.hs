module Main where

import Types
import Parser.Base
import Parser.Intel
import Assembler

import Text.Parsec hiding ((<|>), some, many, optional)
import Text.Parsec.ByteString (parseFromFile)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Writer.CPS
import Data.ByteString.Builder
import System.IO
import Options.Applicative hiding (Parser)
import System.Directory
import System.FilePath

main = join $ customExecParser (prefs $ disambiguate <> showHelpOnError <> showHelpOnEmpty) $ info (helper <*> options) fullDesc
  where
    options = assemble
      <$> strOption (long "output-folder" <> short 'o' <> metavar "FOLDER" <> value "functions" <> showDefault <> help "The folder where the assembled functions go")
      <*> some (strArgument (metavar "FILES" <> help "Input files to assemble"))
    
assemble :: FilePath -> [FilePath] -> IO ()
assemble outFolder inputFiles = do
  instr <- foldM (\a c -> let mainName = takeBaseName c in if isValidLabel mainName then parseFromFile (sepEndBy1 parseInstruction nextLine <* eof) c >>= either (\pe -> a <$ print pe) (\res -> return $ splitFunctions mainName res ++ a) else a <$ putStrLn ("Filename invalid as label name: " ++ c)) [] inputFiles
  createDirectoryIfMissing True outFolder
  withCurrentDirectory outFolder $ mapM_ (\(label, list) -> withBinaryFile (label <.> "mcfunction") WriteMode $ \h -> hPutBuilder h (execWriter (traverse processInstruction list))) instr