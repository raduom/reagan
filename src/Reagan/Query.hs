{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
module Reagan.Query where

import Text.Megaparsec hiding (State)
import qualified Data.ByteString.Lazy as LBS
import Data.Void
import Data.Time.Clock (NominalDiffTime)
import Data.Fixed (Pico)
import Data.Bifunctor (first)
import System.FilePath.Posix ((</>))
import Text.Regex.PCRE.Heavy (Regex, compileM, gsub, re, (=~))

import Reagan.Compiler (CompiledProgram(..))
import Reagan.Execution (ExecutionWithChecksum(..))
import Reagan.Generator (GeneratedProgram(..))

data Result = Result
  { rCompiler  :: CompiledProgram
  , rExecution :: ExecutionWithChecksum
  , rGenerator :: GeneratedProgram
  , rSeed      :: String
  , rProfile   :: String
  } deriving (Show)

instance Read NominalDiffTime where
  readsPrec _ s = map (first realToFrac)
                      (reads s :: [(Pico, String)])

deriving instance Read Result
deriving instance Read CompiledProgram
deriving instance Read ExecutionWithChecksum
deriving instance Read GeneratedProgram

loadGenerator :: FilePath -> IO GeneratedProgram
loadGenerator directory =
  read . fixDuration <$> readFile (directory </> "generator.out")

loadResult :: FilePath -> GeneratedProgram -> String -> IO Result
loadResult directory generator profile = do
  compilerOutput <- load "_compiler.out"
  executionOutput <- load "_execution.out"
  pure Result
    { rCompiler = compilerOutput
    , rExecution = executionOutput
    , rGenerator = generator
    , rSeed = parseSeed directory
    , rProfile = profile
    }
  where
    load :: Read a => String -> IO a
    load suffix = read . fixDuration <$> readFile (directory </> profile ++ suffix)

loadDirectory :: FilePath -> [String] -> [Result]
loadDirectory = undefined

parseFile :: Parsec Void LBS.ByteString a
          -> FilePath
          -> IO (Either (ParseErrorBundle LBS.ByteString Void) a)
parseFile parser file =
  runParser parser file <$> LBS.readFile file

parseSeed :: FilePath -> String
parseSeed directory = directory

fixDuration :: String -> String
fixDuration =
  gsub [re|= (-?[.\d]+)s|] (\(d:_) -> ("= " ++ d :: String))
