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

data Result = Result
  { rCompiler  :: CompiledProgram
  , rExecution :: ExecutionWithChecksum
  , rSeed      :: String
  , rProfile   :: String
  } deriving (Show)

instance Read NominalDiffTime where
  readsPrec _ s = map (first realToFrac)
                      (reads s :: [(Pico, String)])

deriving instance Read Result
deriving instance Read CompiledProgram
deriving instance Read ExecutionWithChecksum

loadResult :: FilePath -> String -> IO Result
loadResult directory profile = do
  compilerOutput <- read . fixDuration <$> readFile (directory </> profile ++ "_compiler.out")
  executionOutput <- read . fixDuration <$> readFile (directory </> profile ++ "_execution.out")
  pure Result
    { rCompiler = compilerOutput
    , rExecution = executionOutput
    , rSeed = ""
    , rProfile = profile
    }

loadDirectory :: FilePath -> [String] -> [Result]
loadDirectory = undefined

parseFile :: Parsec Void LBS.ByteString a
          -> FilePath
          -> IO (Either (ParseErrorBundle LBS.ByteString Void) a)
parseFile parser file =
  runParser parser file <$> LBS.readFile file

fixDuration :: String -> String
fixDuration =
  gsub [re|= (-?[.\d]+)s|] (\(d:_) -> ("= " ++ d :: String))
