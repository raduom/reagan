{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE StandaloneDeriving #-}
module Reagan.Query where

import           Conduit
import           Data.Bifunctor        (first)
import qualified Data.ByteString.Lazy  as LBS
import           Data.Fixed            (Pico)
import           Data.List             (isPrefixOf)
import           Data.Time.Clock       (NominalDiffTime)
import           Data.Void
import           System.Directory      (doesFileExist, listDirectory)
import           System.FilePath.Posix (takeFileName, (</>))
import qualified System.IO.Strict      as IOS
import           Text.Megaparsec       hiding (State)
import           Text.Regex.PCRE.Heavy (Regex, compileM, gsub, re, (=~))

import           Pipes

import           Reagan.Compiler       (CompiledProgram (..))
import           Reagan.Execution      (ExecutionWithChecksum (..))
import           Reagan.Generator      (GeneratedProgram (..))

data Result = Result
  { rCompiler  :: !(Maybe CompiledProgram)
  , rExecution :: !(Maybe ExecutionWithChecksum)
  , rGenerator :: !GeneratedProgram
  , rProfile   :: !String
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
  read . fixDuration <$> IOS.readFile (directory </> "generator.out")

loadResult :: FilePath -> GeneratedProgram -> String -> IO Result
loadResult directory generator profile = do
  compilerOutput <- load "_compiler.out"
  executionOutput <- load "_execution.out"
  pure Result
    { rCompiler = compilerOutput
    , rExecution = executionOutput
    , rGenerator = generator
    , rProfile = profile
    }
  where
    load :: Read a => String -> IO (Maybe a)
    load suffix = do
      exists <- doesFileExist (directory </> profile ++ suffix)
      if exists
      then Just . read . fixDuration <$> IOS.readFile (directory </> profile ++ suffix)
      else return Nothing

repositoryStream :: MonadIO m
                 => [String] -> ConduitT FilePath Result m ()
repositoryStream profiles =
     filterC (isPrefixOf "csmith_seed" . takeFileName)
  .| mapMC load
  .| concatC
  where
    load :: (MonadIO m) => FilePath -> m [Result]
    load sample = liftIO $ do
      putStrLn $ "Processing " ++ sample
      prg <- loadGenerator sample
      mapM (loadResult sample prg) profiles

loadDirectory :: FilePath -> [String] -> IO [Result]
loadDirectory repository profiles = runConduitRes $
     sourceDirectory repository
  .| repositoryStream profiles
  .| sinkList

parseFile :: Parsec Void LBS.ByteString a
          -> FilePath
          -> IO (Either (ParseErrorBundle LBS.ByteString Void) a)
parseFile parser file =
  runParser parser file <$> LBS.readFile file

parseSeed :: FilePath -> String
parseSeed directory = directory

fixDuration :: String -> String
fixDuration =
  gsub [re|= (-?[.\d]+)s|] (\(d:_) -> "= " ++ d :: String)
