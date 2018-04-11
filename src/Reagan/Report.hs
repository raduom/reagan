{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE StandaloneDeriving #-}
module Reagan.Report ( runReportOnDirectory
                     , parseResult
                     , TestResult(..) ) where

import           Control.Concurrent.MVar (newMVar, putMVar, takeMVar)
import           Control.Monad           (filterM, forM, mapM, unless)

import           Data.Bifunctor          (first)
import           Data.Fixed              (Fixed (..), Pico)
import           Data.List               (filter, foldl', isPrefixOf,
                                          isSuffixOf)
import           Data.Time.Clock         (NominalDiffTime)
import           System.Directory        (doesDirectoryExist, doesFileExist,
                                          listDirectory)
import           System.FilePath         ((</>))
import           System.IO.Strict        (readFile)

import           Pipes

import           Reagan.Compiler         (CompiledProgram (..))
import           Reagan.Execution        (ExecutionWithChecksum (..))
import           Reagan.Generator        (GeneratedProgram (..))
import           Reagan.Serializer       (tplCompilerOutput, tplExecutionOutput,
                                          tplGeneratorOutput, tplProgram)
import           Text.Regex.PCRE.Heavy   (Regex, compileM, gsub, re, (=~))

import GHC.Generics (Generic(..))

import Control.DeepSeq (deepseq)

import Prelude hiding (readFile)

instance Read NominalDiffTime where
  readsPrec _ s = map (first realToFrac)
                      (reads s :: [(Pico, String)])

deriving instance Read GeneratedProgram
deriving instance Read CompiledProgram
deriving instance Read ExecutionWithChecksum

data TestResult =
  TestResult { trGeneratorOutput :: !GeneratedProgram
             , trProgram :: !String
             , trCompilerOutput :: ![(CompiledProgram, Maybe ExecutionWithChecksum)]
             } deriving (Show, Eq)

runReportOnDirectory :: (a -> TestResult -> a) -> a -> FilePath -> IO a
runReportOnDirectory reportFold report reportDir = assertIsDirectory reportDir >> do
  accM <- newMVar report
  reports <- filter (isPrefixOf "csmith_seed") <$> listDirectory reportDir
  runEffect $ for (each reports) $ \location -> do
    lift $ putStrLn $ "Processing: " ++ location
    acc <- lift $ takeMVar accM
    testResult <- lift $ parseResult (reportDir </> location)
    let acc' = reportFold acc testResult
    acc' `seq` lift $ putMVar accM acc'
  takeMVar accM

parseResult :: FilePath
            -> IO TestResult
parseResult fp = assertIsDirectory fp >> do
  program <- readFile (fp </> tplProgram)
  generatedProgram <- read . fixDuration <$> readFile (fp </> tplGeneratorOutput)
  compiledPrograms <- parseCompilerOutput fp >>= mapM (parseExecution fp)
  return $ TestResult generatedProgram program compiledPrograms

parseCompilerOutput :: FilePath
                    -> IO [CompiledProgram]
parseCompilerOutput fp = assertIsDirectory fp >> do
  compilerOutputs <- map (fp </> ) .
                     filter (isSuffixOf tplCompilerOutput) <$>
                     listDirectory fp
  mapM (fmap (read . fixDuration) . readFile) compilerOutputs

parseExecution :: FilePath
               -> CompiledProgram
               -> IO (CompiledProgram, Maybe ExecutionWithChecksum)
parseExecution fp cp = assertIsDirectory fp >> do
  let fname = fp </> cpCompilerTag cp ++ tplExecutionOutput
  fexists <- doesFileExist fname
  if fexists
     then do
       executionOutput <- read . fixDuration <$> readFile (fp </> cpCompilerTag cp ++ tplExecutionOutput)
       return (cp, Just executionOutput)
     else return (cp, Nothing)

fixDuration :: String -> String
fixDuration =
  gsub [re|= (-?[.\d]+)s|] (\(d:_) -> ("= " ++ d :: String))

assertIsDirectory :: FilePath -> IO ()
assertIsDirectory fp = do
  checkDirectory <- doesDirectoryExist fp
  unless checkDirectory (error $ "File " ++ fp ++ " must be a directory!")