{-# LANGUAGE OverloadedStrings #-}
module Reagan.Compiler ( CompiledProgram(..)
                       , CompilerDefinition(..)
                       , compileProgram
                       ) where

import           Data.ByteString  (ByteString)
import           Data.Text        (Text, pack, unpack)
import           Data.Time.Clock  (NominalDiffTime, diffUTCTime)
import           Pipes
import           System.Directory (doesFileExist, getPermissions,
                                   setOwnerExecutable, setPermissions)
import           System.IO.Temp   (emptySystemTempFile)

import           Reagan
import           Reagan.Generator

data CompiledProgram =
   CompiledProgram { cpTimeout        :: Int
                   , cpVersion        :: ByteString
                   , cpOutput         :: ByteString
                   , cpError          :: ByteString
                   , cpRunningTime    :: NominalDiffTime
                   , cpExecutablePath :: FilePath
                   } deriving (Show, Eq)

data CompilerDefinition =
  CompilerDefinition { cdVersion :: ExecutionConfig
                     , cdCompilerOutput :: FilePath -> IO (ExecutionConfig, FilePath)
                     }

compileProgram :: CompilerDefinition
               -> GeneratedProgram
               -> IO (Maybe CompiledProgram)
compileProgram compiler prg = do
  (executionConfig, executablePath) <- compilerOutput programPath
  onlyValue executionConfig $
    \result -> do
      compilerVersionOutput <-
        erOutput <$> (onlyResult $ execute (cdVersion compiler))
      compilationSuccess <- doesFileExist executablePath
      if compilationSuccess
        then do
          makeExecutable executablePath
          return $ Just CompiledProgram { cpTimeout = ecTimeout executionConfig
                                        , cpVersion = compilerVersionOutput
                                        , cpOutput = erOutput result
                                        , cpError = erError result
                                        , cpRunningTime = runningTime result
                                        , cpExecutablePath = executablePath
                                        }
        else return Nothing
  where
    programPath = gpProgramPath prg
    compilerOutput = (cdCompilerOutput compiler)

makeExecutable :: FilePath -> IO ()
makeExecutable fp = do
 ps <- getPermissions fp
 let ps' = setOwnerExecutable True ps
 setPermissions fp ps'

{-
runTest ::  -> GeneratedProgram -> IO TestResult
runTest timeout cpl args prg = do
  fp <- emptySystemTempFile $ unpack cpl ++ "-out-"
  compilationResult <- runWithTimeout timeout (unpack cpl) (insertTempFile fp args) undefined
  makeExecutable fp
  executionResult <- runWithTimeout timeout fp [] undefined
  return  TestResult { erCompilationOutput = (eOutput compilationResult)
                     , erGeneratedProgram = gpOutputStream prg
                     , erChecksum = Just (eOutput executionResult)
                     , erSeed = gpSeed prg
                     }
  where
    insertTempFile :: String -> [Text] -> [Text]
    insertTempFile fp = map (\a -> if a == "##TEMP##" then (pack fp) else a)
    makeExecutable :: FilePath -> IO ()
    makeExecutable fp = do
      ps <- getPermissions fp
      let ps' = setOwnerExecutable True ps
      setPermissions fp ps'

runTestUsingCLang :: Int -> GeneratedProgram -> IO TestResult
runTestUsingCLang timeout prg = execute timeout "clang" [(pack $ gpPath prg), "-o", "##TEMP##"] prg

runTestUsingKcc :: Int -> GeneratedProgram -> IO TestResult
runTestUsingKcc timeout prg = execute timeout "kcc" [(pack $ gpPath prg), "-o", "##TEMP##"] prg
-}
