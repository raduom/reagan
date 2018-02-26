{-# LANGUAGE OverloadedStrings #-}
module Reagan.Compiler ( CompiledProgram(..)
                       , CompilerDefinition(..)
                       , compileProgram
                       , mkCompilerDefinition
                       ) where

import           Control.Monad         (when)
import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Time.Clock       (NominalDiffTime, diffUTCTime)
import           System.Directory      (doesFileExist, getPermissions,
                                        setOwnerExecutable, setPermissions, getFileSize)
import           System.IO.Temp        (emptySystemTempFile)

import           Reagan
import           Reagan.Generator

data CompiledProgram =
   CompiledProgram { cpTimeout        :: Int              -- ^ Timeout used for compilation
                   , cpVersion        :: ByteString       -- ^ Version information
                   , cpOutput         :: ByteString       -- ^ Compiler output
                   , cpError          :: ByteString       -- ^ Compiler errors
                   , cpRunningTime    :: NominalDiffTime  -- ^ Running time
                   , cpExecutablePath :: Maybe FilePath   -- ^ Compiler path
                   , cpCompilerTag    :: String           -- ^ Compiler tag
                   } deriving (Show, Eq)

data CompilerDefinition =
  CompilerDefinition { cdVersion :: ExecutionConfig
                     , cdCompilerOutput :: FilePath -> IO (ExecutionConfig, FilePath)
                     , cdTag :: String
                     }

compileProgram :: CompilerDefinition
               -> GeneratedProgram
               -> IO CompiledProgram
compileProgram compiler prg = do
  (executionConfig, executablePath) <- compilerOutput programPath
  onlyValue executionConfig $
    \result -> do
      compilerVersionOutput <-
        erOutput <$> onlyResult (execute (cdVersion compiler))
      compilationSuccess <- do
        fileExists <- doesFileExist executablePath
        fileSize <- getFileSize executablePath
        return $ fileExists && (fileSize > 0)
      when compilationSuccess $ makeExecutable executablePath
      let executable =
            if compilationSuccess then Just executablePath
                                  else Nothing
      return CompiledProgram { cpTimeout = ecTimeout executionConfig
                             , cpVersion = compilerVersionOutput
                             , cpOutput = erOutput result
                             , cpError = erError result
                             , cpRunningTime = runningTime result
                             , cpExecutablePath = executable
                             , cpCompilerTag = cdTag compiler
                             }
  where
    programPath = gpProgramPath prg
    compilerOutput = cdCompilerOutput compiler

applyTemplate :: FilePath -> FilePath -> [ByteString] -> [ByteString]
applyTemplate generatedProgram executable =
  map $ (\arg -> if arg == "##PROGRAM##" then pack generatedProgram else arg) . (\arg -> if arg == "##EXECUTABLE##" then pack executable else arg)

mkCompilerDefinition :: String       -- ^ Compiler tag
                     -> FilePath     -- ^ Compiler command
                     -> [ByteString] -- ^ Version fetching arguments
                     -> [ByteString] -- ^ Compilation arguments
                     -> Int          -- ^ Compilation Timeout
                     -> CompilerDefinition
mkCompilerDefinition tag cpath versionArgs compileArgs compilationTimeout =
  CompilerDefinition { cdVersion = ExecutionConfig { ecTimeout = 5
                                                   , ecCommand = cpath
                                                   , ecArguments = versionArgs
                                                   }
                     , cdTag = tag
                     , cdCompilerOutput =
                         \generatedProgram -> do
                           executable <- emptySystemTempFile (tag ++ "-")
                           return (compilationConfig generatedProgram executable, executable)
                     }
  where
    compilationConfig :: FilePath -> FilePath -> ExecutionConfig
    compilationConfig prg exe =
      ExecutionConfig { ecCommand = cpath
                      , ecArguments = applyTemplate prg exe compileArgs
                      , ecTimeout = compilationTimeout
                      }

makeExecutable :: FilePath -> IO ()
makeExecutable fp = do
 ps <- getPermissions fp
 let ps' = setOwnerExecutable True ps
 setPermissions fp ps'
