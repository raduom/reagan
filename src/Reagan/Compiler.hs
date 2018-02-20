{-# LANGUAGE OverloadedStrings #-}
module Reagan.Compiler ( CompiledProgram(..)
                       , CompilerDefinition(..)
                       , compileProgram
                       , mkCompilerDefinition
                       ) where

import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Time.Clock       (NominalDiffTime, diffUTCTime)
import           System.Directory      (doesFileExist, getPermissions,
                                        setOwnerExecutable, setPermissions)
import           System.IO.Temp        (emptySystemTempFile)

import           Reagan
import           Reagan.Generator

data CompiledProgram =
   CompiledProgram { cpTimeout        :: Int
                   , cpVersion        :: ByteString
                   , cpOutput         :: ByteString
                   , cpError          :: ByteString
                   , cpRunningTime    :: NominalDiffTime
                   , cpExecutablePath :: FilePath
                   , cpCompilerTag    :: String
                   } deriving (Show, Eq)

data CompilerDefinition =
  CompilerDefinition { cdVersion :: ExecutionConfig
                     , cdCompilerOutput :: FilePath -> IO (ExecutionConfig, FilePath)
                     , cdTag :: String
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
                                        , cpCompilerTag = (cdTag compiler)
                                        }
        else return Nothing
  where
    programPath = gpProgramPath prg
    compilerOutput = (cdCompilerOutput compiler)

applyTemplate :: FilePath -> FilePath -> [ByteString] -> [ByteString]
applyTemplate generatedProgram executable =
  map (\arg -> if arg == "##PROGRAM##" then pack generatedProgram else arg) .
  map (\arg -> if arg == "##EXECUTABLE##" then pack executable else arg)

mkCompilerDefinition :: String -- ^ tag
                     -> FilePath -- ^ compiler path
                     -> [ByteString] -- ^ version fetching args
                     -> [ByteString] -- ^ compilation args
                     -> Int -- ^ compilation timeout
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
