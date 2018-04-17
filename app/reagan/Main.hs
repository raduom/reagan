{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Reagan
import           Reagan.Compiler
import           Reagan.Execution
import           Reagan.Generator
import           Reagan.Serializer

import           Data.ByteString.Char8     (ByteString, pack)
import           Data.Either
import           Data.Maybe                (fromJust, maybe)

import           Control.Monad             (forM, forever, mzero)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           System.Directory          (doesDirectoryExist)
import           System.IO.Temp            (emptySystemTempFile)

-- [ Configuration section ]

compilerDefinitions :: [CompilerDefinition]
compilerDefinitions = [clangCompiler, ccertCompiler, kccCompiler, gccCompiler]

ccertCompiler :: CompilerDefinition
ccertCompiler = mkCompilerDefinition "ccomp_default" "ccomp" ["-version"] ["-fall", "##PROGRAM##", "-o", "##EXECUTABLE##"] 60

kccCompiler :: CompilerDefinition
kccCompiler = mkCompilerDefinition "kcc_default" "kcc" ["--version"] ["##PROGRAM##", "-o", "##EXECUTABLE##"] 60

clangCompiler :: CompilerDefinition
clangCompiler = mkCompilerDefinition "clang_default" "clang" ["--version"] ["##PROGRAM##", "-o", "##EXECUTABLE##"] 10

gccCompiler :: CompilerDefinition
gccCompiler = mkCompilerDefinition "gcc_default" "gcc" ["--version"] ["##PROGRAM##", "-o", "##EXECUTABLE##"] 10

executionTimeout :: Int
executionTimeout = 60

generatorTimeout :: Int
generatorTimeout = 10

-- [ Protocol Section ]

singleTest :: ExecutionConfig
           -> MaybeT IO (GeneratedProgram
                         , [(CompiledProgram, Maybe ExecutionWithChecksum)])
singleTest cfg = do
  generatedProgram <- lift $ generateProgram cfg
  seed <- maybe mzero pure (gpSeed generatedProgram)
  testExists <- liftIO $ doesDirectoryExist (tplDirectoryName ++ show seed)
  if testExists
    then mzero
    else do
      testResults <- lift $ mapM (executeWithCompiler generatedProgram) compilerDefinitions
      return (generatedProgram, testResults)

executeWithCompiler :: GeneratedProgram
                    -> CompilerDefinition
                    -> IO (CompiledProgram, Maybe ExecutionWithChecksum)
executeWithCompiler generatedProgram compilerDefinition = do
  program <- compileProgram compilerDefinition generatedProgram
  executable <- forM (cpExecutablePath program) $
    \cmd -> executeProgram ExecutionConfig { ecTimeout = executionTimeout
                                           , ecCommand = cmd
                                           , ecArguments = []
                                           }
  return (program, executable)

main :: IO ()
main =
  forever $ do
    result <- runMaybeT $
              singleTest ExecutionConfig { ecCommand = "csmith"
                                         , ecArguments = ["--no-packed-struct"]
                                         , ecTimeout = generatorTimeout
                                         }
    maybe (pure ()) serialize result
