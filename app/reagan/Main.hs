{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Reagan
import           Reagan.Compiler
import           Reagan.Execution
import           Reagan.Generator
import           Reagan.Serializer

import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Either
import           Data.Maybe            (maybe)

import           System.IO.Temp        (emptySystemTempFile)
import           Control.Monad         (forM, forever)

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
           -> IO (GeneratedProgram
                          , [(CompiledProgram, Maybe ExecutionWithChecksum)])
singleTest cfg =
  generateProgram cfg >>= (
    \generatedProgram -> do
      testResults <- mapM (executeWithCompiler generatedProgram) compilerDefinitions
      return (generatedProgram, testResults))

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
    result <- singleTest ExecutionConfig { ecCommand = "csmith"
                                         , ecArguments = ["--no-packed-struct"]
                                         , ecTimeout = generatorTimeout
                                         }
    serialize result