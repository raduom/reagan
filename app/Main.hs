{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Reagan
import           Reagan.Compiler
import           Reagan.Execution
import           Reagan.Generator

import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Either
import           Data.Map              (Map)

import           System.IO.Temp        (emptySystemTempFile)

import           Control.Monad         (forM, replicateM_)

import           Pipes
import           Pipes.Prelude         (fold, take)

ccertCompiler :: CompilerDefinition
ccertCompiler = mkCompilerDefinition "ccomp_default" "ccomp" ["-version"] ["-fall", "##PROGRAM##", "-o", "##EXECUTABLE##"] 60

kccCompiler :: CompilerDefinition
kccCompiler = mkCompilerDefinition "kcc_default" "kcc" ["--version"] ["##PROGRAM##", "-o", "##EXECUTABLE##"] 60

clangCompiler :: CompilerDefinition
clangCompiler = mkCompilerDefinition "clang_default" "clang" ["--version"] ["##PROGRAM##", "-o", "##EXECUTABLE##"] 10

compilerDefinitions :: [CompilerDefinition]
compilerDefinitions = [clangCompiler, ccertCompiler]

singleTest :: ExecutionConfig
           -> IO (  Maybe (GeneratedProgram
                 , [Maybe (CompiledProgram, Maybe ExecutionWithChecksum)]) )
singleTest cfg =
  generateProgram cfg >>= (traverse $
    \generatedProgram -> do
      testResults <- mapM (executeWithCompiler generatedProgram) compilerDefinitions
      return (generatedProgram, testResults))

executeWithCompiler :: GeneratedProgram
                    -> CompilerDefinition
                    -> IO (Maybe (CompiledProgram, Maybe ExecutionWithChecksum))
executeWithCompiler generatedProgram compilerDefinition = do
  compileProgram compilerDefinition generatedProgram >>= (traverse $
    \compiledProgram -> do
      maybeExecution <-
        executeProgram ExecutionConfig { ecTimeout = 60
                                       , ecCommand = (cpExecutablePath compiledProgram)
                                       , ecArguments = []
                                       }
      return (compiledProgram, maybeExecution))



main :: IO ()
main = do
  replicateM_ 5 $ do
    result <- singleTest ExecutionConfig { ecCommand = "csmith"
                                         , ecArguments = []
                                         , ecTimeout = 10
                                         }
    putStrLn (show result)
