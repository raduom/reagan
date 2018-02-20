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

import           Control.Monad         (replicateM_, forM)

import           Pipes
import           Pipes.Prelude         (fold, take)

kccCompiler :: CompilerDefinition
kccCompiler =
  CompilerDefinition { cdVersion = ExecutionConfig { ecCommand = "kcc"
                                                   , ecArguments = ["--version"]
                                                   , ecTimeout = 5
                                                   }
                     , cdCompilerOutput =
                         \fp -> do
                           executable <- emptySystemTempFile "kcc-out-"
                           return ( ExecutionConfig { ecCommand = "kcc"
                                                    , ecArguments = [pack fp, "-o", pack executable]
                                                    , ecTimeout = 60
                                                    }
                                  , executable )
                     }

clangCompiler :: CompilerDefinition
clangCompiler =
  CompilerDefinition { cdVersion = ExecutionConfig { ecCommand = "clang"
                                                   , ecArguments = ["--version"]
                                                   , ecTimeout = 5
                                                   }
                     , cdCompilerOutput =
                         \fp -> do
                           executable <- emptySystemTempFile "clang-out-"
                           return ( ExecutionConfig { ecCommand = "clang"
                                                    , ecArguments = [pack fp, "-o", pack executable]
                                                    , ecTimeout = 10
                                                    }
                                  , executable )
                     }


compilerDefinitions :: [CompilerDefinition]
compilerDefinitions = [clangCompiler, clangCompiler]

singleTest :: ExecutionConfig
           -> IO ( Maybe (GeneratedProgram
                 , [Maybe (CompiledProgram, Maybe ExecutionWithChecksum)]) )
singleTest cfg = do
  maybeGeneratedProgram <- generateProgram cfg
  forM maybeGeneratedProgram $
    \generatedProgram -> do
      tests <- mapM (executeWithCompiler generatedProgram) compilerDefinitions
      return (generatedProgram, tests)
  where
    executeWithCompiler :: GeneratedProgram
                        -> CompilerDefinition
                        -> IO (Maybe (CompiledProgram, Maybe ExecutionWithChecksum))
    executeWithCompiler generatedProgram compilerDefinition = do
      maybeCompiledProgram <- compileProgram compilerDefinition generatedProgram
      forM maybeCompiledProgram $
        \compiledProgram -> do
          maybeExecution <- executeProgram ExecutionConfig { ecTimeout = 60
                                                           , ecCommand = (cpExecutablePath compiledProgram)
                                                           , ecArguments = []
                                                           }
          return (compiledProgram, maybeExecution)

main :: IO ()
main = do
  replicateM_ 5 $ do
    result <- singleTest ExecutionConfig { ecCommand = "/usr/local/bin/csmith"
                                         , ecArguments = []
                                         , ecTimeout = 10
                                         }
    putStrLn (show result)
