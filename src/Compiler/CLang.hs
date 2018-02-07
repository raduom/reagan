{-# LANGUAGE OverloadedStrings #-}
module Compiler.CLang where

import System.Timeout
import Data.Text

import           Compiler
import           Generator

import           Shelly

newtype CLangT = CLangT Int

compileCode :: GeneratedProgram -> IO (Text, Text)
compileCode prg = shelly $ do
    writefile "a.c" (gpCode prg)
    stdOutput <- silently $ run "clang" ["a.c", "-I/usr/local/Cellar/csmith/2.3.0/include/csmith-2.3.0/runtime/"]
    stdError  <- lastStderr
    pure (stdOutput, stdError)

runCode :: IO Text
runCode = shelly $ silently $ run "./a.out" []

instance Compiler CLangT where
  compile _ prg = do
    (out, err) <- compileCode prg
    checksum   <- timeout 100 runCode
    return $ ExecutionResult { erStdOutput = out
                             , erErrOutput = err
                             , erChecksum = checksum
                             }
