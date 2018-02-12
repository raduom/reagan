{-# LANGUAGE OverloadedStrings #-}
module Reagan.Compiler ( ExecutionResult(..)
                       , executeCLang
                       , executeKcc
                       ) where

import           Data.Text        (Text, pack, unpack)
import           Pipes
import           System.Directory (doesFileExist, getPermissions, setPermissions, setOwnerExecutable)
import           System.IO.Temp   (emptySystemTempFile)

import           Reagan.Generator
import           Reagan.Run

data ExecutionResult =
   ExecutionResult { erCompilationOutput :: Text
                   , erGeneratedProgram  :: Text
                   , erChecksum          :: Maybe Text
                   } deriving (Show, Eq)

execute :: Int -> Text -> [Text] -> GeneratedProgram -> IO ExecutionResult
execute timeout cpl args prg = do
  fp <- emptySystemTempFile $ unpack cpl ++ "-out-"
  (outC, _) <- runWithTimeout timeout (unpack cpl) $ insertTempFile fp args
  makeExecutable fp
  (outR, _) <- runWithTimeout timeout fp []
  return  ExecutionResult { erCompilationOutput = outC
                          , erGeneratedProgram = gpOutputStream prg
                          , erChecksum = Just outR
                          }
  where
    insertTempFile :: String -> [Text] -> [Text]
    insertTempFile fp = map (\a -> if a == "##TEMP##" then (pack fp) else a)
    makeExecutable :: FilePath -> IO ()
    makeExecutable fp = do
      ps <- getPermissions fp
      let ps' = setOwnerExecutable True ps
      setPermissions fp ps'

executeCLang :: Int -> GeneratedProgram -> IO ExecutionResult
executeCLang timeout prg = execute timeout "clang" [(pack $ gpPath prg), "-o", "##TEMP##"] prg

executeKcc :: Int -> GeneratedProgram -> IO ExecutionResult
executeKcc timeout prg = execute timeout "kcc" [(pack $ gpPath prg), "-o", "##TEMP##"] prg
