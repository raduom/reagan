{-# LANGUAGE OverloadedStrings #-}
module Reagan.Generator where

import           Data.Text      (Text, pack, unpack)
import           Pipes

import Control.Monad (forever)
import           System.IO      (FilePath)
import           System.IO.Temp (emptySystemTempFile)

import           Reagan.Run

data GeneratorConfig =
  GeneratorConfig { gcPath    :: String
                  , gcTimeout :: Int
           } deriving (Show, Eq)

data GeneratedProgram =
  GeneratedProgram { gpPath         :: FilePath
                   , gpOutputStream :: Text
                } deriving (Show, Eq)

defaultConfig :: GeneratorConfig
defaultConfig =
  GeneratorConfig { gcPath = "csmith"
                  , gcTimeout = 5
                  }

generateProgram :: GeneratorConfig -> Producer GeneratedProgram IO ()
generateProgram generatorConfig = do
  forever $ do
    prg <- lift $ do
      fp <- emptySystemTempFile "csmith-generated-.c"
      (out, err) <- runWithTimeout (gcTimeout generatorConfig)
                                   (gcPath generatorConfig) ["-o", pack fp]
      return GeneratedProgram { gpOutputStream = out
                              , gpPath = fp }
    yield prg
