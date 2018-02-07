{-# LANGUAGE OverloadedStrings #-}
module Generator where

import           Data.Text (Text)

import           Shelly   

data GeneratorConfig =
  GeneratorConfig { sPath :: Text
           } deriving (Show, Eq)

data GeneratedProgram =
  GeneratedProgram { gpCode :: Text
                } deriving (Show, Eq)

defaultConfig :: GeneratorConfig
defaultConfig =
  GeneratorConfig { sPath = "csmith"
                  }

generateProgram :: GeneratorConfig -> IO GeneratedProgram
generateProgram s = shelly $ do
  output <- silently $ run (fromText $ sPath s) []
  return GeneratedProgram { gpCode = output }
