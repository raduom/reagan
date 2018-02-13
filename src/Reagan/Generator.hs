{-# LANGUAGE OverloadedStrings #-}
module Reagan.Generator where

import           Data.Text.Encoding
import           Pipes
import           Safe                  (headMay, tailMay)
import           Text.Read             (readMaybe)

import           Control.Monad         (forever)
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (unpack)
import           System.IO             (FilePath)
import           System.IO.Temp        (writeSystemTempFile)
import           Text.Regex.PCRE.Light (compile, match, multiline)

import           Reagan.Run

data GeneratorConfig =
  GeneratorConfig { gcPath    :: String
                  , gcTimeout :: Int
           } deriving (Show, Eq)

data GeneratedProgram =
  GeneratedProgram { gpPath         :: FilePath
                   , gpOutputStream :: ByteString
                   , gpSeed         :: Integer
                   , gpOptions      :: ByteString
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
      (out, err) <- runWithTimeout (gcTimeout generatorConfig)
                                   (gcPath generatorConfig) []
      fp <- writeSystemTempFile "csmith-generated-.c" (unpack out)
      case mkGeneratedProgram fp out of
        Nothing  -> error "Cannot parse the program seed or program options."
        Just  gp -> do
          putStrLn $ "seed: " ++ show (gpSeed gp)
          putStrLn $ "options: " ++ show (gpOptions gp)
          return gp
    yield prg

d0 :: ByteString
d0 = "/*\
\ * This is a RANDOMLY GENERATED PROGRAM.\n\
\ *\n\
\ * Generator: csmith 2.3.0\n\
\ * Git version: 30dccd7\n\
\ * Options:   (none)\n\
\ * Seed:      10515825974410246227\n\
\ */"

mkGeneratedProgram :: FilePath -> ByteString -> Maybe GeneratedProgram
mkGeneratedProgram fp content = do
  seed <- parseSeed content
  options <- parseOptions content
  return GeneratedProgram { gpPath = fp
                          , gpOutputStream = content
                          , gpSeed = seed
                          , gpOptions = options
                          }
  where
    reSeed = compile "\\s\\*\\sSeed:\\s+(\\d+)" []
    reOptions = compile "\\s\\*\\sOptions:\\s*(.+)\\s*$" [multiline]
    parseSeed :: ByteString -> Maybe Integer
    parseSeed prg = do
      seed <- match reSeed prg [] >>= tailMay >>= headMay
      readMaybe (unpack seed)
    parseOptions :: ByteString -> Maybe ByteString
    parseOptions prg = do
      match reOptions prg [] >>= tailMay >>= headMay

