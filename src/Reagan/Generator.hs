{-# LANGUAGE OverloadedStrings #-}
module Reagan.Generator
  ( GeneratedProgram(..)
  , generateProgram
  ) where

import           Control.Monad         (forever)
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (unpack)
import           Data.Text             (Text)
import           Data.Text.Encoding
import           Data.Time.Clock       (NominalDiffTime, diffUTCTime)
import           Pipes
import           Safe                  (headMay, tailMay)
import           System.IO             (FilePath)
import           System.IO.Temp        (writeSystemTempFile)
import           Text.Read             (readMaybe)
import           Text.Regex.PCRE.Light (Regex, compile, match, multiline)

import           Reagan

data GeneratedProgram =
  GeneratedProgram { gpTimeout     :: Int
                   , gpSeed        :: Integer
                   , gpArguments   :: [ByteString]
                   , gpVersion     :: ByteString
                   , gpProgramPath :: FilePath
                   , gpRunningTime :: NominalDiffTime
                   } deriving (Show, Eq)

generatedFileTemplate :: String
generatedFileTemplate = "csmith-generated-.c"

versionCommand :: ExecutionConfig -> ExecutionConfig
versionCommand cfg =
  cfg { ecArguments = "--version" : (ecArguments cfg) }

generateProgram :: ExecutionConfig -> IO (Maybe GeneratedProgram)
generateProgram cfg = fst <$> execute cfg generateProgram'

generateProgram' :: ExecutionResult -> IO (Maybe GeneratedProgram)
generateProgram' r = do
  prgPath <- writeSystemTempFile generatedFileTemplate (unpack output)
  versionOutput <- getVersionOutput
  let runningTime = diffUTCTime (erStart r) (erFinished r)
  return $
    maybeGeneratedProgram output
      (\seed options ->
          GeneratedProgram { gpTimeout = ecTimeout cfg
                           , gpSeed = seed
                           , gpArguments = ecArguments cfg
                           , gpVersion = versionOutput
                           , gpProgramPath = prgPath
                           , gpRunningTime = runningTime
                           })
  where
    output           = erOutput r
    cfg              = erConfig r
    getVersionOutput :: IO ByteString
    getVersionOutput =
      erOutput <$> (onlyResult $ execute (versionCommand cfg))

maybeGeneratedProgram :: ByteString
                      -> (Integer -> ByteString -> GeneratedProgram)
                      -> Maybe GeneratedProgram
maybeGeneratedProgram content ctor =
  ctor <$> parseSeed content <*> parseOptions content
  where
    reSeed :: Regex
    reSeed = compile "\\s\\*\\sSeed:\\s+(\\d+)" []

    reOptions :: Regex
    reOptions = compile "\\s\\*\\sOptions:\\s*(.+)\\s*$" [multiline]

    parseSeed :: ByteString -> Maybe Integer
    parseSeed prg = do
      seed <- match reSeed prg [] >>= tailMay >>= headMay
      readMaybe (unpack seed)

    parseOptions :: ByteString -> Maybe ByteString
    parseOptions prg = do
      match reOptions prg [] >>= tailMay >>= headMay
