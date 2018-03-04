{-# LANGUAGE OverloadedStrings #-}
module Reagan.Generator
  ( GeneratedProgram(..)
  , generateProgram
  ) where

import           Control.Monad         (forever)
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (unpack)
import           Data.Time.Clock       (NominalDiffTime, diffUTCTime)
import           Safe                  (headMay, tailMay)
import           System.IO             (FilePath)
import           System.IO.Temp        (writeSystemTempFile)
import           Text.Read             (readMaybe)
import           Text.Regex.PCRE.Light (Regex, compile, match, multiline)

import           Reagan

data GeneratedProgram =
  GeneratedProgram { gpTimeout     :: Int               -- ^ Timeout used for generation (seconds)
                   , gpSeed        :: Maybe Integer     -- ^ Generator seed
                   , gpArguments   :: [ByteString]      -- ^ Generator arguments
                   , gpVersion     :: ByteString        -- ^ Version Information
                   , gpOptions     :: Maybe ByteString  -- ^ Generator options (parsed from output)
                   , gpProgramPath :: FilePath          -- ^ Path to generator program
                   , gpRunningTime :: NominalDiffTime   -- ^ Running time
                   } deriving (Show, Eq)

generatedFileTemplate :: String
generatedFileTemplate = "csmith-generated-.c"

versionCommand :: ExecutionConfig -> ExecutionConfig
versionCommand cfg =
  cfg { ecArguments = "--version" : ecArguments cfg }

generateProgram :: ExecutionConfig -> IO GeneratedProgram
generateProgram cfg = fst <$> execute cfg generateProgram'

generateProgram' :: ExecutionResult -> IO GeneratedProgram
generateProgram' r = do
  prgPath <- writeSystemTempFile generatedFileTemplate (unpack output)
  versionOutput <- getVersionOutput
  let runningTime = diffUTCTime (erStart r) (erFinished r)
  return
    GeneratedProgram { gpTimeout = ecTimeout cfg
                     , gpSeed = parseSeed output
                     , gpArguments = ecArguments cfg
                     , gpVersion = versionOutput
                     , gpProgramPath = prgPath
                     , gpOptions = parseOptions output
                     , gpRunningTime = runningTime
                     }
  where
    output           = erOutput r
    cfg              = erConfig r
    reOptions = compile "\\s\\*\\sOptions:\\s*(.+)\\s*$" [multiline]
    reSeed = compile "\\s\\*\\sSeed:\\s+(\\d+)" []
    getVersionOutput :: IO ByteString
    getVersionOutput =
      erOutput <$> onlyResult (execute (versionCommand cfg))
    parseOptions :: ByteString -> Maybe ByteString
    parseOptions prg =
      match reOptions prg [] >>= tailMay >>= headMay
    parseSeed :: ByteString -> Maybe Integer
    parseSeed prg = do
      seed <- match reSeed prg [] >>= tailMay >>= headMay
      readMaybe (unpack seed)
