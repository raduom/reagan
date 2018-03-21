{-# LANGUAGE OverloadedStrings #-}
module Reagan.Generator
  ( GeneratedProgram(..)
  , generateProgram
  ) where

import           Control.Monad         (forever)
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (unpack, pack)
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
                   , gpArguments   :: [String]      -- ^ Generator arguments
                   , gpVersion     :: String        -- ^ Version Information
                   , gpOptions     :: Maybe String  -- ^ Generator options (parsed from output)
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
  prgPath <- writeSystemTempFile generatedFileTemplate output
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
    getVersionOutput :: IO String
    getVersionOutput =
      erOutput <$> onlyResult (execute (versionCommand cfg))
    parseOptions :: String -> Maybe String
    parseOptions prg =
      unpack <$> (match reOptions (pack prg) [] >>= tailMay >>= headMay)
    parseSeed :: String -> Maybe Integer
    parseSeed prg = do
      seed <- match reSeed (pack prg) [] >>= tailMay >>= headMay
      readMaybe (unpack seed)
