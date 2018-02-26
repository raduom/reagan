{-# LANGUAGE OverloadedStrings #-}
module Reagan.Execution where

import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (unpack)
import           Data.Maybe            (mapMaybe)
import           Data.Time.Clock       (NominalDiffTime)
import           Safe                  (headMay, tailMay)
import           Text.Read             (readMaybe)
import           Text.Regex.PCRE.Light (Regex, compile, match)

import           Reagan
import           Reagan.Compiler

data ExecutionWithChecksum =
  ExecutionWithChecksum { ewcTimeout        :: Int             -- ^ Timeout used for execution
                        , ewcOutput         :: ByteString      -- ^ Execution output
                        , ewcError          :: ByteString      -- ^ Execution errors
                        , ewcChecksum       :: Maybe Int       -- ^ Execution checksum
                        , ewcRunningTime    :: NominalDiffTime -- ^ Running time
                        , ewcExecutablePath :: FilePath        -- ^ Path to executable
                    } deriving (Show, Eq)

executeProgram :: ExecutionConfig -> IO ExecutionWithChecksum
executeProgram cfg =
  onlyValue cfg $
    \result ->
      return ExecutionWithChecksum { ewcTimeout = ecTimeout cfg
                                   , ewcOutput = erOutput result
                                   , ewcError = erError result
                                   , ewcChecksum = parseChecksum (erOutput result)
                                   , ewcRunningTime = runningTime result
                                   , ewcExecutablePath = ecCommand cfg
                                   }

parseChecksum :: ByteString -> Maybe Int
parseChecksum input = do
  checksum <- match reChecksum input [] >>= tailMay >>= headMay
  readMaybe $ "0x" ++ unpack checksum
  where
    reChecksum = compile "checksum = ([0-9A-F]+)" []
