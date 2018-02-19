{-# LANGUAGE OverloadedStrings #-}
module Reagan.Execution where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Time.Clock (NominalDiffTime)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Text.Regex.PCRE.Light (Regex, compile, match)
import Safe (headMay, tailMay)

import Reagan
import Reagan.Compiler

data ExecutionWithChecksum =
  ExecutionWithChecksum { ewcTimeout :: Int
                        , ewcOutput :: ByteString
                        , ewcError :: ByteString
                        , ewcChecksum :: Int
                        , ewcRunningTime :: NominalDiffTime
                    } deriving (Show, Eq)

executeProgram :: ExecutionConfig -> IO (Maybe ExecutionWithChecksum)
executeProgram cfg =
  onlyValue cfg $
    \result -> do
      putStrLn $ "Parsing: " ++ (unpack $ erOutput result)
      return $ ctor result <$> parseChecksum (erOutput result)
  where
    ctor :: ExecutionResult -> Int -> ExecutionWithChecksum
    ctor result checksum =
      ExecutionWithChecksum { ewcTimeout = ecTimeout cfg
                            , ewcOutput = erOutput result
                            , ewcError = erError result
                            , ewcChecksum = checksum
                            , ewcRunningTime = runningTime result
                            }

parseChecksum :: ByteString -> Maybe Int
parseChecksum input = do
  checksum <- match reChecksum input [] >>= tailMay >>= headMay
  readMaybe $ "0x" ++ (unpack checksum)
  where
    reChecksum = compile "checksum = ([0-9A-F]+)" []
