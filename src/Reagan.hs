{-# LANGUAGE OverloadedStrings #-}
module Reagan
  ( ExecutionResult(..)
  , ExecutionConfig(..)
  , execute
  , runningTime
  , onlyValue
  , onlyResult ) where

import           Control.Concurrent    (forkIO, threadDelay)

import qualified Data.ByteString       as BS
import           Data.ByteString.Char8 (unpack)
import           Data.Maybe
import           Data.Monoid
import           Data.Text.Encoding    (decodeUtf8)
import           Data.Text.IO          (hGetContents)
import           Data.Time.Clock       (NominalDiffTime, diffUTCTime)
import           Data.Time.Clock       (UTCTime, getCurrentTime)

import           System.Exit           (ExitCode (..))
import           System.IO             (Handle, hClose)
import           System.Process        (CreateProcess (..), ProcessHandle,
                                        StdStream (..), getProcessExitCode,
                                        proc, terminateProcess, waitForProcess,
                                        withCreateProcess)

data ExecutionConfig =
  ExecutionConfig { ecCommand   :: FilePath
                  , ecArguments :: [BS.ByteString]
                  , ecTimeout   :: Int
                  } deriving (Eq, Show)

data ExecutionResult =
  ExecutionResult { erOutput   :: BS.ByteString
                  , erError    :: BS.ByteString
                  , erStart    :: UTCTime
                  , erFinished :: UTCTime
                  , erConfig   :: ExecutionConfig
                  } deriving (Eq, Show)

runningTime :: ExecutionResult -> NominalDiffTime
runningTime r = diffUTCTime (erFinished r) (erStart r)

readProcessStream :: ProcessHandle
                  -> (Handle, Handle)
                  -> IO (BS.ByteString, BS.ByteString)
readProcessStream ph (out, err) = go (mempty, mempty)
  where
    go (outAcc, errAcc) = do
      threadDelay (100 * 1000)
      bsOut <- BS.hGetNonBlocking out (64 * 1024)
      bsErr <- BS.hGetNonBlocking err (64 * 1024)
      let (outAcc', errAcc') = (outAcc <> bsOut, errAcc <> bsErr)
      s <- getProcessExitCode ph
      case s of
        Nothing -> go (outAcc', errAcc')
        Just ec -> do
          outLast <- BS.hGetContents out
          errLast <- BS.hGetContents err
          return (outAcc' <> outLast, errAcc' <> errLast)

onlyValue :: ExecutionConfig
          -> (ExecutionResult -> IO (Maybe a))
          -> IO (Maybe a)
onlyValue cfg process = fst <$> (execute cfg process)

onlyResult :: ((ExecutionResult -> IO (Maybe ExecutionResult))
                                -> IO (Maybe ExecutionResult, ExecutionResult))
            -> IO ExecutionResult
onlyResult runCmd = snd <$> (runCmd $ return . Just)

execute :: ExecutionConfig
        -> (ExecutionResult -> IO (Maybe a))
        -> IO (Maybe a, ExecutionResult)
execute cfg processResult = do
  let executable = ecCommand cfg
      args       = ecArguments cfg
      timeout    = ecTimeout cfg
  putStrLn $ "Running: " ++ executable ++ " " ++ (show args)
  startExecution <- getCurrentTime
  withCreateProcess
    (proc executable (map unpack args)) { std_out = CreatePipe, std_err = CreatePipe }
    (\_ (Just stdout) (Just stderr) ph -> do
        forkIO $ do
          threadDelay (timeout * 1000000)
          terminateProcess ph
        (out, err) <- readProcessStream ph (stdout, stderr)
        waitForProcess ph
        finishedExecution <- getCurrentTime
        let er = ExecutionResult { erOutput = out
                                 , erError  = err
                                 , erStart = startExecution
                                 , erFinished = finishedExecution
                                 , erConfig = cfg
                                 }
        (\r -> (r, er)) <$> processResult er)



