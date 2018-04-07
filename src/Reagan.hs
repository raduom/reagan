{-# LANGUAGE OverloadedStrings #-}
module Reagan
  ( ExecutionResult(..)
  , ExecutionConfig(..)
  , execute
  , runningTime
  , onlyValue
  , onlyResult ) where

import           Control.Concurrent       (forkIO, threadDelay)
import           Control.Monad            (forM_)
import           Control.Monad.IO.Class   (liftIO)
import qualified Data.ByteString          as BS
import           Data.ByteString.Char8    (unpack)
import           Data.Maybe
import           Data.Monoid
import           Data.Time.Clock          (NominalDiffTime, UTCTime,
                                           diffUTCTime, getCurrentTime)
import           System.Directory         (getCurrentDirectory,
                                           setCurrentDirectory)
import           System.Exit              (ExitCode (..))
import           System.IO                (Handle, hClose)
import           System.IO.Temp           (withSystemTempDirectory)
import           System.Process           (CreateProcess (..),
                                           ProcessHandle (..), StdStream (..),
                                           callProcess, getProcessExitCode,
                                           proc, spawnProcess, terminateProcess,
                                           waitForProcess, withCreateProcess)
import qualified System.Process.Internals as SPI (ProcessHandle__ (..),
                                                  withProcessHandle)

getPid ph = SPI.withProcessHandle ph go
  where
    go ph_ = case ph_ of
               SPI.OpenHandle x   -> return $ Just x
               SPI.ClosedHandle _ -> return Nothing

data ExecutionConfig =
  ExecutionConfig { ecCommand   :: FilePath
                  , ecArguments :: [String]
                  , ecTimeout   :: Int
                  } deriving (Eq, Show)

data ExecutionResult =
  ExecutionResult { erOutput   :: String
                  , erError    :: String
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
      threadDelay (500 * 1000)
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
          -> (ExecutionResult -> IO a)
          -> IO a
onlyValue cfg process = fst <$> execute cfg process

onlyResult :: ((ExecutionResult -> IO (Maybe ExecutionResult))
                                -> IO (Maybe ExecutionResult, ExecutionResult))
            -> IO ExecutionResult
onlyResult runCmd = snd <$> runCmd (return . Just)

withinTemporaryDirectory :: IO a -> IO a
withinTemporaryDirectory action =
  withSystemTempDirectory "execute-" $ \temporaryDirectory -> do
    initialDirectory <- getCurrentDirectory
    setCurrentDirectory temporaryDirectory
    actionResult <- action
    setCurrentDirectory initialDirectory
    return actionResult


execute :: ExecutionConfig
        -> (ExecutionResult -> IO a)
        -> IO (a, ExecutionResult)
execute cfg processResult = do
  let executable = ecCommand cfg
      args       = ecArguments cfg
      timeout    = ecTimeout cfg
  withinTemporaryDirectory $ do
    startExecution <- getCurrentTime
    withCreateProcess
      (proc executable args) { std_out = CreatePipe, std_err = CreatePipe }
      (\_ (Just stdout) (Just stderr) ph -> do
          forkIO $ do
            threadDelay (timeout * 1000000)
            pid <- getPid ph
            forM_ pid $
              \p -> do
                putStrLn $ "Killing process: " ++ show p
                spawnProcess "rkill" ["-9", show p] >>= waitForProcess
          (out, err) <- readProcessStream ph (stdout, stderr)
          waitForProcess ph
          finishedExecution <- getCurrentTime
          let er = ExecutionResult { erOutput = unpack out
                                   , erError  = unpack err
                                   , erStart = startExecution
                                   , erFinished = finishedExecution
                                   , erConfig = cfg
                                   }
          (\r -> (r, er)) <$> processResult er)
