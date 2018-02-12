{-# LANGUAGE OverloadedStrings #-}
module Reagan.Run (runWithTimeout) where

import
  Control.Concurrent       (forkIO, threadDelay)
import qualified Data.ByteString          as BS
import           Data.Maybe
import           Data.Text                (Text, unpack)
import           Data.Text.IO             (hGetContents)
import Data.Text.Encoding (decodeUtf8)
import           System.Exit              (ExitCode (..))
import           System.IO                (Handle, hClose)
import           System.Process           (CreateProcess (..), StdStream (..),
                                           getProcessExitCode, proc,
                                           terminateProcess, waitForProcess,
                                           withCreateProcess)
import           System.Process.Internals
import Data.Monoid

getPid ph = withProcessHandle ph go
  where
    go ph_ = case ph_ of
               OpenHandle x   -> return $ Just x
               ClosedHandle _ -> return Nothing

readProcessStream :: ProcessHandle -> (Handle, Handle) -> IO (BS.ByteString, BS.ByteString)
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

runWithTimeout :: Int -> FilePath -> [Text] -> IO (Text, Text)
runWithTimeout timeout executable args = do
  putStrLn $ "Running: " ++ executable ++ " " ++ (show args)
  withCreateProcess
    (proc executable (map unpack args)) { std_out = CreatePipe, std_err = CreatePipe }
    (\_ (Just stdout) (Just stderr) ph -> do
        forkIO $ do
          threadDelay (timeout * 1000000)
          terminateProcess ph
        (out, err) <- readProcessStream ph (stdout, stderr)
        waitForProcess ph
        return (decodeUtf8 out, decodeUtf8 err))

