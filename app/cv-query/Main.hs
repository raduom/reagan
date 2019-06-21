{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Function ((&))
import System.IO (stdin)
import Data.Char (ord, chr)

import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.FileSystem.File as File
import qualified Streamly.Mem.Array as A
import qualified Streamly.Fold as FL
import qualified Streamly.Prelude as S

import Reagan.Query.CV
import Reagan.Query

serialize :: ConstraintViolation -> String
serialize (ConstraintViolation desc seed) =
  seed ++ " : " ++ desc ++ "\n"

main :: IO ()
main =
    FH.read stdin
  & S.map (chr . fromIntegral)
  & FL.splitOn (A.fromList ['\n']) FL.toList -- SerialT m [Word8]
  & repositoryStream ["kcc_default"]
  & queryConstraintViolations
  & S.map (A.fromList . serialize)
  & A.unlinesArraysBy '\n'
  & S.map (fromIntegral . ord)
  & File.write "cv-query.out"
