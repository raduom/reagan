module Main where

import Data.Function ((&))
import System.IO (stdin)
import Data.Char (ord, chr)

import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.FileSystem.File as File
import qualified Streamly.Mem.Array as A
import qualified Streamly.Fold as FL
import qualified Streamly.Prelude as S

import Reagan.Query.UB
import Reagan.Query

serialize :: UndefinedBehaviour -> String
serialize (UndefinedBehaviour desc seed) =
  seed ++ " : " ++ desc ++ "\n"

main :: IO ()
main =
    FH.read stdin
  & S.map (chr . fromIntegral)
  & FL.splitOn (A.fromList ['\n']) FL.toList
  & repositoryStream ["kcc_default"]
  & queryUndefinedBehaviours
  & S.map (A.fromList . serialize)
  & A.unlinesArraysBy '\n'
  & S.map (fromIntegral . ord)
  & File.write "ub-query.out"
