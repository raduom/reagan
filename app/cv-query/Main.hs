module Main where

import Conduit
import Data.Conduit.Combinators

import qualified Data.ByteString.Char8 as LC8

import Reagan.Query.CV
import Reagan.Query

serialize :: ConstraintViolation -> String
serialize (ConstraintViolation desc seed) =
  seed ++ " : " ++ desc ++ "\n"

main :: IO ()
main = runConduitRes $
     stdin
  .| mapC LC8.unpack
  .| linesUnboundedC
  .| repositoryStream ["kcc_default"]
  .| queryConstraintViolations
  .| mapC (LC8.pack . serialize)
  .| sinkFile "cv-query.out"
