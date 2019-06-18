module Main where

import Conduit
import qualified Data.ByteString.Char8 as LC8

import Reagan.Query.UB
import Reagan.Query

serialize :: UndefinedBehaviour -> String
serialize (UndefinedBehaviour desc seed) =
  seed ++ " : " ++ desc ++ "\n"

main :: IO ()
main = runConduitRes $
     sourceDirectory "."
  .| repositoryStream ["kcc_default"]
  .| queryUndefinedBehaviours
  .| mapC (LC8.pack . serialize)
  .| sinkFile "ub-query.out"
