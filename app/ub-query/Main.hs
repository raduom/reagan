module Main where

import Reagan.Query.UB

serialize :: UndefinedBehaviour -> String
serialize (UndefinedBehaviour desc seed) =
  seed ++ " : " ++ desc ++ "\n"

main :: IO ()
main = do
  contents <- concatMap serialize <$> queryUndefinedBehaviours "."
  writeFile "ub-query.out" contents
