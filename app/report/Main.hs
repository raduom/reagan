module Main where

import System.Environment (getArgs)

import Reagan.Report.ConstraintViolation (runReport)

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ "Running report on directory: " ++ head args
  report <- runReport (head args)
  putStrLn "Done."
