module Main where

import           Reagan.Compiler
import           Reagan.Generator
import           Reagan.Run

import           Data.Either
import           Data.Map (Map)

import           Pipes
import           Pipes.Prelude    (fold, take)

import           Prelude          (IO, id, map, print, ($), (>>=), Integer, undefined, Maybe, (/=), Maybe(..))

simpleTestProtocol :: GeneratorConfig -> Producer ExecutionResult IO ()
simpleTestProtocol cfg =
  for (generateProgram cfg)
      (\prg -> do
          (lift $ executeCLang 60 prg) >>= yield
          (lift $ executeKcc 60 prg) >>= yield)

divergenceProtocol :: GeneratorConfig -> Producer (Maybe Integer) IO ()
divergenceProtocol cfg =
  for (generateProgram cfg)
      (\prg -> do
          erCLang <- lift $ executeCLang 60 prg
          erKcc <- lift $ executeKcc 60 prg
          if (erChecksum erCLang /= erChecksum erKcc)
            then yield $ Just (erSeed erCLang)
            else yield Nothing)

main :: IO ()
main = do
  ers <- fold (\ers er -> er : ers) [] id $ (divergenceProtocol defaultConfig >-> take 6)
  print ers
