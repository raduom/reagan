module Runner where



import Generator
import Compiler
import Compiler.CLang

import Shelly

data RunnerConfig =
  RunnerConfig { rcTimeout :: Int
               , rcGeneratorConfig :: GeneratorConfig
               , rcCompilers :: [GeneratedProgram -> IO ExecutionResult]
               }

runOnce :: RunnerConfig -> IO [ExecutionResult]
runOnce cfg = do
  prg <- generateProgram (rcGeneratorConfig cfg)
  mapM (\f -> f prg) (rcCompilers cfg)

runMany :: RunnerConfig -> Int -> IO [(GeneratedProgram, [ExecutionResult])]
runMany cfg n = go n
  where
    go :: Int -> IO [(GeneratedProgram, [ExecutionResult])]
    go 0 = pure []
    go n = do
      prg <- generateProgram (rcGeneratorConfig cfg)
      result <- mapM (\f -> f prg) (rcCompilers cfg)
      results <- go (n - 1)
      return $ (prg, result) : results

config0 :: RunnerConfig
config0 = RunnerConfig { rcTimeout = 5
                       , rcGeneratorConfig = defaultConfig
                       , rcCompilers = [compile (CLangT 5)]
                       }
