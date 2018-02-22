module Reagan.Serializer ( serialize
                         )where

import           Control.Monad    (when)
import           Data.Foldable    (forM_)
import           Data.List        (groupBy, sortBy)
import Data.Bifunctor (first, second, bimap)
import           Data.Maybe       (catMaybes, isNothing, isJust, fromJust)
import           System.Directory (copyFile, createDirectory,
                                   getCurrentDirectory, removeFile)
import           System.FilePath  ((</>))

import           Reagan.Compiler
import           Reagan.Execution
import           Reagan.Generator

data DivergenceReport =
  DivergenceReport { drFailedCompilation :: [String]
                   , drWrongChecksum     :: [(String, Int)]
                   } deriving (Show, Eq)

isDivergent :: DivergenceReport -> Bool
isDivergent DivergenceReport { drFailedCompilation = [], drWrongChecksum = [] } = False
isDivergent _ = True

serialize :: (GeneratedProgram,
                    [(CompiledProgram, Maybe ExecutionWithChecksum)])
          -> IO ()
serialize (prg, rest) = do
  cwd <- getCurrentDirectory
  case gpSeed prg of
    Nothing -> error "Program generation failed"
    Just seed -> do
      let rootPath = cwd </> "csmith_seed_" ++ show seed
      createDirectory rootPath
      writeFile (rootPath </> "generator.out") (show prg)
      copyFile (gpProgramPath prg) (rootPath </> "program.c")
      removeFile (gpProgramPath prg)
      mapM_ (serializeC rootPath) rest
      let report = DivergenceReport { drFailedCompilation = getFailedComputations rest
                                    , drWrongChecksum = getWrongChecksums rest
                                    }
      when (isDivergent report) $
        writeFile (rootPath </> "divergence.info") (show report)

serializeC :: FilePath
           -> (CompiledProgram, Maybe ExecutionWithChecksum)
           -> IO ()
serializeC rootPath (compiledProgram, rest) = do
  let prefix = cpCompilerTag compiledProgram
  writeFile (rootPath </> prefix ++ "_compiler.out") (show compiledProgram)
  forM_ rest $
      writeFile (rootPath </> prefix ++ "_execution.out") . show
  forM_ (cpExecutablePath compiledProgram) removeFile

getFailedComputations :: [(CompiledProgram, Maybe ExecutionWithChecksum)]
                      -> [String]
getFailedComputations runs =
  map (cpCompilerTag . fst) $ filter (isNothing . snd) runs

getWrongChecksums :: [(CompiledProgram, Maybe ExecutionWithChecksum)]
                  -> [(String, Int)]
getWrongChecksums runs =
  let gexs = groupBy eql $ -- group them by checksum
             sortBy cmp $ -- sort entries by checksum
             map (bimap cpCompilerTag (fromJust . ewcChecksum)) $ -- remove the just
             filter (\(c, e) -> isJust (ewcChecksum e)) $ -- filter out things that failed to run
             map (second fromJust) $ -- remove the just (safe because of previous filter)
             filter (isJust . snd) runs -- filter out things that failed to run
{- If there is just one group it means that:
a) we are not diverging since all checksums match
b) we only have one compiler result (one group of one) because the others failed to run, in which case there is no divergence either
So as a result I don't trigger a divergence if there only one element after the grouping.
-}
  in if length gexs == 1
       then []
       else map head $
            filter (\g -> length g == 1) gexs -- we care to see the cases where we diverge.
  where
    cmp (_, e1) (_, e2) = compare e1 e2
    eql (_, e1) (_, e2) = e1 == e2

