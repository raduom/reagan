module Reagan.Report.ConstraintViolation ( accumulateReport
                                         , CompilerOutput(..)
                                         , ProgramReport(..)
                                         , runReport) where

import           Reagan.Compiler  (CompiledProgram (..))
import           Reagan.Execution (ExecutionWithChecksum)
import           Reagan.Generator (GeneratedProgram (..))
import           Reagan.Report    (TestResult (..), runReportOnDirectory)

import           Data.List        (find, isInfixOf)
import           Data.Maybe       (fromJust, maybe)

import Debug.Trace

data CompilerOutput =
  CompilerOutput { coTag    :: String
                 , coOutput :: [String]
                 } deriving (Eq, Show)

data ProgramReport =
  ProgramReport { prKccOutput      :: [String]
                , prSeed           :: Integer
                , prProgram        :: String
                , prCompilerOutput :: [CompilerOutput]
                } deriving (Eq, Show)

runReport :: FilePath -> IO [ProgramReport]
runReport = runReportOnDirectory accumulateReport []

accumulateReport :: [ProgramReport]
                 -> TestResult
                 -> [ProgramReport]
accumulateReport acc result =
  let output = getKccOutput (trCompilerOutput result)
  in  if null output
      then acc
      else processResult result : acc

processResult :: TestResult
              -> ProgramReport
processResult result =
  ProgramReport { prKccOutput = getKccOutput $ trCompilerOutput result
                , prSeed = fromJust $ gpSeed $ trGeneratorOutput result
                , prProgram = trProgram result
                , prCompilerOutput = map (convert . fst) $ trCompilerOutput result
                }
  where
    convert cp = CompilerOutput (cpCompilerTag cp) (lines $ cpError cp)

getKccOutput :: [(CompiledProgram, Maybe ExecutionWithChecksum)]
             -> [String]
getKccOutput =
  filter (isInfixOf "Constraint violation") .
  maybe [] (lines . cpError) .
  find (\cp -> cpCompilerTag cp == "kcc_default") .
  map fst
