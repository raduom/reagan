module Main where

import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import           Data.Void                  (Void)
import           System.FilePath            (replaceExtension, takeBaseName)
import           System.FilePath.Glob
import           Test.Tasty                 (TestTree, defaultMain, testGroup)
import           Test.Tasty.Golden          (findByExtension, goldenVsString)
import           Text.Megaparsec            (Parsec)
import           Text.Megaparsec.Error      (errorBundlePretty)

import           Reagan.Compiler            (CompiledProgram (..))
import           Reagan.Query               (Result (..), loadResult, parseFile)
import           Reagan.Query.KCC           (parseCompilation, parseExecution)

main :: IO ()
main = defaultMain =<< allTests

parse :: Show a
      => Parsec Void LBS.ByteString a
      -> FilePath
      -> IO LBS.ByteString
parse parser file = do
  filename <- findGoldenInput  file
  result   <- parseFile parser filename
  case result of
           Left err -> putStrLn (errorBundlePretty err) >> pure LBS.empty
           Right cc -> pure (LC8.pack (show cc))

findGoldenInput :: String -> IO FilePath
findGoldenInput filename =
  head . head <$> globDir [compile filename] "test/data/parser"

allTests :: IO TestTree
allTests = pure $ testGroup "Reagan (all)" [parserTests, loaderTests]

parserTests :: TestTree
parserTests = testGroup "Parser"    [kccTests]

packOutput :: (a -> String) -> a -> LBS.ByteString
packOutput f a = LC8.pack (f a)

compilerOutput :: (Result -> String)
compilerOutput = cpError . rCompiler

compilers :: [String]
compilers = ["kcc", "clang", "gcc", "ccomp"]

loaderTests :: TestTree
loaderTests =
  testGroup "Loader" $
    [ goldenVsString
      ("Loading " ++ c ++ " compilation output")
      ("test/data/loader/" ++ c ++ "_default_compiler.out")
      (packOutput (show . rCompiler) <$> loadResult "test/data/loader" (c ++ "_default"))
    | c <- compilers ]

    ++

    [  goldenVsString
      ("Loading " ++ c ++ " execution output")
      ("test/data/loader/" ++ c ++ "_default_compiler.out")
      (packOutput (show . rCompiler) <$> loadResult "test/data/loader" (c ++ "_default"))
    | c <- compilers ]

kccTests :: TestTree
kccTests =
  testGroup "KCC"
    [ goldenVsString
        "Parse compilation output"
        "test/data/parser/kcc_compile.txt.out"
        (parse parseCompilation "kcc_compile.txt")
    , goldenVsString
        "Parse execution output"
        "test/data/parser/kcc_exec.txt.out"
        (parse parseExecution "kcc_exec.txt")
    ]
