module Main where

import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import           Data.Maybe                 (fromJust)
import           Data.Void                  (Void)
import           System.FilePath            (replaceExtension, takeBaseName)
import           System.FilePath.Glob
import           Test.Tasty                 (TestTree, defaultMain, testGroup)
import           Test.Tasty.Golden          (findByExtension, goldenVsString)
import           Text.Megaparsec            (Parsec)
import           Text.Megaparsec.Error      (errorBundlePretty)

import           Reagan.Compiler            (CompiledProgram (..))
import           Reagan.Generator           (GeneratedProgram (..))
import           Reagan.Query               (Result (..), loadDirectory,
                                             loadGenerator, loadResult,
                                             parseFile)
import           Reagan.Query.KCC           (parseCompilation, parseExecution)
import           Reagan.Query.UB            (runUndefinedBehaviours)

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
allTests = pure $ testGroup "Reagan (all)" [parserTests, loaderTests, queryTests]

parserTests :: TestTree
parserTests = testGroup "Parser"    [kccParserTests]

packOutput :: (a -> String) -> a -> LBS.ByteString
packOutput f a = LC8.pack (f a)

compilerOutput :: (Result -> String)
compilerOutput = cpError . fromJust . rCompiler

loaderTests :: TestTree
loaderTests =
  testGroup "Loader" $
    [ goldenVsString
        "Generator output"
        "test/data/loader/generator.out"
        (packOutput show
          <$> loadGenerator "test/data/loader")
    ]

    ++

    [ goldenVsString
        (c ++ " compilation output")
        ("test/data/loader/" ++ c ++ "_compiler.out")
        (out (fromJust . rCompiler) c)
    | c <- compilers ]

    ++

    [ goldenVsString
        (c ++ " execution output")
        ("test/data/loader/" ++ c ++ "_execution.out")
        (out (fromJust . rExecution) c)
    | c <- compilers ]

    ++

    [ goldenVsString
        "Multiple results"
        "test/data/query/loader.out"
        (packOutput show <$> loadDirectory "test/data/query" compilers)
    ]

  where
    out :: (Read a, Show a)
        => (Result -> a)
        -> String
        -> IO LBS.ByteString
    out extract suffix = do
      generatedProgram <- loadGenerator "test/data/loader"
      packOutput (show . extract) <$>
        loadResult "test/data/loader" generatedProgram suffix

    compilers :: [String]
    compilers = map (++ "_default")
                    ["kcc", "clang", "gcc", "ccomp"]

kccParserTests :: TestTree
kccParserTests =
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



queryTests :: TestTree
queryTests =
  testGroup "Query"
    [ goldenVsString
        "Undefined behaviours"
        "test/data/query/ub.out"
        (packOutput show <$> runUndefinedBehaviours "test/data/query")
    ]
