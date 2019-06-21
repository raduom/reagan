{-# LANGUAGE FlexibleContexts #-}
module Reagan.Query.CV where

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as LC8
import           Data.Function              ((&))
import           Data.List                  (isPrefixOf)
import           Data.Maybe                 (mapMaybe)
import           Data.Void                  (Void)
import           Streamly
import qualified Streamly.Prelude           as S
import           System.Directory           (listDirectory)
import           System.FilePath            ((</>))
import           Text.Megaparsec            (runParser)
import           Text.Megaparsec.Error      (ParseErrorBundle (..),
                                             errorBundlePretty)

import           Reagan.Compiler            (CompiledProgram (..))
import           Reagan.Generator           (GeneratedProgram (..))
import           Reagan.Query
import           Reagan.Query.KCC

data ConstraintViolation = ConstraintViolation
  { cvDescription :: String
  , cvSeed        :: String
  } deriving (Show, Read)

data Skim = Skim
  { sSeed :: String
  , sMsgs :: [CompilationMessage]
  } deriving (Show, Read)

runConstraintViolations :: FilePath -> IO [ConstraintViolation]
runConstraintViolations repository =
  listDirectory repository >>= \dirs ->
      S.fromList (map (repository </>) dirs)
    & repositoryStream ["kcc_default"]
    & queryConstraintViolations
    & S.toList

queryConstraintViolations :: (MonadAsync m)
                          => SerialT m Result
                          -> SerialT m ConstraintViolation
queryConstraintViolations resultS =
    S.concatMap S.fromList
  $ S.map getConstraintViolations resultS

getConstraintViolations :: Result -> [ConstraintViolation]
getConstraintViolations result = maybe [] extract (focus result)

focus :: Result -> Maybe Skim
focus (Result (Just compiler) _ generator _) =
  case gpSeed generator of
    Nothing   -> error "No seed found in generator."
    Just seed ->
      let  fileLocation = show seed ++ "/compiler"
           contents     = LC8.pack (cpError compiler)
      in case runParser parseCompilation fileLocation contents of
        Left err   -> error $ errorMessage compiler err
        Right msgs -> Just $ Skim { sSeed = show seed, sMsgs = msgs }
  where
    errorMessage :: CompiledProgram
                 -> ParseErrorBundle BSL.ByteString Void
                 -> String
    errorMessage exec err =
         "Failed to parse: "
      ++ cpError exec
      ++ "\n with error: \n"
      ++ errorBundlePretty err
focus _ = Nothing

extract :: Skim -> [ConstraintViolation]
extract (Skim seed msgs) = mapMaybe extractMessage msgs
  where
    extractMessage :: CompilationMessage -> Maybe ConstraintViolation
    extractMessage em@(CompilationMessage _ _ msg _) =
      if isConstraintViolation em
      then Just $ ConstraintViolation { cvDescription = msg, cvSeed = seed }
      else Nothing

isConstraintViolation :: CompilationMessage -> Bool
isConstraintViolation (CompilationMessage _ _ _ (Reference name _ _)) =
     "Constraint violation" `isPrefixOf` name
