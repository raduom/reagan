module Reagan.Query.CV where

import           Conduit
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as LC8
import           Data.List                  (isPrefixOf)
import           Data.Maybe                 (mapMaybe)
import           Data.Void                  (Void)
import           Data.Word                  (Word8)
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
runConstraintViolations repository = runConduitRes $
     repositoryStream repository ["kcc_default"]
  .| queryConstraintViolations
  .| sinkList

queryConstraintViolations :: (MonadIO m)
                         => ConduitT Result ConstraintViolation m ()
queryConstraintViolations =
     mapC getConstraintViolations
  .| concatC

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
extract (Skim seed msgs) = map extractMessage msgs
  where
    extractMessage :: CompilationMessage -> ConstraintViolation
    extractMessage (CompilationMessage _ _ msg _) =
      ConstraintViolation { cvDescription = msg, cvSeed = seed }

isUndefinedBehaviour :: ExecutionMessage -> Bool
isUndefinedBehaviour (ExecutionMessage _ _ (Reference name _ _)) =
  "Undefined behavior" `isPrefixOf` name
