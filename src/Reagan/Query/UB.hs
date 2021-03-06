module Reagan.Query.UB where

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

import           Reagan.Execution           (ExecutionWithChecksum (..))
import           Reagan.Generator           (GeneratedProgram (..))
import           Reagan.Query
import           Reagan.Query.KCC

data UndefinedBehaviour = UndefinedBehaviour
  { ubDescription :: String
  , ubSeed        :: String
  } deriving (Show, Read)

data Skim = Skim
  { sSeed :: String
  , sMsgs :: [ExecutionMessage]
  } deriving (Show, Read)

runUndefinedBehaviours :: FilePath -> IO [UndefinedBehaviour]
runUndefinedBehaviours repository = runConduitRes $
     sourceDirectory repository
  .| repositoryStream ["kcc_default"]
  .| queryUndefinedBehaviours
  .| sinkList

queryUndefinedBehaviours :: (MonadIO m)
                         => ConduitT Result UndefinedBehaviour m ()
queryUndefinedBehaviours =
     mapC getUndefinedBehaviours
  .| concatC

getUndefinedBehaviours :: Result -> [UndefinedBehaviour]
getUndefinedBehaviours result = maybe [] extract (focus result)

focus :: Result -> Maybe Skim
focus (Result _ (Just execution) generator _) =
  case gpSeed generator of
    Nothing   -> error "No seed found in generator."
    Just seed ->
      let  fileLocation = show seed ++ "/execution"
           contents     = LC8.pack (ewcError execution)
      in case runParser parseExecution fileLocation contents of
        Left err   -> error $ errorMessage execution err
        Right msgs -> Just $ Skim { sSeed = show seed, sMsgs = msgs }
  where
    errorMessage :: ExecutionWithChecksum
                 -> ParseErrorBundle BSL.ByteString Void
                 -> String
    errorMessage exec err =
         "Failed to parse: "
      ++ ewcError exec
      ++ "\n with error: \n"
      ++ errorBundlePretty err
focus _ = Nothing

extract :: Skim -> [UndefinedBehaviour]
extract (Skim seed msgs) = mapMaybe extractMessage msgs
  where
    extractMessage :: ExecutionMessage -> Maybe UndefinedBehaviour
    extractMessage em@(ExecutionMessage msg _ _) =
      if isUndefinedBehaviour em
      then Just $ UndefinedBehaviour { ubDescription = msg, ubSeed = seed }
      else Nothing

isUndefinedBehaviour :: ExecutionMessage -> Bool
isUndefinedBehaviour (ExecutionMessage _ _ (Reference name _ _)) =
  "Undefined behavior" `isPrefixOf` name
