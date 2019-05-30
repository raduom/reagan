module Reagan.Query where

import Text.Megaparsec hiding (State)
import qualified Data.ByteString.Lazy as LBS
import Data.Void

parseFile :: Parsec Void LBS.ByteString a
          -> FilePath
          -> IO (Either (ParseErrorBundle LBS.ByteString Void) a)
parseFile parser file =
  runParser parser file <$> LBS.readFile file
