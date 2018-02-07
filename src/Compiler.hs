module Compiler ( Compiler(..)
                , ExecutionResult(..)
                ) where

import           Data.Text (Text)

import           Generator

data ExecutionResult =
   ExecutionResult { erStdOutput :: Text
                   , erErrOutput :: Text
                   , erChecksum  :: Maybe Text
                   } deriving (Show, Eq)

class Compiler a where
  compile :: a -> GeneratedProgram -> IO ExecutionResult
