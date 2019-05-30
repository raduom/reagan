{-# LANGUAGE OverloadedStrings #-}
module Reagan.Query.KCC where

import Control.Applicative hiding (some, many)
import Control.Monad
import Control.Monad.Combinators
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L

import Data.ByteString.Lazy hiding (empty)
import qualified Data.ByteString.Lazy.Char8 as LC8 hiding (empty)
import Data.Void (Void)
import Data.Word (Word8)
import Data.Functor (($>))

data Location = Location
  { lcFilename :: String
  , lcLineNumber :: Integer
  , lcColumn :: Integer
  } deriving (Show)

data Severity = Error | Warning deriving (Show)

data Reference = Reference
  { rName :: String
  , rCode :: String
  , rSections :: [String]
  } deriving (Show)

data Message = Message
  { mLocation  :: Location
  , mSeverity  :: Severity
  , mMessage   :: String
  , mReference :: Either Reference String
  } deriving (Show)

data StackFrame = StackFrame
  { sfLocation :: Location
  , sfFunction :: String
  } deriving (Show)

data Execution = Execution
  { eMessage :: String
  , eCode    :: String
  , eStack   :: [StackFrame]
  , eReference :: Reference
  } deriving (Show)

type Parser = Parsec Void ByteString

sc :: Parser ()
sc = L.space
  space1
  empty
  empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: ByteString -> Parser ByteString
symbol = L.symbol sc

word8ToString :: [Word8] -> String
word8ToString = LC8.unpack . pack

location :: Parser Location
location = do
  void (opt functionLocation)
  filename   <- word8ToString <$>
                  someTill asciiChar (symbol ":")
  lineNumber <- L.decimal
  void (symbol ":")
  column     <- L.decimal
  void (symbol ":")
  pure $ Location filename lineNumber column

severity :: Parser Severity
severity =
  string "error" $> Error <|>
    string "warning" $> Warning

reference :: Parser Reference
reference = do
  name     <- word8ToString <$> someTill printChar (symbol "(")
  code     <- word8ToString <$> someTill printChar (symbol ")")
  void (symbol ":")
  sections <- many section
  pure $ Reference name code sections

section :: Parser String
section = do
  void (string "see")
  word8ToString <$> lexeme (someTill printChar eol)

message :: Parser Message
message = do
  loc <- location
  sev <- severity
  void (symbol ":")
  msg <- word8ToString <$> someTill printChar newline
  void (many newline)
  ref <- Left <$> try reference <|>
           Right . word8ToString <$> someTill printChar newline
  return $ Message
    { mLocation = loc
    , mSeverity = sev
    , mMessage  = msg
    , mReference  = ref
    }

opt :: Parser a -> Parser (Maybe a)
opt p = try (Just <$> p) <|> pure Nothing

functionLocation :: Parser String
functionLocation = do
  void (someTill asciiChar (symbol ":"))
  void (lexeme (string "In function '"))
  name <- word8ToString <$>
     manyTill printChar (symbol "'")
  void (symbol ":")
  pure name


parseCompilation :: Parser [Message]
parseCompilation = many (lexeme message)
