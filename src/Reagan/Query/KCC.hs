{-# LANGUAGE OverloadedStrings #-}
module Reagan.Query.KCC where

import           Control.Applicative        hiding (many, some)
import           Control.Monad
import           Control.Monad.Combinators
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L

import           Data.ByteString.Lazy       hiding (empty)
import qualified Data.ByteString.Lazy.Char8 as LC8 hiding (empty)
import           Data.Functor               (($>))
import           Data.Maybe                 (fromMaybe)
import           Data.Void                  (Void)
import           Data.Word                  (Word8)

data Location = Location
  { lcFilename   :: String
  , lcLineNumber :: Integer
  , lcColumn     :: Integer
  } deriving (Show, Read)

data Severity = Error | Note | Warning deriving (Show, Read)

data Reference = Reference
  { rName     :: String
  , rCode     :: String
  , rSections :: [String]
  } deriving (Show, Read)

data CompilationMessage = CompilationMessage
  { cmLocation  :: Location
  , cmSeverity  :: Severity
  , cmMessage   :: String
  , cmReference :: Reference
  } deriving (Show, Read)

data StackFrame = StackFrame
  { sfLocation :: Location
  , sfFunction :: String
  } deriving (Show, Read)


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

tryOptional :: Parser a -> Parser (Maybe a)
tryOptional p = try (Just <$> p) <|> pure Nothing

tryWithDefault :: a -> Parser a -> Parser a
tryWithDefault d p = fromMaybe d <$> tryOptional p

toNewline :: Parser String
toNewline = word8ToString <$> lexeme (someTill printChar eol)

location :: Parser Location
location = do
  filename   <- word8ToString <$>
                  someTill asciiChar (symbol ":")
  lineNumber <- L.decimal
  void (symbol ":")
  column     <- L.decimal
  void (symbol ":")
  pure $ Location filename lineNumber column

severity :: Parser Severity
severity =

  (string "error" $> Error
  <|> string "warning" $> Warning
  <|> string "note" $> Note) <* symbol ":"

reference :: Parser Reference
reference = do
  name     <- word8ToString <$> someTill printChar (symbol "(")
  code     <- word8ToString <$> someTill printChar (symbol ")")
  void (optional (symbol ":"))
  sections <- many section
  pure $ Reference name code sections

section :: Parser String
section = string "see" >> toNewline

compilationMessage :: Parser CompilationMessage
compilationMessage = do
  loc <- location
  sev <- severity
  msg <- toNewline
  ref <- lexeme reference
  return $ CompilationMessage
    { cmLocation = loc
    , cmSeverity = sev
    , cmMessage  = msg
    , cmReference  = ref
    }

parseCompilation :: Parser [CompilationMessage]
parseCompilation =
  many (try compilationMessage) <* (many toNewline) <* eof

-- Execution

data ExecutionMessage = ExecutionMessage
  { emMessage   :: String
  , emLocation  :: [String]
  , emReference :: Reference
  } deriving (Show, Read)

executionMessage :: Parser ExecutionMessage
executionMessage =
  ExecutionMessage <$> toNewline
                   <*> errorStack
                   <*> reference

unexpectedError :: Parser ByteString
unexpectedError =
  lexeme (string "Execution failed (configuration dumped)")

errorStack :: Parser [String]
errorStack = do
  void (string "> ")
  some (string "in" *> toNewline)

parseExecution :: Parser [ExecutionMessage]
parseExecution =
  many (try executionMessage) <* optional unexpectedError <* eof
