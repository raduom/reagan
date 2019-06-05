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
  } deriving (Show)

data Severity = Error | Note | Warning deriving (Show)

data Reference = Reference
  { rName     :: String
  , rCode     :: String
  , rSections :: [String]
  } deriving (Show)

data CompilationMessage = CompilationMessage
  { cmLocation  :: Location
  , cmSeverity  :: Severity
  , cmMessage   :: String
  , cmReference :: Either Reference String
  } deriving (Show)

data StackFrame = StackFrame
  { sfLocation :: Location
  , sfFunction :: String
  } deriving (Show)

data Execution = Execution
  { eMessage   :: String
  , eCode      :: String
  , eStack     :: [StackFrame]
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

tryOptional :: Parser a -> Parser (Maybe a)
tryOptional p = try (Just <$> p) <|> pure Nothing

tryWithDefault :: a -> Parser a -> Parser a
tryWithDefault d p = fromMaybe d <$> tryOptional p

toNewline :: Parser String
toNewline = word8ToString <$> lexeme (someTill printChar eol)

location :: Parser Location
location = do
  void $ tryOptional functionLocation
  void $ tryOptional header
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
  void (symbol ":")
  sections <- many section
  pure $ Reference name code sections

section :: Parser String
section = string "see" >> toNewline

compilationMessage :: Parser CompilationMessage
compilationMessage = do
  loc <- location
  sev <- severity
  msg <- toNewline
  ref <- Left <$> try (lexeme reference) <|>
           Right <$> codeSnippet
  return $ CompilationMessage
    { cmLocation = loc
    , cmSeverity = sev
    , cmMessage  = msg
    , cmReference  = ref
    }

codeSnippet :: Parser String
codeSnippet =
  lexeme $ tryWithDefault "" $ toNewline <* string "^"

functionLocation :: Parser String
functionLocation = do
  void (someTill asciiChar (symbol ":"))
  void (lexeme (string "In function '"))
  name <- word8ToString <$>
     manyTill printChar (symbol "'")
  void (symbol ":")
  pure name

header :: Parser ()
header = do
  void $ string "In file"
  void toNewline
  void $ many (string "from" >> toNewline)
  void $ tryOptional (manyTill printChar (string "At top level:"))

parseCompilation :: Parser [CompilationMessage]
parseCompilation = many (lexeme compilationMessage) <* eof

-- Execution

data ExecutionMessage = ExecutionMessage
  { emMessage   :: String
  , emLocation  :: [String]
  , emReference :: Reference
  } deriving (Show)

executionMessage :: Parser ExecutionMessage
executionMessage =
  ExecutionMessage <$> toNewline
                   <*> errorStack
                   <*> reference

checksum :: Parser String
checksum =
  string "checksum = " *> toNewline

errorStack :: Parser [String]
errorStack = do
  void (string "> ")
  some (string "in" *> toNewline)

parseExecution :: Parser [ExecutionMessage]
parseExecution =
  many (try executionMessage) <* checksum <* eof
