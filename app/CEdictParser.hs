{-# LANGUAGE OverloadedStrings #-}

module CEdictParser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import Control.Monad (void)

-- Type alias for our parser
type Parser = Parsec Void Text

-- Data type representing a CEDICT entry
data CedictEntry = CedictEntry
  { ceTraditional :: Text      -- Traditional Chinese characters
  , ceSimplified :: Text       -- Simplified Chinese characters
  , cePronunciation :: Text    -- Pinyin pronunciation
  , ceMeanings :: [Text]       -- English meanings (split by /)
  } deriving (Show, Eq)

-- Parse a single CEDICT entry
parseCedictEntry :: Parser CedictEntry
parseCedictEntry = do
  traditional <- parseChineseText
  space1
  simplified <- parseChineseText
  space1
  pronunciation <- parsePronunciation
  space1
  CedictEntry traditional simplified pronunciation <$> parseMeanings

-- Parse Chinese characters (until whitespace)
parseChineseText :: Parser Text
parseChineseText = T.pack <$> some (satisfy (not . isSpace))
  where
    isSpace c = c == ' ' || c == '\t'

-- Parse pronunciation in square brackets [he2 tong2]
parsePronunciation :: Parser Text
parsePronunciation = do
  _ <- char '['
  T.pack <$> manyTill anySingle (char ']')

-- Parse meanings between forward slashes /meaning1/meaning2/.../
parseMeanings :: Parser [Text]
parseMeanings = do
  _ <- char '/'
  meanings <- manyTill parseMeaning (lookAhead (void eol <|> eof))
  return $ filter (not . T.null) meanings

-- Parse a single meaning (text until the next '/')
parseMeaning :: Parser Text
parseMeaning = do
  meaning <- T.pack <$> manyTill anySingle (char '/')
  return $ T.strip meaning

-- Parse multiple CEDICT entries (entire file)
parseCedictFile :: Parser [CedictEntry]
parseCedictFile = do
  entries <- many parseCedictLine
  eof
  return entries

-- Parse a single line, handling comments and empty lines
parseCedictLine :: Parser CedictEntry
parseCedictLine = do
  skipComments
  entry <- parseCedictEntry
  void eol <|> eof
  return entry

-- Skip comment lines (starting with #) and empty lines
skipComments :: Parser ()
skipComments = skipMany (commentLine <|> emptyLine)
  where
    commentLine = do
      _ <- char '#'
      _ <- manyTill anySingle (void eol <|> eof)
      return ()
    emptyLine = do
      _ <- some (char ' ' <|> char '\t')
      void eol <|> eof
      return ()

-- Convenience functions for parsing from different sources

-- Parse CEDICT from a file
parseCedictFromFile :: FilePath -> IO (Either (ParseErrorBundle Text Void) [CedictEntry])
parseCedictFromFile filePath = do
  content <- T.readFile filePath
  return $ parse parseCedictFile filePath content

-- Parse a single CEDICT line from text
parseCedictFromText :: Text -> Either (ParseErrorBundle Text Void) CedictEntry
parseCedictFromText = parse parseCedictEntry "input"

-- Parse multiple lines from text
parseCedictLinesFromText :: Text -> Either (ParseErrorBundle Text Void) [CedictEntry]
parseCedictLinesFromText = parse parseCedictFile "input"

-- Pretty printing functions

-- Pretty print a single entry
prettyPrintEntry :: CedictEntry -> Text
prettyPrintEntry entry = T.unlines
  [ "Traditional: " <> ceTraditional entry
  , "Simplified:  " <> ceSimplified entry
  , "Pinyin:      " <> cePronunciation entry
  , "Meanings:    " <> T.intercalate "; " (ceMeanings entry)
  , "─────────────────────────────────────────────────"
  ]

-- Pretty print multiple entries
prettyPrintEntries :: [CedictEntry] -> Text
prettyPrintEntries = T.concat . map prettyPrintEntry

-- Utility functions for querying parsed data

-- Find entries by traditional character
findByTraditional :: Text -> [CedictEntry] -> [CedictEntry]
findByTraditional traditional = filter (\e -> ceTraditional e == traditional)

-- Find entries by simplified character
findBySimplified :: Text -> [CedictEntry] -> [CedictEntry]
findBySimplified simplified = filter (\e -> ceSimplified e == simplified)

-- Find entries containing a meaning (case-insensitive substring search)
findByMeaning :: Text -> [CedictEntry] -> [CedictEntry]
findByMeaning searchTerm = filter containsMeaning
  where
    lowerSearchTerm = T.toLower searchTerm
    containsMeaning entry = any (T.isInfixOf lowerSearchTerm . T.toLower) (ceMeanings entry)

-- Find entries by pinyin pronunciation (exact match)
findByPronunciation :: Text -> [CedictEntry] -> [CedictEntry]
findByPronunciation pronunciation = filter (\e -> cePronunciation e == pronunciation)

-- Example usage and testing
mainCEdictParser :: IO ()
mainCEdictParser = do
  -- Test with sample data
  let sampleData = T.unlines
        [ "河童 河童 [he2 tong2] /kappa, a child-size humanoid water creature in Japanese folklore/"
        , "河粉 河粉 [he2 fen3] /hor fun, a type of wide, flat rice noodle/"
        , "河蚌 河蚌 [he2 bang4] /mussels/bivalves grown in rivers and lakes/"
        ]

  case parseCedictLinesFromText sampleData of
    Left err -> putStrLn $ "Parse error: " ++ errorBundlePretty err
    Right entries -> do
      putStrLn $ "Successfully parsed " ++ show (length entries) ++ " entries:"
      T.putStrLn $ prettyPrintEntries entries

      -- Demonstrate query functions
      putStrLn "=== Query Examples ==="
      putStrLn "Entries with '河童':"
      T.putStrLn $ prettyPrintEntries $ findByTraditional "河童" entries

      putStrLn "Entries containing 'water':"
      T.putStrLn $ prettyPrintEntries $ findByMeaning "water" entries

-- Alternative parser for handling the entire cedict_ts.u8 file format
-- This version is more robust for handling the actual file structure
parseRobustCedictFile :: Parser [CedictEntry]
parseRobustCedictFile = do
  entries <- many parseRobustLine
  eof
  return $ concat entries
  where
    parseRobustLine = choice
      [ [] <$ skipCommentLine
      , [] <$ skipEmptyLine
      , (:[]) <$> parseCedictEntry <* (void eol <|> eof)
      ]

    skipCommentLine = do
      _ <- char '#'
      _ <- manyTill anySingle (void eol <|> eof)
      return ()

    skipEmptyLine = do
      _ <- many (char ' ' <|> char '\t')
      void eol <|> eof
      return ()

-- Robust file parser function
parseCedictFileRobust :: FilePath -> IO (Either (ParseErrorBundle Text Void) [CedictEntry])
parseCedictFileRobust filePath = do
  content <- T.readFile filePath
  return $ parse parseRobustCedictFile filePath content