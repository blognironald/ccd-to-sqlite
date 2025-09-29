{-# LANGUAGE OverloadedStrings #-}

module UnihanReadingsParser where

import Text.Megaparsec
import Text.Megaparsec.Char
-- import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import Data.Char (chr, isAsciiLower)
import Numeric (readHex)
import Control.Monad (void)
import Data.Maybe (catMaybes)
import Data.List (find)

-- Type alias for our parser
type Parser = Parsec Void Text

-- Data type representing a Unihan entry with definition
data UnihanEntry = UnihanEntry
  { ueCharacter :: Char        -- The actual Unicode character
  , ueCodePoint :: Text        -- The U+XXXX code point (for reference)
  , ueDefinition :: Text       -- The definition from kDefinition field
  } deriving (Show, Eq)

-- Parse the entire Unihan file, returning only entries with definitions
parseUnihanFile :: Parser [UnihanEntry]
parseUnihanFile = do
  entries <- many parseUnihanLine
  eof
  return $ catMaybes entries

-- Parse a single line, returning Nothing for non-definition lines
parseUnihanLine :: Parser (Maybe UnihanEntry)
parseUnihanLine = choice
  [ Nothing <$ parseCommentLine
  , Nothing <$ parseEmptyLine
  , parseDataLine
  ]

-- Parse a comment line (starting with #)
parseCommentLine :: Parser ()
parseCommentLine = do
  _ <- char '#'
  _ <- manyTill anySingle (void eol <|> eof)
  return ()

-- Parse an empty line
parseEmptyLine :: Parser ()
parseEmptyLine = do
  _ <- many (char ' ' <|> char '\t')
  _ <- void eol <|> eof
  return ()

-- Parse a data line (could be any field, but we only care about kDefinition)
parseDataLine :: Parser (Maybe UnihanEntry)
parseDataLine = do
  codePoint <- parseCodePoint
  _ <- char '\t'
  fieldName <- parseFieldName
  _ <- char '\t'
  fieldValue <- parseFieldValue
  _ <- void eol <|> eof
  
  -- Only create an entry if it's a kDefinition field
  if fieldName == "kDefinition"
    then case codePointToChar codePoint of
      Just c -> return $ Just $ UnihanEntry c codePoint fieldValue
      Nothing -> return Nothing
    else return Nothing

-- Parse the Unicode code point (U+XXXX format)
parseCodePoint :: Parser Text
parseCodePoint = do
  _ <- string "U+"
  hex <- some hexDigitChar
  return $ T.pack $ "U+" ++ hex

-- Parse field name (e.g., kDefinition, kCantonese, etc.)
parseFieldName :: Parser Text
parseFieldName = T.pack <$> some (alphaNumChar <|> char '_')

-- Parse field value (everything until end of line)
parseFieldValue :: Parser Text
parseFieldValue = T.pack <$> manyTill anySingle (lookAhead (void eol <|> eof))

-- Convert a code point string to the actual character
codePointToChar :: Text -> Maybe Char
codePointToChar codePoint =
  case T.stripPrefix "U+" codePoint of
    Just hexStr ->
      case readHex (T.unpack hexStr) of
        [(value, "")] -> Just (chr value)
        _ -> Nothing
    Nothing -> Nothing

-- Alternative conversion function (for use in other contexts)
charToCodePoint :: Char -> Text
charToCodePoint c = T.pack $ "U+" ++ toHexString (fromEnum c)
  where
    toHexString n = let hex = showHex n ""
                    in map toUpper $ replicate (4 - length hex) '0' ++ hex
    showHex n s
      | n < 16 = hexDigit n : s
      | otherwise = showHex (n `div` 16) (hexDigit (n `mod` 16) : s)
    hexDigit n
      | n < 10 = chr (48 + n)
      | n < 16 = chr (55 + n)
      | otherwise = error "Invalid hex digit"
    toUpper c'
      | isAsciiLower c' = chr (fromEnum c' - 32)
      | otherwise = c'

-- Parse Unihan file from file path
parseUnihanFromFile :: FilePath -> IO (Either (ParseErrorBundle Text Void) [UnihanEntry])
parseUnihanFromFile filePath = do
  content <- T.readFile filePath
  return $ parse parseUnihanFile filePath content

-- Parse Unihan content from Text
parseUnihanFromText :: Text -> Either (ParseErrorBundle Text Void) [UnihanEntry]
parseUnihanFromText = parse parseUnihanFile "input"

-- Pretty printing functions

-- Pretty print a single entry
prettyPrintEntry :: UnihanEntry -> Text
prettyPrintEntry entry = T.unlines
  [ "Character:   " <> T.singleton (ueCharacter entry) <> " (" <> ueCodePoint entry <> ")"
  , "Definition:  " <> ueDefinition entry
  , "─────────────────────────────────────────────────"
  ]

-- Pretty print multiple entries
prettyPrintEntries :: [UnihanEntry] -> Text
prettyPrintEntries = T.concat . map prettyPrintEntry

-- Utility functions for querying parsed data

-- Find entry by character
findByCharacter :: Char -> [UnihanEntry] -> Maybe UnihanEntry
findByCharacter c = find (\e -> ueCharacter e == c)

-- Find entries containing a word in the definition (case-insensitive)
findByDefinitionWord :: Text -> [UnihanEntry] -> [UnihanEntry]
findByDefinitionWord searchTerm = filter containsWord
  where
    lowerSearchTerm = T.toLower searchTerm
    containsWord entry = T.isInfixOf lowerSearchTerm (T.toLower $ ueDefinition entry)

-- Get all characters with definitions as a Text string
getAllCharacters :: [UnihanEntry] -> Text
getAllCharacters entries = T.pack $ map ueCharacter entries

-- Statistics function
getStatistics :: [UnihanEntry] -> (Int, Int, Int)
getStatistics entries = (totalEntries, avgDefLength, maxDefLength)
  where
    totalEntries = length entries
    defLengths = map (T.length . ueDefinition) entries
    avgDefLength = if totalEntries > 0
                   then sum defLengths `div` totalEntries
                   else 0
    maxDefLength = if null defLengths then 0 else maximum defLengths

-- Example usage and testing
mainUnihanParser :: IO ()
mainUnihanParser = do
  -- Test with sample data
  let sampleData = T.unlines
        [ "# Comments should be ignored"
        , "U+3400\tkCantonese\tjau1"
        , "U+3400\tkDefinition\t(same as 丘) hillock or mound"
        , "U+3400\tkJapanese\tキュウ おか"
        , "U+3401\tkDefinition\tto lick; to taste, a mat, bamboo bark"
        , "U+3402\tkDefinition\t(non-standard Japanese variant of 喜), to like, love, enjoy; a joyful thing"
        , "U+3405\tkCantonese\tng5"
        , "U+3405\tkDefinition\t(ancient form of 五) five"
        , "# Another comment"
        , "U+340C\tkDefinition\ta tribe of savages in South China"
        ]

  case parseUnihanFromText sampleData of
    Left err -> putStrLn $ "Parse error: " ++ errorBundlePretty err
    Right entries -> do
      putStrLn $ "Successfully parsed " ++ show (length entries) ++ " entries with definitions:"
      T.putStrLn $ prettyPrintEntries entries

      -- Demonstrate query functions
      putStrLn "=== Query Examples ==="

      case entries of
        (firstEntry:_) -> do
          let c = ueCharacter firstEntry
          putStrLn $ "Looking up character: " ++ [c]
          case findByCharacter c entries of
            Just entry -> T.putStrLn $ prettyPrintEntry entry
            Nothing -> putStrLn "Character not found"
        _ -> putStrLn "No entries found"

      putStrLn "Entries containing 'ancient':"
      T.putStrLn $ prettyPrintEntries $ findByDefinitionWord "ancient" entries

      -- Show statistics
      let (total, avgLen, maxLen) = getStatistics entries
      putStrLn   "\n=== Statistics ==="
      putStrLn $ "Total entries with definitions: " ++ show total
      putStrLn $ "Average definition length: " ++ show avgLen ++ " characters"
      putStrLn $ "Maximum definition length: " ++ show maxLen ++ " characters"
      putStrLn $ "All characters: " ++ T.unpack (getAllCharacters entries)

-- Function to process a full Unihan file and save results
processUnihanFile :: FilePath -> FilePath -> IO ()
processUnihanFile inputPath outputPath = do
  putStrLn $ "Processing " ++ inputPath ++ "..."
  result <- parseUnihanFromFile inputPath
  case result of
    Left err -> putStrLn $ "Parse error: " ++ errorBundlePretty err
    Right entries -> do
      putStrLn $ "Found " ++ show (length entries) ++ " entries with definitions"
      T.writeFile outputPath (prettyPrintEntries entries)
      putStrLn $ "Results saved to " ++ outputPath

      -- Print some sample entries
      putStrLn "\nFirst 5 entries:"
      T.putStrLn $ prettyPrintEntries (take 5 entries)

-- Helper to extract all definitions as a list of (Character, Definition) pairs
extractDefinitions :: [UnihanEntry] -> [(Char, Text)]
extractDefinitions = map (\e -> (ueCharacter e, ueDefinition e))

-- Main function for testing
-- main :: IO ()
-- main = mainUnihanParser