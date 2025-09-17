{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module JMdictParser where

import Text.XML
import Text.XML.Cursor
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe 
-- import Control.Monad (guard)
-- import Data.List (find)

-- | Main JMdict entry structure
data JMdictEntry = JMdictEntry
  { entrySeq :: !Int                    -- ^ Unique sequence number
  , kanjiElements :: ![KanjiElement]    -- ^ Kanji headwords
  , readingElements :: ![ReadingElement] -- ^ Kana readings
  , senses :: ![Sense]                  -- ^ Meanings and translations
  , examples :: ![Example]              -- ^ Example sentences (from _examp version)
  } deriving (Show, Eq)

-- | Kanji element (headword with kanji)
data KanjiElement = KanjiElement
  { kanjiText :: !Text                  -- ^ The kanji headword
  , kanjiInfo :: ![Text]                -- ^ Additional kanji information
  , kanjiPriority :: ![Text]            -- ^ Priority markers
  } deriving (Show, Eq)

-- | Reading element (kana pronunciation)
data ReadingElement = ReadingElement
  { readingText :: !Text                -- ^ The kana reading
  , readingNoKanji :: !Bool             -- ^ True if reading has no associated kanji
  , readingRestrictions :: ![Text]      -- ^ Kanji this reading applies to
  , readingInfo :: ![Text]              -- ^ Additional reading information
  , readingPriority :: ![Text]          -- ^ Priority markers
  } deriving (Show, Eq)

-- | Sense (meaning/translation)
data Sense = Sense
  { senseRestrictions :: ![Text]        -- ^ Kanji/reading restrictions
  , crossReferences :: ![Text]          -- ^ Cross-references to other entries
  , antonyms :: ![Text]                 -- ^ Antonym references
  , partOfSpeech :: ![Text]             -- ^ Parts of speech
  , fields :: ![Text]                   -- ^ Field of application
  , miscInfo :: ![Text]                 -- ^ Miscellaneous information
  , dialectInfo :: ![Text]              -- ^ Dialect information
  , glosses :: ![Gloss]                 -- ^ Translation glosses
  , languageSources :: ![LanguageSource] -- ^ Source language information
  } deriving (Show, Eq)

-- | Translation gloss
data Gloss = Gloss
  { glossLang :: !(Maybe Text)          -- ^ Language code (default: eng)
  , glossGender :: !(Maybe Text)        -- ^ Gender information
  , glossType :: !(Maybe Text)          -- ^ Type of gloss
  , glossText :: !Text                  -- ^ The actual translation
  } deriving (Show, Eq)

-- | Language source information
data LanguageSource = LanguageSource
  { lsLang :: !Text                     -- ^ Source language
  , lsType :: !(Maybe Text)             -- ^ Type of borrowing
  , lsWasei :: !Bool                    -- ^ Wasei (Japanese-made) word
  , lsText :: !(Maybe Text)             -- ^ Source word
  } deriving (Show, Eq)

-- | Example sentence pair
data Example = Example
  { exampleJapanese :: !Text            -- ^ Japanese sentence
  , exampleEnglish :: !Text             -- ^ English translation
  , exampleSource :: !(Maybe Text)     -- ^ Source reference
  } deriving (Show, Eq)

-- | Parse JMdict_e_examp XML file
parseJMdictFile :: FilePath -> IO [JMdictEntry]
parseJMdictFile filePath = do
  doc <- Text.XML.readFile def filePath
  let cursor = fromDocument doc
  return $ parseJMdictEntries cursor

-- | Parse JMdict entries from XML cursor
parseJMdictEntries :: Cursor -> [JMdictEntry]
parseJMdictEntries cursor =
  cursor $// element "entry" &| parseEntry

-- | Parse a single entry
parseEntry :: Cursor -> JMdictEntry
parseEntry cursor = JMdictEntry
  { entrySeq = parseEntSeq cursor
  , kanjiElements = parseKanjiElements cursor
  , readingElements = parseReadingElements cursor
  , senses = parseSenses cursor
  , examples = parseExamples cursor
  }

-- | Parse entry sequence number
parseEntSeq :: Cursor -> Int
parseEntSeq cursor =
  case cursor $/ element "ent_seq" &/ content of
    (seqText:_) -> read $ T.unpack seqText
    [] -> 0

-- | Parse kanji elements
parseKanjiElements :: Cursor -> [KanjiElement]
parseKanjiElements cursor =
  cursor $/ element "k_ele" &| parseKanjiElement

parseKanjiElement :: Cursor -> KanjiElement
parseKanjiElement cursor = KanjiElement
  { kanjiText = getFirstContent cursor "keb"
  , kanjiInfo = getMultipleContent cursor "ke_inf"
  , kanjiPriority = getMultipleContent cursor "ke_pri"
  }

-- | Parse reading elements
parseReadingElements :: Cursor -> [ReadingElement]
parseReadingElements cursor =
  cursor $/ element "r_ele" &| parseReadingElement

parseReadingElement :: Cursor -> ReadingElement
parseReadingElement cursor = ReadingElement
  { readingText = getFirstContent cursor "reb"
  , readingNoKanji = not $ null $ cursor $/ element "re_nokanji"
  , readingRestrictions = getMultipleContent cursor "re_restr"
  , readingInfo = getMultipleContent cursor "re_inf"
  , readingPriority = getMultipleContent cursor "re_pri"
  }

-- | Parse sense elements
parseSenses :: Cursor -> [Sense]
parseSenses cursor =
  cursor $/ element "sense" &| parseSense

parseSense :: Cursor -> Sense
parseSense cursor = Sense
  { senseRestrictions = getMultipleContent cursor "stagr" ++ getMultipleContent cursor "stagk"
  , crossReferences = getMultipleContent cursor "xref"
  , antonyms = getMultipleContent cursor "ant"
  , partOfSpeech = getMultipleContent cursor "pos"
  , fields = getMultipleContent cursor "field"
  , miscInfo = getMultipleContent cursor "misc"
  , dialectInfo = getMultipleContent cursor "dial"
  , glosses = parseGlosses cursor
  , languageSources = parseLanguageSources cursor
  }

-- | Parse gloss elements
parseGlosses :: Cursor -> [Gloss]
parseGlosses cursor =
  cursor $/ element "gloss" &| parseGloss

parseGloss :: Cursor -> Gloss
parseGloss cursor = Gloss
  { glossLang = listToMaybe $ cursor $| attribute "xml:lang"
  , glossGender = listToMaybe $ cursor $| attribute "g_gend"
  , glossType = listToMaybe $ cursor $| attribute "g_type"
  , glossText = T.concat $ cursor $/ content
  }

-- | Parse language source elements
parseLanguageSources :: Cursor -> [LanguageSource]
parseLanguageSources cursor =
  cursor $/ element "lsource" &| parseLanguageSource

parseLanguageSource :: Cursor -> LanguageSource
parseLanguageSource cursor = LanguageSource
  { lsLang = fromMaybe "eng" $ listToMaybe $ cursor $| attribute "xml:lang"
  , lsType = listToMaybe $ cursor $| attribute "ls_type"
  , lsWasei = "y" `elem` (cursor $| attribute "ls_wasei")
  , lsText = case cursor $/ content of
             [] -> Nothing
             texts -> Just $ T.concat texts
  }

-- | Parse example sentences (specific to JMdict_e_examp)
parseExamples :: Cursor -> [Example]
parseExamples cursor =
  cursor $/ element "example" &| parseExample

parseExample :: Cursor -> Example
parseExample cursor = Example
  { exampleJapanese = getFirstContent cursor "ex_sent"
  , exampleEnglish = getFirstContent cursor "ex_sent"
  , exampleSource = case getMultipleContent cursor "ex_srce" of
                     [] -> Nothing
                     (x:_) -> Just x
  }

-- Helper functions

-- | Get first content of an element
getFirstContent :: Cursor -> Name -> Text
getFirstContent cursor elementName =
  case cursor $/ element elementName &/ content of
    (text:_) -> text
    [] -> ""

-- | Get all content of elements with the same name
getMultipleContent :: Cursor -> Name -> [Text]
getMultipleContent cursor elementName =
  cursor $/ element elementName &/ content

-- | Search for entries by kanji or reading
searchEntries :: Text -> [JMdictEntry] -> [JMdictEntry]
searchEntries searchTerm = filter matchesEntry
  where
    matchesEntry entry =
      any (T.isInfixOf searchTerm . kanjiText) (kanjiElements entry) ||
      any (T.isInfixOf searchTerm . readingText) (readingElements entry)

-- | Get all English glosses from an entry
getEnglishGlosses :: JMdictEntry -> [Text]
getEnglishGlosses entry = concatMap getSenseGlosses (senses entry)
  where
    getSenseGlosses sense =
      map glossText $ filter isEnglish (glosses sense)
    isEnglish gloss = maybe True (== "eng") (glossLang gloss)

-- | Pretty print an entry
prettyPrintEntry :: JMdictEntry -> Text
prettyPrintEntry entry = T.unlines
  [ "Entry #" <> T.pack (show $ entrySeq entry)
  , "Kanji: " <> T.intercalate ", " (map kanjiText $ kanjiElements entry)
  , "Reading: " <> T.intercalate ", " (map readingText $ readingElements entry)
  , "Meanings: " <> T.intercalate "; " (getEnglishGlosses entry)
  , "Examples: " <> T.pack (show $ length $ examples entry) <> " sentences"
  , ""
  ]

-- Example usage function
exampleUsage :: IO ()
exampleUsage = do
  -- Parse the JMdict_e_examp file
  entries <- parseJMdictFile "JMdict_e_examp"

  putStrLn $ "Loaded " ++ show (length entries) ++ " entries"

  -- Search for entries containing "水"
  let waterEntries = searchEntries "水" entries
  putStrLn $ "Found " ++ show (length waterEntries) ++ " entries for '水'"

  -- Print first few entries
  mapM_ (T.putStr . prettyPrintEntry) (take 3 waterEntries)

  -- Show entries with examples
  let entriesWithExamples = filter (not . null . examples) entries
  putStrLn $ show (length entriesWithExamples) ++ " entries have example sentences"