{-# LANGUAGE OverloadedStrings #-}

module JMdictParser where

import Text.XML
import Text.XML.Cursor
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe
-- import Control.Monad (guard)
-- import Data.List (find)

import Control.Exception (try)

-- | Main JMdict entry structure
data JMdictEntry = JMdictEntry
  { entrySeq :: !Int                    -- ^ Unique sequence number
  , kanjiElements :: ![KanjiElement]    -- ^ Kanji headwords
  , readingElements :: ![ReadingElement] -- ^ Kana readings
  , senses :: ![Sense]                  -- ^ Meanings and translations
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
  , miscInfo :: ![Text]                 -- ^ Miscellaneous information\
  , languageSources :: ![LanguageSource]-- ^ Source language information
  , dialectInfo :: ![Text]              -- ^ Dialect information
  , glosses :: ![Gloss]                 -- ^ Translation glosses
  , senseInfo :: ![Text]                -- ^ Sense information
  , example :: ![Example]             -- ^ Examples
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
  , exampleSourceType :: !Text          -- ^ Seq Num from the Tatoeba Project
  , exampleSource :: !Text              -- ^ Source reference
  , exampleText :: !Text
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
  , senseInfo = getMultipleContent cursor "s_inf"
  , languageSources = parseLanguageSources cursor
  , example = parseExamples cursor
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
  { lsLang = fromMaybe "eng" $ listToMaybe $ cursor $| attribute xmlLang
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
  { exampleJapanese = getLanguageExample cursor "jpn"
  , exampleEnglish = getLanguageExample cursor "eng"
  , exampleSource = getFirstContent cursor "ex_srce"
  , exampleSourceType = fromMaybe "" $ listToMaybe (cursor $/ attribute "exsrc_type")
  , exampleText = getFirstContent cursor "ex_text"
  }

-- Helper functions
xmlLang :: Name
xmlLang = Name "lang" (Just "http://www.w3.org/XML/1998/namespace") (Just "xml")

getLanguageExample :: Cursor -> Text -> Text
getLanguageExample cursor lang =
  case cursor $// element "ex_sent" >=> attributeIs xmlLang lang of
    (c:_) -> T.concat $ c $/ content
    [] -> ""

-- | Get first content of an element
getFirstContent :: Cursor -> Name -> Text
getFirstContent cursor elementName' =
  case cursor $/ element elementName' &/ content of
    (text:_) -> text
    [] -> ""

-- | Get all content of elements with the same name
getMultipleContent :: Cursor -> Name -> [Text]
getMultipleContent cursor elementName' =
  cursor $/ element elementName' &/ content

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
  , "Examples:"
  ] <> prettyPrintExamples (senses entry)
  where
    prettyPrintExamples [] = "  No examples available\n"
    prettyPrintExamples ss = T.unlines $ map formatExample ss

    formatExample sense = case example sense of
      [] -> "  No examples available\n"
      exs -> T.unlines $ map formatSingleExample exs

    formatSingleExample ex = T.unlines
      [ "  Japanese: " <> exampleJapanese ex
      , "  English:  " <> exampleEnglish ex
      , "  Source:   " <> exampleSource ex
      , ""
      ]

prettyPrintEntryDetailed :: JMdictEntry -> Text
prettyPrintEntryDetailed entry = T.unlines $
    [ "============ Entry #" <> T.pack (show $ entrySeq entry) <> " ============"
    , ""
    , "=== Kanji Elements ==="
    ] <> concatMap printKanjiElement (kanjiElements entry) <>
    [ ""
    , "=== Reading Elements ==="
    ] <> concatMap printReadingElement (readingElements entry) <>
    [ ""
    , "=== Senses ==="
    ] <> concatMap printSense (senses entry)
  where
    printKanjiElement k =
      [ "  Kanji: " <> kanjiText k
      , "    Info: " <> formatList (kanjiInfo k)
      , "    Priority: " <> formatList (kanjiPriority k)
      , ""
      ]

    printReadingElement r =
      [ "  Reading: " <> readingText r
      , "    No Kanji: " <> T.pack (show $ readingNoKanji r)
      , "    Restrictions: " <> formatList (readingRestrictions r)
      , "    Info: " <> formatList (readingInfo r)
      , "    Priority: " <> formatList (readingPriority r)
      , ""
      ]

    printSense s =
      [ "  Sense" <> ":"
      , "    Restrictions: " <> formatList (senseRestrictions s)
      , "    Cross References: " <> formatList (crossReferences s)
      , "    Antonyms: " <> formatList (antonyms s)
      , "    Parts of Speech: " <> formatList (partOfSpeech s)
      , "    Fields: " <> formatList (fields s)
      , "    Misc Info: " <> formatList (miscInfo s)
      , "    Dialect Info: " <> formatList (dialectInfo s)
      , "    Sense Info: " <> formatList (senseInfo s)
      , "    Language Sources:"
      ] <> map ("      " <>) (concatMap printLangSource (languageSources s)) <>
      [ "    Glosses:"
      ] <> map ("      " <>) (concatMap printGloss (glosses s)) <>
      [ "    Examples:"
      ] <> map ("      " <>) (concatMap printExample (example s)) <>
      [""]

    printLangSource ls =
      [ "Language: " <> lsLang ls
      , "Type: " <> fromMaybe "None" (lsType ls)
      , "Wasei: " <> T.pack (show $ lsWasei ls)
      , "Text: " <> fromMaybe "None" (lsText ls)
      , ""
      ]

    printGloss g =
      [ "Text: " <> glossText g
      , "Language: " <> fromMaybe "eng" (glossLang g)
      , "Gender: " <> fromMaybe "None" (glossGender g)
      , "Type: " <> fromMaybe "None" (glossType g)
      , ""
      ]

    printExample e =
      [ "Japanese: " <> exampleJapanese e
      , "English: " <> exampleEnglish e
      , "Source Type: " <> exampleSourceType e
      , "Source: " <> exampleSource e
      , "Text: " <> exampleText e
      , ""
      ]

    formatList [] = "None"
    formatList xs = T.intercalate ", " xs

-- Example usage function
exampleUsage :: IO ()
exampleUsage = do
  -- Parse the JMdict_e_examp file
  -- entries <- parseJMdictFile "others\\JMdict_e_examp.xml"
  entries <- parseJMdictFile "others\\jmdictinfo.xml"

  putStrLn $ "Loaded " ++ show (length entries) ++ " entries"

  -- Search for entries containing "水"
  let waterEntries = searchEntries "明白" entries
  putStrLn $ "Found " ++ show (length waterEntries) ++ " entries for '明白'"

  -- Print first few entries
  -- mapM_ (T.putStr . prettyPrintEntry) (take 3 waterEntries)
  mapM_ (T.putStr . prettyPrintEntryDetailed) (take 3 waterEntries)

-- | Run the JMdict parser on a file
runJMdictParser :: FilePath -> IO (Either String [JMdictEntry])
runJMdictParser filePath = do
    result <- try $ parseJMdictFile filePath
    case result of
        Left err -> return $ Left (show (err :: XMLException))
        Right entries -> return $ Right entries