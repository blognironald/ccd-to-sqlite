{-# LANGUAGE OverloadedStrings #-}

module KanjiDic2Parser where

import Text.XML
import Text.XML.Cursor
import Data.Text as T
import Data.Text.IO as T
import Data.Maybe
import Text.Read
import qualified Data.Text.Read as T

-- Data types representing KANJIDIC2 structure
data KanjiDic2 = KanjiDic2
  { kdHeader :: Header
  , kdCharacters :: [Character]
  } deriving (Show, Eq)

data Header = Header
  { hFileVersion :: Text
  , hDatabaseVersion :: Text
  , hDateOfCreation :: Text
  } deriving (Show, Eq)

data Character = Character
  { cLiteral :: Text
  , cCodepoint :: [Codepoint]
  , cRadical :: [Radical]
  , cMisc :: Misc
  , cDicNumber :: [DicNumber]
  , cQueryCode :: [QueryCode]
  , cReadingMeaning :: Maybe ReadingMeaning
  } deriving (Show, Eq)

data Codepoint = Codepoint
  { cpType :: Text
  , cpValue :: Text
  } deriving (Show, Eq)

data Radical = Radical
  { radType :: Text
  , radValue :: Int
  } deriving (Show, Eq)

data Misc = Misc
  { mGrade :: Maybe Int
  , mStrokeCount :: [Int]
  , mVariant :: [Variant]
  , mFreq :: Maybe Int
  , mRadName :: [Text]
  , mJlpt :: Maybe Int
  } deriving (Show, Eq)

data Variant = Variant
  { vType :: Text
  , vValue :: Text
  } deriving (Show, Eq)

data DicNumber = DicNumber
  { dnType :: Text
  , dnValue :: Text
  , dnVolume :: Maybe Text
  , dnPage :: Maybe Text
  } deriving (Show, Eq)

data QueryCode = QueryCode
  { qcType :: Text
  , qcValue :: Text
  , qcSkipMisclass :: Maybe Text
  } deriving (Show, Eq)

data ReadingMeaning = ReadingMeaning
  { rmGroups :: [RMGroup]
  , rmNanori :: [Text]
  } deriving (Show, Eq)

data RMGroup = RMGroup
  { rmgReadings :: [Reading]
  , rmgMeanings :: [Meaning]
  } deriving (Show, Eq)

data Reading = Reading
  { rType :: Text
  , rValue :: Text
  , rOnType :: Maybe Text
  , rRStatus :: Maybe Text
  } deriving (Show, Eq)

data Meaning = Meaning
  { mLang :: Maybe Text
  , mValue :: Text
  } deriving (Show, Eq)

-- Parser functions
-- parseKanjiDic2 :: FilePath -> IO (Either String KanjiDic2)
parseKanjiDic2 :: FilePath -> IO KanjiDic2
parseKanjiDic2 filePath = do
  doc <- Text.XML.readFile def filePath
  return $ parseKanjiDic2Doc doc


parseKanjiDic2Doc :: Document -> KanjiDic2
parseKanjiDic2Doc doc =
  let cursor = fromDocument doc
      root = cursor $| element "kanjidic2"
  in case root of
    [rootCursor] -> KanjiDic2
      { kdHeader = parseHeader rootCursor
      , kdCharacters = parseCharacters rootCursor
      }
    _ -> error "Invalid KANJIDIC2 format: no root element"

parseHeader :: Cursor -> Header
parseHeader cursor =
  let headerCursor = cursor $/ element "header"
  in case headerCursor of
    [hc] -> Header
      { hFileVersion = getChildText "file_version" hc
      , hDatabaseVersion = getChildText "database_version" hc
      , hDateOfCreation = getChildText "date_of_creation" hc
      }
    _ -> Header "" "" ""

parseCharacters :: Cursor -> [Character]
parseCharacters cursor =
  cursor $/ element "character" &| parseCharacter

parseCharacter :: Cursor -> Character
parseCharacter cursor = Character
  { cLiteral = getChildText "literal" cursor
  , cCodepoint = parseCodepoints cursor
  , cRadical = parseRadicals cursor
  , cMisc = parseMisc cursor
  , cDicNumber = parseDicNumbers cursor
  , cQueryCode = parseQueryCodes cursor
  , cReadingMeaning = parseReadingMeaning cursor
  }

parseCodepoints :: Cursor -> [Codepoint]
parseCodepoints cursor =
  cursor $/ element "codepoint" &/ element "cp_value" &| parseCodepoint

parseCodepoint :: Cursor -> Codepoint
parseCodepoint cursor = Codepoint
  { cpType = getAttr "cp_type" cursor
  , cpValue = T.concat $ cursor $/ content
  }

parseRadicals :: Cursor -> [Radical]
parseRadicals cursor =
  cursor $/ element "radical" &/ element "rad_value" &| parseRadical

parseRadical :: Cursor -> Radical
parseRadical cursor = Radical
  { radType = getAttr "rad_type" cursor
  , radValue = readInt $ T.concat $ cursor $/ content
  }

parseMisc :: Cursor -> Misc
parseMisc cursor =
  let miscCursor = cursor $/ element "misc"
  in case miscCursor of
    [mc] -> Misc
      { mGrade = readIntMaybe $ getChildText "grade" mc
      , mStrokeCount = mapMaybe (readMaybe . T.unpack) $ mc $/ element "stroke_count" &/ content
      , mVariant = mc $/ element "variant" &| parseVariant
      , mFreq = readIntMaybe $ getChildText "freq" mc
      , mRadName = mc $/ element "rad_name" &/ content
      , mJlpt = readIntMaybe $ getChildText "jlpt" mc
      }
    _ -> Misc Nothing [] [] Nothing [] Nothing

parseVariant :: Cursor -> Variant
parseVariant cursor = Variant
  { vType = getAttr "var_type" cursor
  , vValue = T.concat $ cursor $/ content
  }

parseDicNumbers :: Cursor -> [DicNumber]
parseDicNumbers cursor =
  cursor $/ element "dic_number" &/ element "dic_ref" &| parseDicNumber

parseDicNumber :: Cursor -> DicNumber
parseDicNumber cursor = DicNumber
  { dnType = getAttr "dr_type" cursor
  , dnValue = T.concat $ cursor $/ content
  , dnVolume = getAttrMaybe "m_vol" cursor
  , dnPage = getAttrMaybe "m_page" cursor
  }

parseQueryCodes :: Cursor -> [QueryCode]
parseQueryCodes cursor =
  cursor $/ element "query_code" &/ element "q_code" &| parseQueryCode

parseQueryCode :: Cursor -> QueryCode
parseQueryCode cursor = QueryCode
  { qcType = getAttr "qc_type" cursor
  , qcValue = T.concat $ cursor $/ content
  , qcSkipMisclass = getAttrMaybe "skip_misclass" cursor
  }

parseReadingMeaning :: Cursor -> Maybe ReadingMeaning
parseReadingMeaning cursor =
  let rmCursor = cursor $/ element "reading_meaning"
  in case rmCursor of
    [rmc] -> Just $ ReadingMeaning
      { rmGroups = rmc $/ element "rmgroup" &| parseRMGroup
      , rmNanori = rmc $/ element "nanori" &/ content
      }
    _ -> Nothing

parseRMGroup :: Cursor -> RMGroup
parseRMGroup cursor = RMGroup
  { rmgReadings = cursor $/ element "reading" &| parseReading
  , rmgMeanings = cursor $/ element "meaning" &| parseMeaning
  }

parseReading :: Cursor -> Reading
parseReading cursor = Reading
  { rType = getAttr "r_type" cursor
  , rValue = T.concat $ cursor $/ content
  , rOnType = getAttrMaybe "on_type" cursor
  , rRStatus = getAttrMaybe "r_status" cursor
  }

parseMeaning :: Cursor -> Meaning
parseMeaning cursor = Meaning
  { mLang = getAttrMaybe "m_lang" cursor
  , mValue = T.concat $ cursor $/ content
  }

-- Helper functions
getChildText :: Name -> Cursor -> Text
getChildText name cursor =
  case cursor $/ element name &/ content of
    [text] -> text
    _ -> ""

getAttr :: Name -> Cursor -> Text
getAttr name cursor =
  case attribute name cursor of
    [text] -> text
    _ -> ""

getAttrMaybe :: Name -> Cursor -> Maybe Text
getAttrMaybe name cursor =
  case attribute name cursor of
    [text] -> Just text
    _ -> Nothing

readInt :: Text -> Int
readInt text =
  case T.decimal text of
    Right (n, _) -> n
    Left _ -> 0

readIntMaybe :: Text -> Maybe Int
readIntMaybe text
  | T.null text = Nothing
  | otherwise = case T.decimal text of
      Right (n, _) -> Just n
      Left _ -> Nothing

-- Usage example
main :: IO ()
main = do
  kanjiDic2 <- parseKanjiDic2 "others\\kanjidic2.xml"
  T.putStrLn $ T.pack "Parsed " <> T.pack (show (Prelude.length (kdCharacters kanjiDic2))) <> T.pack " characters"
-- Print first character as example
  case kdCharacters kanjiDic2 of
    (c:_) -> do
      T.putStrLn $ T.pack "First character: " <> cLiteral c
      T.putStrLn $ T.pack "Stroke count: " <> T.pack (show (mStrokeCount . cMisc $ c))
    [] -> T.putStrLn "No characters found"

-- Utility functions for querying parsed data
findKanjiByLiteral :: Text -> KanjiDic2 -> Maybe Character
findKanjiByLiteral literal kd =
  listToMaybe $ Prelude.filter (\c -> cLiteral c == literal) (kdCharacters kd)

-- getKunReadings :: Character -> [Text]
-- getKunReadings char =
--   case cReadingMeaning char of
--     Just rm -> T.concatMap (T.map rValue . T.filter (\r -> rType r == "ja_kun") . rmgReadings) (rmGroups rm)
--     Nothing -> []
-- getOnReadings :: Character -> [Text]
-- getOnReadings char =
--   case cReadingMeaning char of
--     Just rm -> T.concatMap (T.map rValue . T.filter (\r -> rType r == "ja_on") . rmgReadings) (rmGroups rm)
--     Nothing -> []

-- getMeanings :: Character -> Text
-- getMeanings char =
--   case cReadingMeaning char of
--     Just rm -> T.concatMap (T.map mValue . rmgMeanings) (rmGroups rm)
--     Nothing -> ""
