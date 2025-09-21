{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module KanjiDic2Sql where

import Database.SQLite
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe, listToMaybe)
import Control.Monad (void, forM_)
import Control.Exception (bracket)

-- Import the data types from KanjiDic2Parser
-- (Assuming the KanjiDic2Parser module is available)
-- import KanjiDic2Parser

-- Re-defining the data types here for completeness
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

-- Database operations
type DatabasePath = String

-- Create all tables
createTables :: Database -> IO ()
createTables db = do
  -- Characters table
  exec db $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS characters ("
    , "id INTEGER PRIMARY KEY AUTOINCREMENT,"
    , "literal TEXT NOT NULL UNIQUE,"
    , "grade INTEGER,"
    , "freq INTEGER,"
    , "jlpt INTEGER"
    , ");"
    ]
  
  -- Codepoints table
  exec db $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS codepoints ("
    , "id INTEGER PRIMARY KEY AUTOINCREMENT,"
    , "character_id INTEGER NOT NULL,"
    , "cp_type TEXT NOT NULL,"
    , "cp_value TEXT NOT NULL,"
    , "FOREIGN KEY (character_id) REFERENCES characters(id) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Radicals table
  exec db $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS radicals ("
    , "id INTEGER PRIMARY KEY AUTOINCREMENT,"
    , "character_id INTEGER NOT NULL,"
    , "rad_type TEXT NOT NULL,"
    , "rad_value INTEGER NOT NULL,"
    , "FOREIGN KEY (character_id) REFERENCES characters(id) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Stroke counts table
  exec db $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS stroke_counts ("
    , "id INTEGER PRIMARY KEY AUTOINCREMENT,"
    , "character_id INTEGER NOT NULL,"
    , "stroke_count INTEGER NOT NULL,"
    , "FOREIGN KEY (character_id) REFERENCES characters(id) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Variants table
  exec db $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS variants ("
    , "id INTEGER PRIMARY KEY AUTOINCREMENT,"
    , "character_id INTEGER NOT NULL,"
    , "var_type TEXT NOT NULL,"
    , "var_value TEXT NOT NULL,"
    , "FOREIGN KEY (character_id) REFERENCES characters(id) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Radical names table
  exec db $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS radical_names ("
    , "id INTEGER PRIMARY KEY AUTOINCREMENT,"
    , "character_id INTEGER NOT NULL,"
    , "rad_name TEXT NOT NULL,"
    , "FOREIGN KEY (character_id) REFERENCES characters(id) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Dictionary numbers table
  exec db $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS dic_numbers ("
    , "id INTEGER PRIMARY KEY AUTOINCREMENT,"
    , "character_id INTEGER NOT NULL,"
    , "dr_type TEXT NOT NULL,"
    , "dr_value TEXT NOT NULL,"
    , "m_vol TEXT,"
    , "m_page TEXT,"
    , "FOREIGN KEY (character_id) REFERENCES characters(id) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Query codes table
  exec db $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS query_codes ("
    , "id INTEGER PRIMARY KEY AUTOINCREMENT,"
    , "character_id INTEGER NOT NULL,"
    , "qc_type TEXT NOT NULL,"
    , "qc_value TEXT NOT NULL,"
    , "skip_misclass TEXT,"
    , "FOREIGN KEY (character_id) REFERENCES characters(id) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Reading-Meaning groups table
  exec db $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS rm_groups ("
    , "id INTEGER PRIMARY KEY AUTOINCREMENT,"
    , "character_id INTEGER NOT NULL,"
    , "group_order INTEGER NOT NULL DEFAULT 0,"
    , "FOREIGN KEY (character_id) REFERENCES characters(id) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Readings table
  exec db $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS readings ("
    , "id INTEGER PRIMARY KEY AUTOINCREMENT,"
    , "rm_group_id INTEGER NOT NULL,"
    , "r_type TEXT NOT NULL,"
    , "r_value TEXT NOT NULL,"
    , "on_type TEXT,"
    , "r_status TEXT,"
    , "FOREIGN KEY (rm_group_id) REFERENCES rm_groups(id) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Meanings table
  exec db $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS meanings ("
    , "id INTEGER PRIMARY KEY AUTOINCREMENT,"
    , "rm_group_id INTEGER NOT NULL,"
    , "m_lang TEXT DEFAULT 'en',"
    , "m_value TEXT NOT NULL,"
    , "FOREIGN KEY (rm_group_id) REFERENCES rm_groups(id) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Nanori table
  exec db $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS nanori ("
    , "id INTEGER PRIMARY KEY AUTOINCREMENT,"
    , "character_id INTEGER NOT NULL,"
    , "nanori_value TEXT NOT NULL,"
    , "FOREIGN KEY (character_id) REFERENCES characters(id) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Create indexes
  createIndexes db

createIndexes :: Database -> IO ()
createIndexes db = do
  exec db "CREATE INDEX IF NOT EXISTS idx_characters_literal ON characters(literal);"
  exec db "CREATE INDEX IF NOT EXISTS idx_characters_grade ON characters(grade);"
  exec db "CREATE INDEX IF NOT EXISTS idx_characters_freq ON characters(freq);"
  exec db "CREATE INDEX IF NOT EXISTS idx_characters_jlpt ON characters(jlpt);"
  exec db "CREATE INDEX IF NOT EXISTS idx_codepoints_character_id ON codepoints(character_id);"
  exec db "CREATE INDEX IF NOT EXISTS idx_codepoints_type ON codepoints(cp_type);"
  exec db "CREATE INDEX IF NOT EXISTS idx_radicals_character_id ON radicals(character_id);"
  exec db "CREATE INDEX IF NOT EXISTS idx_readings_rm_group_id ON readings(rm_group_id);"
  exec db "CREATE INDEX IF NOT EXISTS idx_readings_type ON readings(r_type);"
  exec db "CREATE INDEX IF NOT EXISTS idx_meanings_rm_group_id ON meanings(rm_group_id);"
  exec db "CREATE INDEX IF NOT EXISTS idx_meanings_lang ON meanings(m_lang);"

-- Insert a character and return its ID
insertCharacter :: Database -> Character -> IO Int64
insertCharacter db char = do
  let misc = cMisc char
  
  stmt <- prepare db $ T.unwords
    [ "INSERT INTO characters (literal, grade, freq, jlpt)"
    , "VALUES (?, ?, ?, ?);"
    ]
  
  bind stmt [SQLText (cLiteral char)]
  bindNamed stmt [":2" := mGrade misc, ":3" := mFreq misc, ":4" := mJlpt misc]
  
  void $ step stmt
  finalize stmt
  lastInsertRowId db

-- Insert codepoints for a character
insertCodepoints :: Database -> Int64 -> [Codepoint] -> IO ()
insertCodepoints db charId codepoints = do
  stmt <- prepare db "INSERT INTO codepoints (character_id, cp_type, cp_value) VALUES (?, ?, ?);"
  
  forM_ codepoints $ \cp -> do
    reset stmt
    bind stmt [SQLInteger charId, SQLText (cpType cp), SQLText (cpValue cp)]
    void $ step stmt
  
  finalize stmt

-- Insert radicals for a character
insertRadicals :: Database -> Int64 -> [Radical] -> IO ()
insertRadicals db charId radicals = do
  stmt <- prepare db "INSERT INTO radicals (character_id, rad_type, rad_value) VALUES (?, ?, ?);"
  
  forM_ radicals $ \rad -> do
    reset stmt
    bind stmt [SQLInteger charId, SQLText (radType rad), SQLInteger (fromIntegral $ radValue rad)]
    void $ step stmt
  
  finalize stmt

-- Insert stroke counts for a character
insertStrokeCounts :: Database -> Int64 -> [Int] -> IO ()
insertStrokeCounts db charId strokeCounts = do
  stmt <- prepare db "INSERT INTO stroke_counts (character_id, stroke_count) VALUES (?, ?);"
  
  forM_ strokeCounts $ \sc -> do
    reset stmt
    bind stmt [SQLInteger charId, SQLInteger (fromIntegral sc)]
    void $ step stmt
  
  finalize stmt

-- Insert variants for a character
insertVariants :: Database -> Int64 -> [Variant] -> IO ()
insertVariants db charId variants = do
  stmt <- prepare db "INSERT INTO variants (character_id, var_type, var_value) VALUES (?, ?, ?);"
  
  forM_ variants $ \var -> do
    reset stmt
    bind stmt [SQLInteger charId, SQLText (vType var), SQLText (vValue var)]
    void $ step stmt
  
  finalize stmt

-- Insert radical names for a character
insertRadicalNames :: Database -> Int64 -> [Text] -> IO ()
insertRadicalNames db charId radNames = do
  stmt <- prepare db "INSERT INTO radical_names (character_id, rad_name) VALUES (?, ?);"
  
  forM_ radNames $ \rn -> do
    reset stmt
    bind stmt [SQLInteger charId, SQLText rn]
    void $ step stmt
  
  finalize stmt

-- Insert dictionary numbers for a character
insertDicNumbers :: Database -> Int64 -> [DicNumber] -> IO ()
insertDicNumbers db charId dicNumbers = do
  stmt <- prepare db $ T.unwords
    [ "INSERT INTO dic_numbers (character_id, dr_type, dr_value, m_vol, m_page)"
    , "VALUES (?, ?, ?, ?, ?);"
    ]
  
  forM_ dicNumbers $ \dn -> do
    reset stmt
    bind stmt [SQLInteger charId, SQLText (dnType dn), SQLText (dnValue dn)]
    bindNamed stmt [":4" := dnVolume dn, ":5" := dnPage dn]
    void $ step stmt
  
  finalize stmt

-- Insert query codes for a character
insertQueryCodes :: Database -> Int64 -> [QueryCode] -> IO ()
insertQueryCodes db charId queryCodes = do
  stmt <- prepare db $ T.unwords
    [ "INSERT INTO query_codes (character_id, qc_type, qc_value, skip_misclass)"
    , "VALUES (?, ?, ?, ?);"
    ]
  
  forM_ queryCodes $ \qc -> do
    reset stmt
    bind stmt [SQLInteger charId, SQLText (qcType qc), SQLText (qcValue qc)]
    bindNamed stmt [":4" := qcSkipMisclass qc]
    void $ step stmt
  
  finalize stmt

-- Insert reading-meaning group and return its ID
insertRMGroup :: Database -> Int64 -> Int -> IO Int64
insertRMGroup db charId groupOrder = do
  stmt <- prepare db "INSERT INTO rm_groups (character_id, group_order) VALUES (?, ?);"
  bind stmt [SQLInteger charId, SQLInteger (fromIntegral groupOrder)]
  void $ step stmt
  finalize stmt
  lastInsertRowId db

-- Insert readings for a reading-meaning group
insertReadings :: Database -> Int64 -> [Reading] -> IO ()
insertReadings db rmGroupId readings = do
  stmt <- prepare db $ T.unwords
    [ "INSERT INTO readings (rm_group_id, r_type, r_value, on_type, r_status)"
    , "VALUES (?, ?, ?, ?, ?);"
    ]
  
  forM_ readings $ \r -> do
    reset stmt
    bind stmt [SQLInteger rmGroupId, SQLText (rType r), SQLText (rValue r)]
    bindNamed stmt [":4" := rOnType r, ":5" := rRStatus r]
    void $ step stmt
  
  finalize stmt

-- Insert meanings for a reading-meaning group
insertMeanings :: Database -> Int64 -> [Meaning] -> IO ()
insertMeanings db rmGroupId meanings = do
  stmt <- prepare db $ T.unwords
    [ "INSERT INTO meanings (rm_group_id, m_lang, m_value)"
    , "VALUES (?, ?, ?);"
    ]
  
  forM_ meanings $ \m -> do
    reset stmt
    bind stmt [SQLInteger rmGroupId]
    bindNamed stmt [":2" := mLang m, ":3" := SQLText (mValue m)]
    void $ step stmt
  
  finalize stmt

-- Insert nanori readings for a character
insertNanori :: Database -> Int64 -> [Text] -> IO ()
insertNanori db charId nanoriList = do
  stmt <- prepare db "INSERT INTO nanori (character_id, nanori_value) VALUES (?, ?);"
  
  forM_ nanoriList $ \n -> do
    reset stmt
    bind stmt [SQLInteger charId, SQLText n]
    void $ step stmt
  
  finalize stmt

-- Insert complete character with all related data
insertCompleteCharacter :: Database -> Character -> IO ()
insertCompleteCharacter db char = do
  -- Insert main character record
  charId <- insertCharacter db char
  
  -- Insert related data
  insertCodepoints db charId (cCodepoint char)
  insertRadicals db charId (cRadical char)
  
  let misc = cMisc char
  insertStrokeCounts db charId (mStrokeCount misc)
  insertVariants db charId (mVariant misc)
  insertRadicalNames db charId (mRadName misc)
  
  insertDicNumbers db charId (cDicNumber char)
  insertQueryCodes db charId (cQueryCode char)
  
  -- Insert reading-meaning data if present
  case cReadingMeaning char of
    Nothing -> return ()
    Just rm -> do
      -- Insert nanori readings
      insertNanori db charId (rmNanori rm)
      
      -- Insert reading-meaning groups
      forM_ (zip [0..] (rmGroups rm)) $ \(groupOrder, rmGroup) -> do
        rmGroupId <- insertRMGroup db charId groupOrder
        insertReadings db rmGroupId (rmgReadings rmGroup)
        insertMeanings db rmGroupId (rmgMeanings rmGroup)

-- Query functions

-- Find character by literal
findCharacterByLiteral :: Database -> Text -> IO (Maybe Character)
findCharacterByLiteral db literal = do
  stmt <- prepare db "SELECT id FROM characters WHERE literal = ? LIMIT 1;"
  bind stmt [SQLText literal]
  result <- step stmt
  
  case result of
    Row -> do
      charId <- column stmt 0
      finalize stmt
      case charId of
        SQLInteger cid -> Just <$> reconstructCharacter db cid
        _ -> return Nothing
    Done -> do
      finalize stmt
      return Nothing

-- Reconstruct character from database
reconstructCharacter :: Database -> Int64 -> IO Character
reconstructCharacter db charId = do
  -- Get basic character info
  stmt <- prepare db "SELECT literal, grade, freq, jlpt FROM characters WHERE id = ?;"
  bind stmt [SQLInteger charId]
  void $ step stmt
  
  literal <- column stmt 0
  grade <- column stmt 1
  freq <- column stmt 2
  jlpt <- column stmt 3
  finalize stmt
  
  -- Get all related data
  codepoints <- getCodepoints db charId
  radicals <- getRadicals db charId
  strokeCounts <- getStrokeCounts db charId
  variants <- getVariants db charId
  radNames <- getRadicalNames db charId
  dicNumbers <- getDicNumbers db charId
  queryCodes <- getQueryCodes db charId
  readingMeaning <- getReadingMeaning db charId
  
  let misc = Misc
        { mGrade = sqlToMaybe grade
        , mStrokeCount = strokeCounts
        , mVariant = variants
        , mFreq = sqlToMaybe freq
        , mRadName = radNames
        , mJlpt = sqlToMaybe jlpt
        }
  
  return Character
    { cLiteral = sqlToText literal
    , cCodepoint = codepoints
    , cRadical = radicals
    , cMisc = misc
    , cDicNumber = dicNumbers
    , cQueryCode = queryCodes
    , cReadingMeaning = readingMeaning
    }

-- Helper functions for reconstruction (simplified implementations)
getCodepoints :: Database -> Int64 -> IO [Codepoint]
getCodepoints db charId = do
  stmt <- prepare db "SELECT cp_type, cp_value FROM codepoints WHERE character_id = ?;"
  bind stmt [SQLInteger charId]
  results <- collectRows stmt []
  finalize stmt
  return results
  where
    collectRows stmt acc = do
      result <- step stmt
      case result of
        Row -> do
          cpType <- column stmt 0
          cpValue <- column stmt 1
          let cp = Codepoint (sqlToText cpType) (sqlToText cpValue)
          collectRows stmt (cp : acc)
        Done -> return (reverse acc)

getRadicals :: Database -> Int64 -> IO [Radical]
getRadicals db charId = do
  stmt <- prepare db "SELECT rad_type, rad_value FROM radicals WHERE character_id = ?;"
  bind stmt [SQLInteger charId]
  results <- collectRows stmt []
  finalize stmt
  return results
  where
    collectRows stmt acc = do
      result <- step stmt
      case result of
        Row -> do
          radType <- column stmt 0
          radValue <- column stmt 1
          let rad = Radical (sqlToText radType) (sqlToInt radValue)
          collectRows stmt (rad : acc)
        Done -> return (reverse acc)

-- Similar helper functions would be implemented for other data types...
-- (Omitted for brevity, but following the same pattern)

-- Stub implementations for remaining helper functions
getStrokeCounts :: Database -> Int64 -> IO [Int]
getStrokeCounts _ _ = return []

getVariants :: Database -> Int64 -> IO [Variant]
getVariants _ _ = return []

getRadicalNames :: Database -> Int64 -> IO [Text]
getRadicalNames _ _ = return []

getDicNumbers :: Database -> Int64 -> IO [DicNumber]
getDicNumbers _ _ = return []

getQueryCodes :: Database -> Int64 -> IO [QueryCode]
getQueryCodes _ _ = return []

getReadingMeaning :: Database -> Int64 -> IO (Maybe ReadingMeaning)
getReadingMeaning _ _ = return Nothing

-- Utility functions
sqlToText :: SQLData -> Text
sqlToText (SQLText t) = t
sqlToText _ = ""

sqlToInt :: SQLData -> Int
sqlToInt (SQLInteger i) = fromIntegral i
sqlToInt _ = 0

sqlToMaybe :: SQLData -> Maybe Int
sqlToMaybe (SQLInteger i) = Just (fromIntegral i)
sqlToMaybe SQLNull = Nothing
sqlToMaybe _ = Nothing

-- Database initialization and usage
initializeDatabase :: DatabasePath -> IO Database
initializeDatabase dbPath = do
  db <- open (T.pack dbPath)
  createTables db
  return db

-- Example usage
exampleUsage :: IO ()
exampleUsage = do
  db <- initializeDatabase "kanjidic2.db"
  
  -- Create example character (水 - water)
  let waterChar = Character
        { cLiteral = "水"
        , cCodepoint = [Codepoint "ucs" "6c34", Codepoint "jis208" "1-31-51"]
        , cRadical = [Radical "classical" 85, Radical "nelson_c" 85]
        , cMisc = Misc
            { mGrade = Just 1
            , mStrokeCount = [4]
            , mVariant = []
            , mFreq = Just 240
            , mRadName = ["水", "みず"]
            , mJlpt = Just 5
            }
        , cDicNumber = [DicNumber "nelson_c" "2481" Nothing Nothing]
        , cQueryCode = [QueryCode "skip" "1-3-1" Nothing]
        , cReadingMeaning = Just $ ReadingMeaning
            { rmGroups = [RMGroup
                { rmgReadings = 
                    [ Reading "ja_on" "スイ" (Just "kan") Nothing
                    , Reading "ja_kun" "みず" Nothing Nothing
                    , Reading "ja_kun" "みず-" Nothing Nothing
                    ]
                , rmgMeanings = 
                    [ Meaning (Just "en") "water"
                    , Meaning (Just "en") "liquid"
                    ]
                }]
            , rmNanori = ["うず", "ずみ", "つ", "ど", "みつ", "みな", "みん"]
            }
        }
  
  -- Insert the character
  insertCompleteCharacter db waterChar
  putStrLn "Inserted character 水"
  
  -- Query it back
  result <- findCharacterByLiteral db "水"
  case result of
    Just char -> putStrLn $ "Found character: " ++ T.unpack (cLiteral char)
    Nothing -> putStrLn "Character not found"
  
  close db

-- Main function for testing
main :: IO ()
main = exampleUsage