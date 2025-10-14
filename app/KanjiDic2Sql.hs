{-# LANGUAGE OverloadedStrings #-}

module KanjiDic2Sql where

import Database.SQLite.Simple
import Data.Text 
import qualified Data.Text as T
import Control.Monad 

import KanjiDic2Parser

-- Connection operations
type ConnectionPath = String

-- Create all tables
createTables :: Connection -> IO ()
createTables db = do
  -- Characters table
  execute_ db $ Query $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS kd_character ("
    , "character TEXT NOT NULL,"
    , "grade INTEGER,"
    , "freq INTEGER,"
    , "jlpt INTEGER"
    , ");"
    ]
  
  -- Codepoints table
  execute_ db $ Query $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS kd_codepoint ("
    , "character TEXT NOT NULL,"
    , "cp_type TEXT NOT NULL,"
    , "cp_value TEXT NOT NULL,"
    , "FOREIGN KEY (character) REFERENCES kd_character(character) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Radicals table
  execute_ db $ Query $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS kd_radical ("
    , "character TEXT NOT NULL,"
    , "rad_type TEXT NOT NULL,"
    , "rad_value INTEGER NOT NULL,"
    , "FOREIGN KEY (character) REFERENCES kd_character(character) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Stroke counts table
  execute_ db $ Query $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS kd_stroke_count ("
    , "character TEXT NOT NULL,"
    , "stroke_count INTEGER NOT NULL,"
    , "FOREIGN KEY (character) REFERENCES kd_character(character) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Variants table
  execute_ db $ Query $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS kd_variant ("
    , "character TEXT NOT NULL,"
    , "var_type TEXT NOT NULL,"
    , "var_value TEXT NOT NULL,"
    , "FOREIGN KEY (character) REFERENCES kd_character(character) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Radical names table
  execute_ db $ Query $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS kd_radical_name ("
    , "character TEXT NOT NULL,"
    , "rad_name TEXT NOT NULL,"
    , "FOREIGN KEY (character) REFERENCES kd_character(character) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Dictionary numbers table
  execute_ db $ Query $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS kd_dict_no ("
    , "character TEXT NOT NULL,"
    , "dr_type TEXT NOT NULL,"
    , "dr_value TEXT NOT NULL,"
    , "m_vol TEXT,"
    , "m_page TEXT,"
    , "FOREIGN KEY (character) REFERENCES kd_character(character) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Query codes table
  execute_ db $ Query $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS kd_query_code ("
    , "character TEXT NOT NULL,"
    , "qc_type TEXT NOT NULL,"
    , "qc_value TEXT NOT NULL,"
    , "skip_misclass TEXT,"
    , "FOREIGN KEY (character) REFERENCES kd_character(character) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Readings table
  execute_ db $ Query $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS kd_reading ("
    , "character TEXT NOT NULL,"
    , "rm_group_no INTEGER NOT NULL,"
    , "r_type TEXT NOT NULL,"
    , "r_value TEXT NOT NULL,"
    -- , "on_type TEXT,"  -- not used
    -- , "r_status TEXT," -- not used
    , "FOREIGN KEY (character) REFERENCES kd_character(character) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Meanings table
  execute_ db $ Query $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS kd_meaning ("
    , "character TEXT NOT NULL,"
    , "rm_group_no INTEGER NOT NULL,"
    , "m_lang TEXT DEFAULT 'en',"
    , "m_value TEXT NOT NULL,"
    , "FOREIGN KEY (character) REFERENCES kd_character(character) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Nanori table
  execute_ db $ Query $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS kd_nanori ("
    , "character TEXT NOT NULL,"
    , "nanori_value TEXT NOT NULL,"
    , "FOREIGN KEY (character) REFERENCES kd_character(character) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Create indexes
  -- createIndexes db

-- createIndexes :: Connection -> IO ()
-- createIndexes db = do
--   execute_ db "CREATE INDEX IF NOT EXISTS idx_characters_literal ON characters(literal);"
--   execute_ db "CREATE INDEX IF NOT EXISTS idx_characters_grade ON characters(grade);"
--   execute_ db "CREATE INDEX IF NOT EXISTS idx_characters_freq ON characters(freq);"
--   execute_ db "CREATE INDEX IF NOT EXISTS idx_characters_jlpt ON characters(jlpt);"
--   execute_ db "CREATE INDEX IF NOT EXISTS idx_codepoints_character_id ON codepoints(character_id);"
--   execute_ db "CREATE INDEX IF NOT EXISTS idx_codepoints_type ON codepoints(cp_type);"
--   execute_ db "CREATE INDEX IF NOT EXISTS idx_radicals_character_id ON radicals(character_id);"
--   execute_ db "CREATE INDEX IF NOT EXISTS idx_readings_rm_group_id ON readings(rm_group_id);"
--   execute_ db "CREATE INDEX IF NOT EXISTS idx_readings_type ON readings(r_type);"
--   execute_ db "CREATE INDEX IF NOT EXISTS idx_meanings_rm_group_id ON meanings(rm_group_id);"
--   execute_ db "CREATE INDEX IF NOT EXISTS idx_meanings_lang ON meanings(m_lang);"

-- Insert a character and return its ID
insertCharacter :: Connection -> Character -> IO ()
insertCharacter db char = do
  let misc = cMisc char
  execute db "INSERT INTO kd_character (character, grade, freq, jlpt) VALUES (?, ?, ?, ?)"
    (cLiteral char, mGrade misc, mFreq misc, mJlpt misc)

-- Insert codepoints for a character
insertCodepoints :: Connection -> Text -> [Codepoint] -> IO ()
insertCodepoints db charId codepoints = do
  forM_ codepoints $ \cp -> do
    execute db "INSERT INTO kd_codepoint (character, cp_type, cp_value) VALUES (?, ?, ?)"
      (charId, cpType cp, cpValue cp)

-- Insert radicals for a character
insertRadicals :: Connection -> Text -> [Radical] -> IO ()
insertRadicals db charId radicals = do
  forM_ radicals $ \rad -> do
    execute db "INSERT INTO kd_radical (character, rad_type, rad_value) VALUES (?, ?, ?)"
      (charId, radType rad, radValue rad)

-- Insert stroke counts for a character
insertStrokeCounts :: Connection -> Text -> [Int] -> IO ()
insertStrokeCounts db charId strokeCounts = do
  forM_ strokeCounts $ \sc -> do
    execute db "INSERT INTO kd_stroke_count (character, stroke_count) VALUES (?, ?)"
      (charId, sc)

-- Insert variants for a character
insertVariants :: Connection -> Text -> [Variant] -> IO ()
insertVariants db charId variants = do
  forM_ variants $ \var -> do
    execute db "INSERT INTO kd_variant (character, var_type, var_value) VALUES (?, ?, ?)"
      (charId, vType var, vValue var)

-- Insert radical names for a character
insertRadicalNames :: Connection -> Text -> [Text] -> IO ()
insertRadicalNames db charId radNames = do
  forM_ radNames $ \rn -> do
    execute db "INSERT INTO kd_radical_name (character, rad_name) VALUES (?, ?)"
      (charId, rn)

-- Insert dictionary numbers for a character
insertDicNumbers :: Connection -> Text -> [DicNumber] -> IO ()
insertDicNumbers db charId dicNumbers = do
  forM_ dicNumbers $ \dn -> do
    execute db "INSERT INTO kd_dict_no (character, dr_type, dr_value, m_vol, m_page) VALUES (?, ?, ?, ?, ?)"
      (charId, dnType dn, dnValue dn, dnVolume dn, dnPage dn)

-- Insert query codes for a character
insertQueryCodes :: Connection -> Text -> [QueryCode] -> IO ()
insertQueryCodes db charId queryCodes = do
  forM_ queryCodes $ \qc -> do
    execute db "INSERT INTO kd_query_code (character, qc_type, qc_value, skip_misclass) VALUES (?, ?, ?, ?)"
      (charId, qcType qc, qcValue qc, qcSkipMisclass qc)

-- Insert readings for a reading-meaning group
insertReadings :: Connection -> Text -> Int -> [Reading] -> IO ()
insertReadings db character groupNo readings = do
  forM_ readings $ \r -> do
    execute db "INSERT INTO kd_reading (character, rm_group_no, r_type, r_value) VALUES (?, ?, ?, ?)"
      (character, groupNo, rType r, rValue r)

-- Insert meanings for a reading-meaning group
insertMeanings :: Connection -> Text -> Int -> [Meaning] -> IO ()
insertMeanings db character groupNo meanings = do
  forM_ meanings $ \m -> do
    execute db "INSERT INTO kd_meaning (character, rm_group_no, m_lang, m_value) VALUES (?, ?, ?, ?)"
      (character, groupNo, mLang m, mValue m)

-- Insert nanori readings for a character
insertNanori :: Connection -> Text -> [Text] -> IO ()
insertNanori db charId nanoriList = do
  forM_ nanoriList $ \n -> do
    execute db "INSERT INTO kd_nanori (character, nanori_value) VALUES (?, ?)"
      (charId, n)

-- Insert complete character with all related data
insertCompleteCharacter :: Connection -> Character -> IO ()
insertCompleteCharacter db char = do
  let character = cLiteral char

  -- Insert main character record
  insertCharacter db char
  
  -- Insert related data
  insertCodepoints db character (cCodepoint char)
  insertRadicals db character (cRadical char)
  
  let misc = cMisc char
  insertStrokeCounts db character (mStrokeCount misc)
  insertVariants db character (mVariant misc)
  insertRadicalNames db character (mRadName misc)
  
  insertDicNumbers db character (cDicNumber char)
  insertQueryCodes db character (cQueryCode char)
  
  -- Insert reading-meaning data if present
  case cReadingMeaning char of
    Nothing -> return ()
    Just rm -> do
      -- Insert nanori readings
      insertNanori db character (rmNanori rm)
      mapM_
        (\(rmGroup, groupNo) -> do
            -- insertRMGroup db character groupNo
            insertReadings db character groupNo (rmgReadings rmGroup)
            insertMeanings db character groupNo (rmgMeanings rmGroup))
        (Prelude.zip (rmGroups rm) [0..])

-- Query functions
-- Find character by literal
findCharacterByLiteral :: Connection -> Text -> IO (Maybe Character)
findCharacterByLiteral db character = do
  results <- query db "SELECT character FROM kd_character WHERE character = ? LIMIT 1" (Only character)
  case results of
    [Only charId] -> Just <$> reconstructCharacter db charId
    [] -> return Nothing
    _ -> return Nothing

-- Reconstruct character from database
reconstructCharacter :: Connection -> Text -> IO Character
reconstructCharacter db charId = do
  -- Get basic character info
  [(character, grade, freq, jlpt)] <- query db 
    "SELECT character, grade, freq, jlpt FROM kd_character WHERE character = ?" 
    (Only charId)
  
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
        { mGrade = grade
        , mStrokeCount = strokeCounts
        , mVariant = variants
        , mFreq = freq
        , mRadName = radNames
        , mJlpt = jlpt
        }
  
  return Character
    { cLiteral = character
    , cCodepoint = codepoints
    , cRadical = radicals
    , cMisc = misc
    , cDicNumber = dicNumbers
    , cQueryCode = queryCodes
    , cReadingMeaning = readingMeaning
    }

-- Helper functions for reconstruction
getCodepoints :: Connection -> Text -> IO [Codepoint]
getCodepoints db charId = do
  rows <- query db "SELECT cp_type, cp_value FROM kd_codepoint WHERE character = ?" (Only charId)
  return [Codepoint t v | (t, v) <- rows]

getRadicals :: Connection -> Text -> IO [Radical]
getRadicals db charId = do
  rows <- query db "SELECT rad_type, rad_value FROM kd_radical WHERE character = ?" (Only charId)
  return [Radical t v | (t, v) <- rows]

getStrokeCounts :: Connection -> Text -> IO [Int]
getStrokeCounts db charId = do
  rows <- query db "SELECT stroke_count FROM kd_stroke_count WHERE character = ?" (Only charId)
  return [sc | Only sc <- rows]

getVariants :: Connection -> Text -> IO [Variant]
getVariants db charId = do
  rows <- query db "SELECT var_type, var_value FROM kd_variant WHERE character = ?" (Only charId)
  return [Variant varType varValue | (varType, varValue) <- rows]

getRadicalNames :: Connection -> Text -> IO [Text]
getRadicalNames db charId = do
  rows <- query db "SELECT rad_name FROM kd_radical_name WHERE character = ?" (Only charId)
  return [rn | Only rn <- rows]

getDicNumbers :: Connection -> Text -> IO [DicNumber]
getDicNumbers db charId = do
  rows <- query db "SELECT dr_type, dr_value, m_vol, m_page FROM kd_dict_no WHERE character = ?" (Only charId)
  return [DicNumber drType drValue mVol mPage | (drType, drValue, mVol, mPage) <- rows]

getQueryCodes :: Connection -> Text -> IO [QueryCode]
getQueryCodes db charId = do
  rows <- query db "SELECT qc_type, qc_value, skip_misclass FROM kd_query_code WHERE character = ?" (Only charId)
  return [QueryCode t v skipMisclass | (t, v, skipMisclass) <- rows]

getReadingMeaning :: Connection -> Text -> IO (Maybe ReadingMeaning)
getReadingMeaning db charId = do
  -- Get nanori readings
  nanoriRows <- query db "SELECT nanori_value FROM kd_nanori WHERE character = ?" (Only charId)
  let nanoriList = [n | Only n <- nanoriRows]
  readings <- getReadings db charId
  meanings <- getMeanings db charId


  if Prelude.null readings && Prelude.null nanoriList && Prelude.null meanings
    then return Nothing
    else 
      return $ Just $ ReadingMeaning [] nanoriList

getReadings :: Connection -> Text -> IO [Reading]
getReadings db rmGroupId = do
  rows <- query db "SELECT r_type, r_value FROM kd_reading WHERE character = ?" (Only rmGroupId)
  return [Reading t v | (t, v) <- rows]

getMeanings :: Connection -> Text -> IO [Meaning]
getMeanings db rmGroupId = do
  rows <- query db "SELECT m_lang, m_value FROM kd_meaning WHERE character = ?" (Only rmGroupId)
  return [Meaning l v | (l, v) <- rows]

-- Connection initialization
initializeConnection :: ConnectionPath -> IO Connection
initializeConnection dbPath = do
  db <- open dbPath
  createTables db
  return db

-- Batch operations
insertKanjiDic2 :: Connection -> KanjiDic2 -> IO ()
insertKanjiDic2 db kd = do
  putStrLn $ "Inserting " ++ show (Prelude.length (kdCharacters kd)) ++ " characters..."
  
  -- Use transaction for better performance
  execute_ db "BEGIN TRANSACTION;"
  
  forM_ (Prelude.zip ([1..]::[Int]) (kdCharacters kd)) $ \(i, char) -> do
    insertCompleteCharacter db char
    when (i `mod` 1000 == 0) $ 
      putStrLn $ "Inserted " ++ show i ++ " characters..."
  
  execute_ db "COMMIT;"
  putStrLn "All characters inserted successfully!"

-- Query helper functions
getCharactersByGrade :: Connection -> Int -> IO [Character]
getCharactersByGrade db grade = do
  charIds <- query db "SELECT id FROM kd_character WHERE grade = ?" (Only grade)
  mapM (reconstructCharacter db) [charId | Only charId <- charIds]

getCharactersByJLPT :: Connection -> Int -> IO [Character]
getCharactersByJLPT db jlpt = do
  charIds <- query db "SELECT id FROM kd_character WHERE jlpt = ?" (Only jlpt)
  mapM (reconstructCharacter db) [charId | Only charId <- charIds]

searchCharactersByMeaning :: Connection -> Text -> IO [Character]
searchCharactersByMeaning db meaningText = do
  charIds <- 
    query db 
      (Query $ T.unwords 
        [ "SELECT DISTINCT c.id FROM kd_character c"
        -- , "JOIN rm_groups rg ON c.id = rg.character_id"
        , "JOIN kd_meanings m ON rg.id = m.rm_group_id"
        , "WHERE m.m_value LIKE ?"
        ])
      (Only $ "%" <> meaningText <> "%")
  mapM (reconstructCharacter db) [charId | Only charId <- charIds]

-- Example usage and testing
exampleUsageSql :: IO ()
exampleUsageSql = do
  db <- initializeConnection "others\\kanjidic4.db"
  
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
                    [ Reading "ja_on" "スイ" -- (Just "kan") Nothing
                    , Reading "ja_kun" "みず" -- Nothing Nothing
                    , Reading "ja_kun" "みず-" -- Nothing Nothing
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
    Just char -> do
      putStrLn $ "Found character: " ++ T.unpack (cLiteral char)
      putStrLn $ "Grade: " ++ show (mGrade . cMisc $ char)
      putStrLn $ "Stroke count: " ++ show (mStrokeCount . cMisc $ char)
    Nothing -> putStrLn "Character not found"
  
  close db

-- Main function for testing
mainSql :: IO ()
mainSql = exampleUsageSql