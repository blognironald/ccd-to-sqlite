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
    [ "CREATE TABLE IF NOT EXISTS characters ("
    , "id INTEGER PRIMARY KEY AUTOINCREMENT,"
    , "literal TEXT NOT NULL UNIQUE,"
    , "grade INTEGER,"
    , "freq INTEGER,"
    , "jlpt INTEGER"
    , ");"
    ]
  
  -- Codepoints table
  execute_ db $ Query $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS codepoints ("
    , "id INTEGER PRIMARY KEY AUTOINCREMENT,"
    , "character_id INTEGER NOT NULL,"
    , "cp_type TEXT NOT NULL,"
    , "cp_value TEXT NOT NULL,"
    , "FOREIGN KEY (character_id) REFERENCES characters(id) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Radicals table
  execute_ db $ Query $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS radicals ("
    , "id INTEGER PRIMARY KEY AUTOINCREMENT,"
    , "character_id INTEGER NOT NULL,"
    , "rad_type TEXT NOT NULL,"
    , "rad_value INTEGER NOT NULL,"
    , "FOREIGN KEY (character_id) REFERENCES characters(id) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Stroke counts table
  execute_ db $ Query $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS stroke_counts ("
    , "id INTEGER PRIMARY KEY AUTOINCREMENT,"
    , "character_id INTEGER NOT NULL,"
    , "stroke_count INTEGER NOT NULL,"
    , "FOREIGN KEY (character_id) REFERENCES characters(id) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Variants table
  execute_ db $ Query $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS variants ("
    , "id INTEGER PRIMARY KEY AUTOINCREMENT,"
    , "character_id INTEGER NOT NULL,"
    , "var_type TEXT NOT NULL,"
    , "var_value TEXT NOT NULL,"
    , "FOREIGN KEY (character_id) REFERENCES characters(id) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Radical names table
  execute_ db $ Query $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS radical_names ("
    , "id INTEGER PRIMARY KEY AUTOINCREMENT,"
    , "character_id INTEGER NOT NULL,"
    , "rad_name TEXT NOT NULL,"
    , "FOREIGN KEY (character_id) REFERENCES characters(id) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Dictionary numbers table
  execute_ db $ Query $ T.unwords
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
  execute_ db $ Query $ T.unwords
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
  execute_ db $ Query $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS rm_groups ("
    , "id INTEGER PRIMARY KEY AUTOINCREMENT,"
    , "character_id INTEGER NOT NULL,"
    , "group_order INTEGER NOT NULL DEFAULT 0,"
    , "FOREIGN KEY (character_id) REFERENCES characters(id) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Readings table
  execute_ db $ Query $ T.unwords
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
  execute_ db $ Query $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS meanings ("
    , "id INTEGER PRIMARY KEY AUTOINCREMENT,"
    , "rm_group_id INTEGER NOT NULL,"
    , "m_lang TEXT DEFAULT 'en',"
    , "m_value TEXT NOT NULL,"
    , "FOREIGN KEY (rm_group_id) REFERENCES rm_groups(id) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Nanori table
  execute_ db $ Query $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS nanori ("
    , "id INTEGER PRIMARY KEY AUTOINCREMENT,"
    , "character_id INTEGER NOT NULL,"
    , "nanori_value TEXT NOT NULL,"
    , "FOREIGN KEY (character_id) REFERENCES characters(id) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Create indexes
  createIndexes db

createIndexes :: Connection -> IO ()
createIndexes db = do
  execute_ db "CREATE INDEX IF NOT EXISTS idx_characters_literal ON characters(literal);"
  execute_ db "CREATE INDEX IF NOT EXISTS idx_characters_grade ON characters(grade);"
  execute_ db "CREATE INDEX IF NOT EXISTS idx_characters_freq ON characters(freq);"
  execute_ db "CREATE INDEX IF NOT EXISTS idx_characters_jlpt ON characters(jlpt);"
  execute_ db "CREATE INDEX IF NOT EXISTS idx_codepoints_character_id ON codepoints(character_id);"
  execute_ db "CREATE INDEX IF NOT EXISTS idx_codepoints_type ON codepoints(cp_type);"
  execute_ db "CREATE INDEX IF NOT EXISTS idx_radicals_character_id ON radicals(character_id);"
  execute_ db "CREATE INDEX IF NOT EXISTS idx_readings_rm_group_id ON readings(rm_group_id);"
  execute_ db "CREATE INDEX IF NOT EXISTS idx_readings_type ON readings(r_type);"
  execute_ db "CREATE INDEX IF NOT EXISTS idx_meanings_rm_group_id ON meanings(rm_group_id);"
  execute_ db "CREATE INDEX IF NOT EXISTS idx_meanings_lang ON meanings(m_lang);"

-- Insert a character and return its ID
insertCharacter :: Connection -> Character -> IO Int
insertCharacter db char = do
  let misc = cMisc char
  
  execute db "INSERT INTO characters (literal, grade, freq, jlpt) VALUES (?, ?, ?, ?)"
    (cLiteral char, mGrade misc, mFreq misc, mJlpt misc)
  
  fromIntegral <$> lastInsertRowId db

-- Insert codepoints for a character
insertCodepoints :: Connection -> Int -> [Codepoint] -> IO ()
insertCodepoints db charId codepoints = do
  forM_ codepoints $ \cp -> do
    execute db "INSERT INTO codepoints (character_id, cp_type, cp_value) VALUES (?, ?, ?)"
      (charId, cpType cp, cpValue cp)

-- Insert radicals for a character
insertRadicals :: Connection -> Int -> [Radical] -> IO ()
insertRadicals db charId radicals = do
  forM_ radicals $ \rad -> do
    execute db "INSERT INTO radicals (character_id, rad_type, rad_value) VALUES (?, ?, ?)"
      (charId, radType rad, radValue rad)

-- Insert stroke counts for a character
insertStrokeCounts :: Connection -> Int -> [Int] -> IO ()
insertStrokeCounts db charId strokeCounts = do
  forM_ strokeCounts $ \sc -> do
    execute db "INSERT INTO stroke_counts (character_id, stroke_count) VALUES (?, ?)"
      (charId, sc)

-- Insert variants for a character
insertVariants :: Connection -> Int -> [Variant] -> IO ()
insertVariants db charId variants = do
  forM_ variants $ \var -> do
    execute db "INSERT INTO variants (character_id, var_type, var_value) VALUES (?, ?, ?)"
      (charId, vType var, vValue var)

-- Insert radical names for a character
insertRadicalNames :: Connection -> Int -> [Text] -> IO ()
insertRadicalNames db charId radNames = do
  forM_ radNames $ \rn -> do
    execute db "INSERT INTO radical_names (character_id, rad_name) VALUES (?, ?)"
      (charId, rn)

-- Insert dictionary numbers for a character
insertDicNumbers :: Connection -> Int -> [DicNumber] -> IO ()
insertDicNumbers db charId dicNumbers = do
  forM_ dicNumbers $ \dn -> do
    execute db "INSERT INTO dic_numbers (character_id, dr_type, dr_value, m_vol, m_page) VALUES (?, ?, ?, ?, ?)"
      (charId, dnType dn, dnValue dn, dnVolume dn, dnPage dn)

-- Insert query codes for a character
insertQueryCodes :: Connection -> Int -> [QueryCode] -> IO ()
insertQueryCodes db charId queryCodes = do
  forM_ queryCodes $ \qc -> do
    execute db "INSERT INTO query_codes (character_id, qc_type, qc_value, skip_misclass) VALUES (?, ?, ?, ?)"
      (charId, qcType qc, qcValue qc, qcSkipMisclass qc)

-- Insert reading-meaning group and return its ID
insertRMGroup :: Connection -> Int -> Int -> IO Int
insertRMGroup db charId groupOrder = do
  execute db "INSERT INTO rm_groups (character_id, group_order) VALUES (?, ?)"
    (charId, groupOrder)
  fromIntegral <$> lastInsertRowId db

-- Insert readings for a reading-meaning group
insertReadings :: Connection -> Int -> [Reading] -> IO ()
insertReadings db rmGroupId readings = do
  forM_ readings $ \r -> do
    execute db "INSERT INTO readings (rm_group_id, r_type, r_value, on_type, r_status) VALUES (?, ?, ?, ?, ?)"
      (rmGroupId, rType r, rValue r, rOnType r, rRStatus r)

-- Insert meanings for a reading-meaning group
insertMeanings :: Connection -> Int -> [Meaning] -> IO ()
insertMeanings db rmGroupId meanings = do
  forM_ meanings $ \m -> do
    execute db "INSERT INTO meanings (rm_group_id, m_lang, m_value) VALUES (?, ?, ?)"
      (rmGroupId, mLang m, mValue m)

-- Insert nanori readings for a character
insertNanori :: Connection -> Int -> [Text] -> IO ()
insertNanori db charId nanoriList = do
  forM_ nanoriList $ \n -> do
    execute db "INSERT INTO nanori (character_id, nanori_value) VALUES (?, ?)"
      (charId, n)

-- Insert complete character with all related data
insertCompleteCharacter :: Connection -> Character -> IO ()
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
      forM_ (Prelude.zip [0..] (rmGroups rm)) $ \(groupOrder, rmGroup) -> do
        rmGroupId <- insertRMGroup db charId groupOrder
        insertReadings db rmGroupId (rmgReadings rmGroup)
        insertMeanings db rmGroupId (rmgMeanings rmGroup)

-- Query functions

-- Find character by literal
findCharacterByLiteral :: Connection -> Text -> IO (Maybe Character)
findCharacterByLiteral db literal = do
  results <- query db "SELECT id FROM characters WHERE literal = ? LIMIT 1" (Only literal)
  case results of
    [Only charId] -> Just <$> reconstructCharacter db charId
    [] -> return Nothing
    _ -> return Nothing

-- Reconstruct character from database
reconstructCharacter :: Connection -> Int -> IO Character
reconstructCharacter db charId = do
  -- Get basic character info
  [(literal, grade, freq, jlpt)] <- query db 
    "SELECT literal, grade, freq, jlpt FROM characters WHERE id = ?" 
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
    { cLiteral = literal
    , cCodepoint = codepoints
    , cRadical = radicals
    , cMisc = misc
    , cDicNumber = dicNumbers
    , cQueryCode = queryCodes
    , cReadingMeaning = readingMeaning
    }

-- Helper functions for reconstruction
getCodepoints :: Connection -> Int -> IO [Codepoint]
getCodepoints db charId = do
  rows <- query db "SELECT cp_type, cp_value FROM codepoints WHERE character_id = ?" (Only charId)
  return [Codepoint t v | (t, v) <- rows]

getRadicals :: Connection -> Int -> IO [Radical]
getRadicals db charId = do
  rows <- query db "SELECT rad_type, rad_value FROM radicals WHERE character_id = ?" (Only charId)
  return [Radical t v | (t, v) <- rows]

getStrokeCounts :: Connection -> Int -> IO [Int]
getStrokeCounts db charId = do
  rows <- query db "SELECT stroke_count FROM stroke_counts WHERE character_id = ?" (Only charId)
  return [sc | Only sc <- rows]

getVariants :: Connection -> Int -> IO [Variant]
getVariants db charId = do
  rows <- query db "SELECT var_type, var_value FROM variants WHERE character_id = ?" (Only charId)
  return [Variant varType varValue | (varType, varValue) <- rows]

getRadicalNames :: Connection -> Int -> IO [Text]
getRadicalNames db charId = do
  rows <- query db "SELECT rad_name FROM radical_names WHERE character_id = ?" (Only charId)
  return [rn | Only rn <- rows]

getDicNumbers :: Connection -> Int -> IO [DicNumber]
getDicNumbers db charId = do
  rows <- query db "SELECT dr_type, dr_value, m_vol, m_page FROM dic_numbers WHERE character_id = ?" (Only charId)
  return [DicNumber drType drValue mVol mPage | (drType, drValue, mVol, mPage) <- rows]

getQueryCodes :: Connection -> Int -> IO [QueryCode]
getQueryCodes db charId = do
  rows <- query db "SELECT qc_type, qc_value, skip_misclass FROM query_codes WHERE character_id = ?" (Only charId)
  return [QueryCode t v skipMisclass | (t, v, skipMisclass) <- rows]

getReadingMeaning :: Connection -> Int -> IO (Maybe ReadingMeaning)
getReadingMeaning db charId = do
  -- Get nanori readings
  nanoriRows <- query db "SELECT nanori_value FROM nanori WHERE character_id = ?" (Only charId)
  let nanoriList = [n | Only n <- nanoriRows]
  
  -- Get reading-meaning groups
  rmGroupRows <- query db "SELECT id, group_order FROM rm_groups WHERE character_id = ? ORDER BY group_order" (Only charId) :: IO [(Int, Int)]
  
  if Prelude.null rmGroupRows && Prelude.null nanoriList
    then return Nothing
    else do
      groups <- forM rmGroupRows $ \(rmGroupId, _groupOrder) -> do
        readings <- getReadings db rmGroupId
        meanings <- getMeanings db rmGroupId
        return $ RMGroup readings meanings
      
      return $ Just $ ReadingMeaning groups nanoriList

getReadings :: Connection -> Int -> IO [Reading]
getReadings db rmGroupId = do
  rows <- query db "SELECT r_type, r_value, on_type, r_status FROM readings WHERE rm_group_id = ?" (Only rmGroupId)
  return [Reading t v onType rStatus | (t, v, onType, rStatus) <- rows]

getMeanings :: Connection -> Int -> IO [Meaning]
getMeanings db rmGroupId = do
  rows <- query db "SELECT m_lang, m_value FROM meanings WHERE rm_group_id = ?" (Only rmGroupId)
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
  charIds <- query db "SELECT id FROM characters WHERE grade = ?" (Only grade)
  mapM (reconstructCharacter db) [charId | Only charId <- charIds]

getCharactersByJLPT :: Connection -> Int -> IO [Character]
getCharactersByJLPT db jlpt = do
  charIds <- query db "SELECT id FROM characters WHERE jlpt = ?" (Only jlpt)
  mapM (reconstructCharacter db) [charId | Only charId <- charIds]

searchCharactersByMeaning :: Connection -> Text -> IO [Character]
searchCharactersByMeaning db meaningText = do
  charIds <- 
    query db 
      (Query $ T.unwords 
        [ "SELECT DISTINCT c.id FROM characters c"
        , "JOIN rm_groups rg ON c.id = rg.character_id"
        , "JOIN meanings m ON rg.id = m.rm_group_id"
        , "WHERE m.m_value LIKE ?"
        ])
      (Only $ "%" <> meaningText <> "%")
  mapM (reconstructCharacter db) [charId | Only charId <- charIds]

-- Example usage and testing
exampleUsageSql :: IO ()
exampleUsageSql = do
  db <- initializeConnection "others\\exampleKanjiDic2.db"
  
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
    Just char -> do
      putStrLn $ "Found character: " ++ T.unpack (cLiteral char)
      putStrLn $ "Grade: " ++ show (mGrade . cMisc $ char)
      putStrLn $ "Stroke count: " ++ show (mStrokeCount . cMisc $ char)
    Nothing -> putStrLn "Character not found"
  
  close db

-- Main function for testing
mainSql :: IO ()
mainSql = exampleUsageSql