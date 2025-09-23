{-# LANGUAGE OverloadedStrings #-}

module CEdictSql where

import Database.SQLite.Simple
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (listToMaybe)
import Control.Monad (void, forM_)
import Control.Exception (bracket)

import CEdictParser (CedictEntry(..))

-- Connection operations
type ConnectionPath = String

-- Create all tables
createTables :: Connection -> IO ()
createTables db = do
  -- Main CEDICT entries table
  exec db $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS cedict_entries ("
    , "id INTEGER PRIMARY KEY AUTOINCREMENT,"
    , "traditional TEXT NOT NULL,"
    , "simplified TEXT NOT NULL,"
    , "pronunciation TEXT NOT NULL,"
    , "created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP"
    , ");"
    ]
  
  -- Meanings table (separate table for multiple meanings per entry)
  exec db $ T.unwords
    [ "CREATE TABLE IF NOT EXISTS cedict_meanings ("
    , "id INTEGER PRIMARY KEY AUTOINCREMENT,"
    , "entry_id INTEGER NOT NULL,"
    , "meaning TEXT NOT NULL,"
    , "meaning_order INTEGER NOT NULL DEFAULT 0,"
    , "FOREIGN KEY (entry_id) REFERENCES cedict_entries(id) ON DELETE CASCADE"
    , ");"
    ]
  
  -- Create indexes
  createIndexes db

createIndexes :: Connection -> IO ()
createIndexes db = do
  exec db "CREATE INDEX IF NOT EXISTS idx_cedict_traditional ON cedict_entries(traditional);"
  exec db "CREATE INDEX IF NOT EXISTS idx_cedict_simplified ON cedict_entries(simplified);"
  exec db "CREATE INDEX IF NOT EXISTS idx_cedict_pronunciation ON cedict_entries(pronunciation);"
  exec db "CREATE INDEX IF NOT EXISTS idx_cedict_meanings_entry_id ON cedict_meanings(entry_id);"
  exec db "CREATE INDEX IF NOT EXISTS idx_cedict_meanings_text ON cedict_meanings(meaning);"

-- Create optional full-text search tables
createFtsTable :: Connection -> IO ()
createFtsTable db = do
  exec db $ T.unwords
    [ "CREATE VIRTUAL TABLE IF NOT EXISTS cedict_meanings_fts USING fts5("
    , "meaning,"
    , "content='cedict_meanings',"
    , "content_rowid='id'"
    , ");"
    ]
  
  -- Create triggers to keep FTS synchronized
  exec db $ T.unwords
    [ "CREATE TRIGGER IF NOT EXISTS cedict_meanings_fts_insert AFTER INSERT ON cedict_meanings BEGIN"
    , "INSERT INTO cedict_meanings_fts(rowid, meaning) VALUES (new.id, new.meaning);"
    , "END;"
    ]
  
  exec db $ T.unwords
    [ "CREATE TRIGGER IF NOT EXISTS cedict_meanings_fts_delete AFTER DELETE ON cedict_meanings BEGIN"
    , "INSERT INTO cedict_meanings_fts(cedict_meanings_fts, rowid, meaning) VALUES('delete', old.id, old.meaning);"
    , "END;"
    ]
  
  exec db $ T.unwords
    [ "CREATE TRIGGER IF NOT EXISTS cedict_meanings_fts_update AFTER UPDATE ON cedict_meanings BEGIN"
    , "INSERT INTO cedict_meanings_fts(cedict_meanings_fts, rowid, meaning) VALUES('delete', old.id, old.meaning);"
    , "INSERT INTO cedict_meanings_fts(rowid, meaning) VALUES (new.id, new.meaning);"
    , "END;"
    ]

-- Insert a CEDICT entry and return its ID
insertCedictEntry :: Connection -> CedictEntry -> IO Int
insertCedictEntry db entry = do
  stmt <- prepare db $ T.unwords
    [ "INSERT INTO cedict_entries (traditional, simplified, pronunciation)"
    , "VALUES (?, ?, ?);"
    ]
  
  bind stmt 
    [ SQLText (ceTraditional entry)
    , SQLText (ceSimplified entry)  
    , SQLText (cePronunciation entry)
    ]
  
  void $ step stmt
  finalize stmt
  lastInsertRowId db

-- Insert meanings for an entry
insertMeanings :: Connection -> Int -> [Text] -> IO ()
insertMeanings db entryId meanings = do
  stmt <- prepare db $ T.unwords
    [ "INSERT INTO cedict_meanings (entry_id, meaning, meaning_order)"
    , "VALUES (?, ?, ?);"
    ]
  
  forM_ (zip [1..] meanings) $ \(order, meaning) -> do
    reset stmt
    bind stmt [SQLInteger entryId, SQLText meaning, SQLInteger order]
    void $ step stmt
  
  finalize stmt

-- Insert complete CEDICT entry with all meanings
insertCompleteCedictEntry :: Connection -> CedictEntry -> IO ()
insertCompleteCedictEntry db entry = do
  entryId <- insertCedictEntry db entry
  insertMeanings db entryId (ceMeanings entry)

-- Batch insert multiple entries in a transaction
insertMultipleCedictEntries :: Connection -> [CedictEntry] -> IO ()
insertMultipleCedictEntries db entries = do
  execute_ db "BEGIN TRANSACTION;"
  mapM_ (insertCompleteCedictEntry db) entries
  execute_ db "COMMIT;"

-- Query functions

-- Find entries by traditional characters
findByTraditional :: Connection -> Text -> IO [CedictEntry]
findByTraditional db traditional = do
  stmt <- prepare db $ T.unwords
    [ "SELECT e.id, e.traditional, e.simplified, e.pronunciation,"
    , "GROUP_CONCAT(m.meaning, '|') as meanings"
    , "FROM cedict_entries e"
    , "LEFT JOIN cedict_meanings m ON e.id = m.entry_id"
    , "WHERE e.traditional = ?"
    , "GROUP BY e.id"
    , "ORDER BY m.meaning_order;"
    ]
  
  bind stmt [SQLText traditional]
  results <- collectCedictRows stmt []
  finalize stmt
  return results

-- Find entries by simplified characters
findBySimplified :: Connection -> Text -> IO [CedictEntry]
findBySimplified db simplified = do
  stmt <- prepare db $ T.unwords
    [ "SELECT e.id, e.traditional, e.simplified, e.pronunciation,"
    , "GROUP_CONCAT(m.meaning, '|') as meanings"
    , "FROM cedict_entries e"
    , "LEFT JOIN cedict_meanings m ON e.id = m.entry_id"
    , "WHERE e.simplified = ?"
    , "GROUP BY e.id"
    , "ORDER BY m.meaning_order;"
    ]
  
  bind stmt [SQLText simplified]
  results <- collectCedictRows stmt []
  finalize stmt
  return results

-- Find entries by pronunciation
findByPronunciation :: Connection -> Text -> IO [CedictEntry]
findByPronunciation db pronunciation = do
  stmt <- prepare db $ T.unwords
    [ "SELECT e.id, e.traditional, e.simplified, e.pronunciation,"
    , "GROUP_CONCAT(m.meaning, '|') as meanings"
    , "FROM cedict_entries e"
    , "LEFT JOIN cedict_meanings m ON e.id = m.entry_id"
    , "WHERE e.pronunciation = ?"
    , "GROUP BY e.id"
    , "ORDER BY m.meaning_order;"
    ]
  
  bind stmt [SQLText pronunciation]
  results <- collectCedictRows stmt []
  finalize stmt
  return results

-- Search meanings (case-insensitive substring search)
searchByMeaning :: Connection -> Text -> IO [CedictEntry]
searchByMeaning db searchTerm = do
  stmt <- prepare db $ T.unwords
    [ "SELECT DISTINCT e.id, e.traditional, e.simplified, e.pronunciation,"
    , "GROUP_CONCAT(m.meaning, '|') as meanings"
    , "FROM cedict_entries e"
    , "JOIN cedict_meanings m ON e.id = m.entry_id"
    , "WHERE LOWER(m.meaning) LIKE LOWER(?)"
    , "GROUP BY e.id"
    , "ORDER BY m.meaning_order;"
    ]
  
  bind stmt [SQLText $ "%" <> searchTerm <> "%"]
  results <- collectCedictRows stmt []
  finalize stmt
  return results

-- Full-text search in meanings (requires FTS table)
searchMeaningsFts :: Connection -> Text -> IO [CedictEntry]
searchMeaningsFts db searchTerm = do
  stmt <- prepare db $ T.unwords
    [ "SELECT DISTINCT e.id, e.traditional, e.simplified, e.pronunciation,"
    , "GROUP_CONCAT(m.meaning, '|') as meanings"
    , "FROM cedict_entries e"
    , "JOIN cedict_meanings m ON e.id = m.entry_id"
    , "JOIN cedict_meanings_fts fts ON m.id = fts.rowid"
    , "WHERE cedict_meanings_fts MATCH ?"
    , "GROUP BY e.id"
    , "ORDER BY rank;"
    ]
  
  bind stmt [SQLText searchTerm]
  results <- collectCedictRows stmt []
  finalize stmt
  return results

-- Get all entries (with pagination)
getAllEntries :: Connection -> Int -> Int -> IO [CedictEntry]
getAllEntries db offset limit = do
  stmt <- prepare db $ T.unwords
    [ "SELECT e.id, e.traditional, e.simplified, e.pronunciation,"
    , "GROUP_CONCAT(m.meaning, '|') as meanings"
    , "FROM cedict_entries e"
    , "LEFT JOIN cedict_meanings m ON e.id = m.entry_id"
    , "GROUP BY e.id"
    , "ORDER BY e.id"
    , "LIMIT ? OFFSET ?;"
    ]
  
  bind stmt [SQLInteger (fromIntegral limit), SQLInteger (fromIntegral offset)]
  results <- collectCedictRows stmt []
  finalize stmt
  return results

-- Count total entries
countEntries :: Connection -> IO Int
countEntries db = do
  stmt <- prepare db "SELECT COUNT(*) FROM cedict_entries;"
  void $ step stmt
  count <- column stmt 0
  finalize stmt
  case count of
    SQLInteger n -> return (fromIntegral n)
    _ -> return 0

-- Helper function to collect rows and reconstruct CedictEntry objects
collectCedictRows :: Statement -> [CedictEntry] -> IO [CedictEntry]
collectCedictRows stmt acc = do
  result <- step stmt
  case result of
    Row -> do
      _ <- column stmt 0 :: IO SQLData  -- id (we don't need it for reconstruction)
      traditional <- column stmt 1
      simplified <- column stmt 2
      pronunciation <- column stmt 3
      meanings <- column stmt 4
      
      let entry = CedictEntry
            { ceTraditional = sqlToText traditional
            , ceSimplified = sqlToText simplified
            , cePronunciation = sqlToText pronunciation
            , ceMeanings = parseMeanings (sqlToText meanings)
            }
      collectCedictRows stmt (entry : acc)
    Done -> return (reverse acc)

-- Helper function to parse concatenated meanings back into list
parseMeanings :: Text -> [Text]
parseMeanings "" = []
parseMeanings meaningsText = 
  filter (not . T.null) $ map T.strip $ T.splitOn "|" meaningsText

-- Utility functions
sqlToText :: SQLData -> Text
sqlToText (SQLText t) = t
sqlToText _ = ""

-- Database initialization
initializeConnection :: ConnectionPath -> IO Connection
initializeConnection dbPath = do
  db <- open (T.pack dbPath)
  createTables db
  return db

-- Initialize connection with FTS enabled
initializeConnectionWithFts :: ConnectionPath -> IO Connection
initializeConnectionWithFts dbPath = do
  db <- open (T.pack dbPath)
  createTables db
  createFtsTable db
  return db

-- Statistics and maintenance functions
getDatabaseStats :: Connection -> IO (Int, Int)
getDatabaseStats db = do
  entryCount <- countEntries db
  meaningCount <- countMeanings db
  return (entryCount, meaningCount)
  where
    countMeanings conn = do
      stmt <- prepare conn "SELECT COUNT(*) FROM cedict_meanings;"
      void $ step stmt
      count <- column stmt 0
      finalize stmt
      case count of
        SQLInteger n -> return (fromIntegral n)
        _ -> return 0

-- Rebuild FTS index
rebuildFtsIndex :: Connection -> IO ()
rebuildFtsIndex db = do
  execute_ db "INSERT INTO cedict_meanings_fts(cedict_meanings_fts) VALUES('rebuild');"

-- Example usage and testing
exampleUsage :: IO ()
exampleUsage = do
  db <- initializeConnection "cedict.db"
  
  -- Sample entries from the cedict_ts.u8 file
  let sampleEntries = 
        [ CedictEntry "河童" "河童" "he2 tong2" 
            ["kappa, a child-size humanoid water creature in Japanese folklore"]
        , CedictEntry "河粉" "河粉" "he2 fen3" 
            ["hor fun, a type of wide, flat rice noodle"]
        , CedictEntry "河蚌" "河蚌" "he2 bang4" 
            ["mussels", "bivalves grown in rivers and lakes"]
        , CedictEntry "河蟹" "河蟹" "he2 xie4" 
            ["river crab", "Internet censorship (pun on \"harmonious\" 和諧|和谐[he2 xie2], which is blocked by the great firewall of China)"]
        , CedictEntry "河西" "河西" "He2 xi1" 
            ["land west of the Yellow river", "Shaanxi, Qinghai and Gansu provinces"]
        ]
  
  -- Insert sample entries
  putStrLn "Inserting sample entries..."
  insertMultipleCedictEntries db sampleEntries
  
  -- Test queries
  putStrLn "\n=== Query Examples ==="
  
  putStrLn "\nFinding '河童' by traditional characters:"
  results1 <- findByTraditional db "河童"
  mapM_ (putStrLn . T.unpack . prettyPrintCedictEntry) results1
  
  putStrLn "\nSearching for entries containing 'water':"
  results2 <- searchByMeaning db "water"
  mapM_ (putStrLn . T.unpack . prettyPrintCedictEntry) results2
  
  putStrLn "\nFinding by pronunciation 'he2 fen3':"
  results3 <- findByPronunciation db "he2 fen3"
  mapM_ (putStrLn . T.unpack . prettyPrintCedictEntry) results3
  
  -- Get database statistics
  (entryCount, meaningCount) <- getDatabaseStats db
  putStrLn $ "\nDatabase contains " ++ show entryCount ++ " entries and " ++ show meaningCount ++ " meanings."
  
  close db

-- Pretty print a CEDICT entry
prettyPrintCedictEntry :: CedictEntry -> Text
prettyPrintCedictEntry entry = T.unlines
  [ "Traditional: " <> ceTraditional entry
  , "Simplified:  " <> ceSimplified entry  
  , "Pinyin:      " <> cePronunciation entry
  , "Meanings:    " <> T.intercalate "; " (ceMeanings entry)
  , "─────────────────────────────────────────────────"
  ]

-- Function to import from a parsed CEDICT file
importFromCedictFile :: Connection -> FilePath -> IO ()
importFromCedictFile db filePath = do
  putStrLn $ "Parsing CEDICT file: " ++ filePath
  parseResult <- parseCedictFromFile filePath
  case parseResult of
    Left err -> putStrLn $ "Parse error: " ++ errorBundlePretty err
    Right entries -> do
      putStrLn $ "Successfully parsed " ++ show (length entries) ++ " entries"
      putStrLn "Inserting into database..."
      insertMultipleCedictEntries db entries
      putStrLn "Import completed!"
  where
    -- You'll need to import this from CEdictParser
    parseCedictFromFile = error "Import parseCedictFromFile from CEdictParser module"
    errorBundlePretty = error "Import errorBundlePretty from Text.Megaparsec"

-- Main function for testing
mainCEdictSql :: IO ()
mainCEdictSql = exampleUsage