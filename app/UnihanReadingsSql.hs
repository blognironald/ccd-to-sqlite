{-# LANGUAGE OverloadedStrings #-}

module UnihanReadingsSql where

import qualified UnihanReadingsParser as P
import Database.SQLite.Simple
import Data.Text as T

-- SQL commands
createTableUnihanEntry :: Query
createTableUnihanEntry =
    Query (T.pack "\
        \CREATE TABLE IF NOT EXISTS unihan_reading (\
        \  character   TEXT PRIMARY KEY    \
        \, definition  TEXT NOT NULL       \
        \);")

createIndexCharacter :: Query
createIndexCharacter =
    Query (T.pack "\
        \CREATE INDEX IF NOT EXISTS idx_unihan_codepoint \
        \ON unihan_reading(character);")

createIndexDefinition :: Query
createIndexDefinition =
    Query (T.pack "\
        \CREATE INDEX IF NOT EXISTS idx_unihan_definition \
        \ON unihan_reading(definition);")

insertUnihanReading :: Query
insertUnihanReading =
    Query (T.pack "\
        \INSERT INTO unihan_reading ( \
        \  character                \
        \, definition)              \
        \VALUES (?, ?)            \
        \")

-- Haskell 'UnihanReading' structure to SQL datatype
newtype UnihanRow = UnihanRow P.UnihanReading

instance ToRow UnihanRow where
    toRow (UnihanRow entry) = toRow (
        P.uniCharacter entry,
        P.uniDefinition entry)

-- Query functions
findByCharacterQuery :: Query
findByCharacterQuery =
    Query (T.pack "\
        \SELECT character, definition \
        \FROM unihan_reading                       \
        \WHERE character = ?                     \
        \")

searchDefinitionQuery :: Query
searchDefinitionQuery =
    Query (T.pack "\
        \SELECT character, definition \
        \FROM unihan_reading                       \
        \WHERE definition LIKE ?                 \
        \")

-- Query helper functions
findByCharacter :: Connection -> Char -> IO [P.UnihanReading]
findByCharacter db char =
    query db findByCharacterQuery (Only (T.singleton char))

searchDefinition :: Connection -> Text -> IO [P.UnihanReading]
searchDefinition db searchTerm =
    query db searchDefinitionQuery (Only ("%" <> searchTerm <> "%"))

-- Statistics
countEntriesQuery :: Query
countEntriesQuery = Query "SELECT COUNT(*) FROM unihan_reading"

countEntries :: Connection -> IO Int
countEntries db = do
    [Only c] <- query_ db countEntriesQuery :: IO [Only Int]
    return c

-- Run!
runUnihanSql :: String -> [P.UnihanReading] -> IO ()
runUnihanSql dbName entryList = do
    db <- open dbName
    -- Create table
    execute_ db createTableUnihanEntry
    -- Insert entries
    executeMany db insertUnihanReading (fmap UnihanRow entryList)
    -- Create indexes
    execute_ db createIndexCharacter
    execute_ db createIndexDefinition
    close db

-- Example usage
exampleUsage :: IO ()
exampleUsage = do
    -- Sample entries
    let sampleEntries =
            [ P.UnihanReading "㐀" "(same as 丘) hillock or mound"
            , P.UnihanReading "㐁" "to lick; to taste, a mat, bamboo bark"
            , P.UnihanReading "㐂" "(non-standard Japanese variant of 喜), to like, love, enjoy; a joyful thing"
            , P.UnihanReading "㐅" "(ancient form of 五) five"
            , P.UnihanReading "㐌" "a tribe of savages in South China"
            ]

    -- Create database and insert data
    let dbName = "others//unihan.db3"
    runUnihanSql dbName sampleEntries
    putStrLn $ "Inserted " ++ show (Prelude.length sampleEntries) ++ " entries into " ++ dbName

    -- Query examples
    db <- open dbName

    putStrLn "\n=== Query Examples ==="

    -- Find by character
    putStrLn "\nSearching for character '㐅':"
    results1 <- findByCharacter db '㐅'
    mapM_ print results1

    -- Search definition
    putStrLn "\nSearching for 'ancient' in definitions:"
    results2 <- searchDefinition db "ancient"
    mapM_ print results2

    -- Count entries
    c <- countEntries db
    putStrLn $ "\nTotal entries in database: " ++ show c

    close db
