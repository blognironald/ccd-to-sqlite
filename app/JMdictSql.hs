{-# LANGUAGE OverloadedStrings #-}

module JMdictSql where

import qualified JMdictParser as P
import Database.SQLite.Simple
import Data.Text as T
import Control.Monad

-- SQL table creation commands
createTableEntry :: Query
createTableEntry = Query $ T.pack "\
    \CREATE TABLE IF NOT EXISTS entry (\
    \    seq_id INTEGER PRIMARY KEY\
    \);"

createTableKanjiElement :: Query
createTableKanjiElement = Query $ T.pack "\
    \CREATE TABLE IF NOT EXISTS kanji (\
    \    kanji_text TEXT NOT NULL,\
    \    entry_seq INTEGER NOT NULL,\
    \    kanji_text_no INTEGER NOT NULL\
    \);"

createTableKanjiInfo :: Query
createTableKanjiInfo = Query $ T.pack "\
    \CREATE TABLE IF NOT EXISTS kanji_info (\
    \    info_text TEXT NOT NULL,\
    \    entry_seq INTEGER NOT NULL,\
    \    kanji_text_no INTEGER NOT NULL,\
    \    FOREIGN KEY(entry_seq, kanji_text_no) REFERENCES kanji(entry_seq, kanji_text_no)\
    \);"

createTableKanjiPriority :: Query
createTableKanjiPriority = Query $ T.pack "\
    \CREATE TABLE IF NOT EXISTS kanji_priority (\
    \    priority_text TEXT NOT NULL,\
    \    entry_seq INTEGER NOT NULL,\
    \    kanji_text_no INTEGER NOT NULL,\
    \    FOREIGN KEY(entry_seq, kanji_text_no) REFERENCES kanji(entry_seq, kanji_text_no)\
    \);"

createTableReadingElement :: Query
createTableReadingElement = Query $ T.pack "\
    \CREATE TABLE IF NOT EXISTS reading (\
    \    reading_text TEXT NOT NULL,\
    \    no_kanji BOOLEAN NOT NULL,\
    \    entry_seq INTEGER NOT NULL,\
    \    reading_text_no INTEGER NOT NULL\
    \);"

createTableReadingRestriction :: Query
createTableReadingRestriction = Query $ T.pack "\
    \CREATE TABLE IF NOT EXISTS reading_restriction (\
    \    restriction_text TEXT NOT NULL,\
    \    reading_text_no INTEGER NOT NULL,\
    \    entry_seq INTEGER NOT NULL,\
    \    FOREIGN KEY(entry_seq, reading_text_no) REFERENCES reading(entry_seq, reading_text_no)\
    \);"

createTableReadingInfo :: Query
createTableReadingInfo = Query $ T.pack "\
    \CREATE TABLE IF NOT EXISTS reading_info (\
    \    info_text TEXT NOT NULL,\
    \    reading_text_no INTEGER NOT NULL,\
    \    entry_seq INTEGER NOT NULL,\
    \    FOREIGN KEY(entry_seq, reading_text_no) REFERENCES reading(entry_seq, reading_text_no)\
    \);"

createTableReadingPriority :: Query
createTableReadingPriority = Query $ T.pack "\
    \CREATE TABLE IF NOT EXISTS reading_priority (\
    \    priority_text TEXT NOT NULL,\
    \    reading_text_no INTEGER NOT NULL,\
    \    entry_seq INTEGER NOT NULL,\
    \    FOREIGN KEY(entry_seq, reading_text_no) REFERENCES reading(entry_seq, reading_text_no)\
    \);"

createTableSense :: Query
createTableSense = Query $ T.pack "\
    \CREATE TABLE IF NOT EXISTS sense (\
    \    sense_id INTEGER PRIMARY KEY AUTOINCREMENT,\
    \    entry_id INTEGER NOT NULL,\
    \    FOREIGN KEY(entry_id) REFERENCES entry(seq_id)\
    \);"

createTableGloss :: Query
createTableGloss = Query $ T.pack "\
    \CREATE TABLE IF NOT EXISTS gloss (\
    \    sense_id INTEGER NOT NULL,\
    \    gloss_text TEXT NOT NULL,\
    \    lang TEXT,\
    \    g_type TEXT,\
    \    g_gend TEXT,\
    \    FOREIGN KEY(sense_id) REFERENCES sense(sense_id)\
    \);"

createTableExample :: Query
createTableExample = Query $ T.pack "\
    \CREATE TABLE IF NOT EXISTS example (\
    \    sense_id INTEGER NOT NULL,\
    \    japanese TEXT NOT NULL,\
    \    english TEXT NOT NULL,\
    \    source TEXT,\
    \    source_type TEXT,\
    \    FOREIGN KEY(sense_id) REFERENCES sense(sense_id)\
    \);"

-- Insert commands
insertEntry :: Query
insertEntry = Query "INSERT INTO entry (seq_id) VALUES (?);"

insertKanji :: Query
insertKanji = Query "INSERT INTO kanji (kanji_text, entry_seq, kanji_text_no) VALUES (?, ?, ?);"

insertKanjiInfo :: Query
insertKanjiInfo = Query "\
    \INSERT INTO kanji_info (info_text, entry_seq, kanji_text_no) \
    \VALUES (?, ?, ?);"

insertKanjiPriority :: Query
insertKanjiPriority = Query "\
    \INSERT INTO kanji_priority (priority_text, entry_seq, kanji_text_no) \
    \VALUES (?, ?, ?);"

insertReadingElement :: Query
insertReadingElement = Query "\
    \INSERT INTO reading (reading_text, entry_seq, no_kanji, reading_text_no) \
    \VALUES (?, ?, ?, ?);"

insertReadingRestriction :: Query
insertReadingRestriction = Query "\
    \INSERT INTO reading_restriction (restriction_text, entry_seq, reading_text_no) \
    \VALUES (?, ?, ?);"

insertReadingInfo :: Query
insertReadingInfo = Query "\
    \INSERT INTO reading_info (info_text, entry_seq, reading_text_no) \
    \VALUES (?, ?, ?);"

insertReadingPriority :: Query
insertReadingPriority = Query "\
    \INSERT INTO reading_priority (priority_text, entry_seq, reading_text_no) \
    \VALUES (?, ?, ?);"

insertSense :: Query
insertSense = Query "\
    \INSERT INTO sense (entry_id) \
    \VALUES (?);"

insertGloss :: Query
insertGloss = Query "\
    \INSERT INTO gloss (sense_id, gloss_text, lang, g_type, g_gend) \
    \VALUES (?, ?, ?, ?, ?);"

insertExample :: Query
insertExample = Query "\
    \INSERT INTO example (sense_id, japanese, english, source, source_type) \
    \VALUES (?, ?, ?, ?, ?);"

-- Helper functions to convert from Parser types to SQL rows
newtype EntryRow = EntryRow P.JMdictEntry
instance ToRow EntryRow where
    toRow (EntryRow entry) = toRow (Only (P.entrySeq entry))

data KanjiRow = KanjiRow Text Int Int
instance ToRow KanjiRow where
    toRow (KanjiRow kanji entrySeqInt kanjiTextNo) = toRow (kanji, entrySeqInt, kanjiTextNo)

data ReadingRow = ReadingRow Text Bool Int Int
instance ToRow ReadingRow where
    toRow (ReadingRow reading noKanji entrySeqInt readingNo ) =
        toRow (reading, noKanji, entrySeqInt, readingNo)

data GlossRow = GlossRow Int P.Gloss
instance ToRow GlossRow where
    toRow (GlossRow senseId gloss) =
        toRow (senseId, P.glossText gloss, P.glossLang gloss,
               P.glossType gloss, P.glossGender gloss)

data ExampleRow = ExampleRow Int P.Example
instance ToRow ExampleRow where
    toRow (ExampleRow senseId ex) =
        toRow (senseId, P.exampleJapanese ex, P.exampleEnglish ex,
               P.exampleSource ex, P.exampleSourceType ex)

-- Main function to run the SQL insertions
runJMdictSql :: String -> [P.JMdictEntry] -> IO ()
runJMdictSql dbName entries = do
    db <- open dbName

    -- Create tables
    execute_ db createTableEntry -- to delete
    execute_ db createTableKanjiElement
    execute_ db createTableKanjiPriority
    execute_ db createTableKanjiInfo
    execute_ db createTableReadingElement
    execute_ db createTableReadingRestriction
    execute_ db createTableReadingInfo
    execute_ db createTableReadingPriority
    execute_ db createTableSense
    execute_ db createTableGloss
    execute_ db createTableExample

    -- Insert entries and their related data
    mapM_ (insertEntryData db) entries

    close db

insertEntryData :: Connection -> P.JMdictEntry -> IO ()
insertEntryData db entry = do
    -- Insert main entry
    execute db insertEntry (EntryRow entry)
    let entryId = P.entrySeq entry

    -- Insert kanji elements
    -- mapM_ (\k -> execute db insertKanji $
    --        KanjiRow (P.kanjiText k) entryId)
    --        (P.kanjiElements entry)
    mapM_ 
        (\(kElem, kElem_idx) -> do
            execute db insertKanji $ KanjiRow (P.kanjiText kElem) entryId kElem_idx
            mapM_ 
                (\info -> execute db insertKanjiInfo $ KanjiRow info entryId kElem_idx) 
                (P.kanjiInfo kElem)
            mapM_ 
                (\priority -> execute db insertKanjiPriority $ KanjiRow priority entryId kElem_idx) 
                (P.kanjiPriority kElem)
        )
        (Prelude.zip (P.kanjiElements entry) [1..])

    -- Insert reading elements
    -- mapM_ (\r -> execute db insertReading $
    --        ReadingRow (P.readingText r) entryId (P.readingNoKanji r))
    --        (P.readingElements entry)
    mapM_ 
        (\(rElem, rElem_idx) -> do
            execute db insertReadingElement $ ReadingRow (P.readingText rElem) (P.readingNoKanji rElem) entryId rElem_idx
            mapM_ 
                (\restriction -> execute db insertReadingRestriction $ KanjiRow restriction entryId rElem_idx) 
                (P.readingRestrictions rElem)
            mapM_ 
                (\info -> execute db insertReadingInfo $ KanjiRow info entryId rElem_idx) 
                (P.readingInfo rElem)
            mapM_ 
                (\priority -> execute db insertReadingPriority $ KanjiRow priority entryId rElem_idx) 
                (P.readingPriority rElem)
        )
        (Prelude.zip (P.readingElements entry) [1..])

    -- Insert senses and their related data
    mapM_ (insertSenseData db entryId) (P.senses entry)

insertSenseData :: Connection -> Int -> P.Sense -> IO ()
insertSenseData db entryId sense = do
    -- Insert sense and get its ID
    execute db insertSense (Only entryId)
    [Only senseId] <- query_ db "SELECT last_insert_rowid()" :: IO [Only Int]

    -- Insert glosses
    mapM_ (execute db insertGloss . GlossRow senseId)
           (P.glosses sense)

    -- Insert examples
    mapM_ (execute db insertExample . ExampleRow senseId)
           (P.example sense)