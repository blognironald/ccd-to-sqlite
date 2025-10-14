{-# LANGUAGE OverloadedStrings #-}

module JMdictSql where

import qualified JMdictParser as P
import Database.SQLite.Simple
import Data.Text as T

-- SQL table creation commands
-- Kanji
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

-- Reading
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

-- Sense
createTableSense :: Query
createTableSense = Query $ T.pack "\
    \CREATE TABLE IF NOT EXISTS sense (\
    \    entry_seq INTEGER NOT NULL,\
    \    sense_no INTEGER NOT NULL\
    \);"

createTableSenseRestriction :: Query
createTableSenseRestriction = Query $ T.pack "\
    \CREATE TABLE IF NOT EXISTS sense_restriction (\
    \    restriction_text TEXT NOT NULL,\
    \    entry_seq INTEGER NOT NULL,\
    \    sense_no INTEGER NOT NULL,\
    \    FOREIGN KEY(entry_seq, sense_no) REFERENCES sense(entry_seq, sense_no)\
    \);"

createTableXRef :: Query
createTableXRef = Query $ T.pack "\
    \CREATE TABLE IF NOT EXISTS sense_xref (\
    \    xref_text TEXT NOT NULL,\
    \    xref_sense_no INTEGER,\
    \    entry_seq INTEGER NOT NULL,\
    \    sense_no INTEGER NOT NULL,\
    \    FOREIGN KEY(entry_seq, sense_no) REFERENCES sense(entry_seq, sense_no)\
    \);"

createTableSenseAntonym :: Query
createTableSenseAntonym = Query $ T.pack "\
    \CREATE TABLE IF NOT EXISTS sense_antonym (\
    \    antonym_text TEXT NOT NULL,\
    \    entry_seq INTEGER NOT NULL,\
    \    sense_no INTEGER NOT NULL,\
    \    FOREIGN KEY(entry_seq, sense_no) REFERENCES sense(entry_seq, sense_no)\
    \);"

createTableSensePartOfSpeech :: Query
createTableSensePartOfSpeech = Query $ T.pack "\
    \CREATE TABLE IF NOT EXISTS sense_part_of_speech (\
    \    pos_text TEXT NOT NULL,\
    \    entry_seq INTEGER NOT NULL,\
    \    sense_no INTEGER NOT NULL,\
    \    FOREIGN KEY(entry_seq, sense_no) REFERENCES sense(entry_seq, sense_no)\
    \);"

createTableSenseField :: Query
createTableSenseField = Query $ T.pack "\
    \CREATE TABLE IF NOT EXISTS sense_field (\
    \    field_text TEXT NOT NULL,\
    \    entry_seq INTEGER NOT NULL,\
    \    sense_no INTEGER NOT NULL,\
    \    FOREIGN KEY(entry_seq, sense_no) REFERENCES sense(entry_seq, sense_no)\
    \);"

createTableSenseMisc :: Query
createTableSenseMisc = Query $ T.pack "\
    \CREATE TABLE IF NOT EXISTS sense_misc (\
    \    misc_text TEXT NOT NULL,\
    \    entry_seq INTEGER NOT NULL,\
    \    sense_no INTEGER NOT NULL,\
    \    FOREIGN KEY(entry_seq, sense_no) REFERENCES sense(entry_seq, sense_no)\
    \);"

createTableLanguageSource :: Query
createTableLanguageSource = Query $ T.pack "\
    \CREATE TABLE IF NOT EXISTS sense_language_source (\
    \    lang_source TEXT NOT NULL,\
    \    lang_type TEXT,\
    \    lang_wasei INTEGER NOT NULL,\
    \    lang_text TEXT,\
    \    entry_seq INTEGER NOT NULL,\
    \    sense_no INTEGER NOT NULL,\
    \    FOREIGN KEY(entry_seq, sense_no) REFERENCES sense(entry_seq, sense_no)\
    \);"

createTableSenseDialectInfo :: Query
createTableSenseDialectInfo = Query $ T.pack "\ 
    \CREATE TABLE IF NOT EXISTS sense_dialect_info (\
    \    dialect_text TEXT NOT NULL,\
    \    entry_seq INTEGER NOT NULL,\
    \    sense_no INTEGER NOT NULL,\
    \    FOREIGN KEY(entry_seq, sense_no) REFERENCES sense(entry_seq, sense_no)\
    \);"

createTableSenseGloss :: Query
createTableSenseGloss = Query $ T.pack "\
    \CREATE TABLE IF NOT EXISTS sense_gloss (\
    \    gloss_lang TEXT,\
    \    gloss_gender TEXT,\
    \    gloss_type TEXT,\
    \    gloss_text TEXT NOT NULL,\
    \    entry_seq INTEGER NOT NULL,\
    \    sense_no INTEGER NOT NULL,\
    \    FOREIGN KEY(entry_seq, sense_no) REFERENCES sense(entry_seq, sense_no)\
    \);"

createTableSenseInfo :: Query
createTableSenseInfo = Query $ T.pack "\
    \CREATE TABLE IF NOT EXISTS sense_info (\
    \    info_text TEXT NOT NULL,\
    \    entry_seq INTEGER NOT NULL,\
    \    sense_no INTEGER NOT NULL,\
    \    FOREIGN KEY(entry_seq, sense_no) REFERENCES sense(entry_seq, sense_no)\
    \);"

createTableSenseExample :: Query
createTableSenseExample = Query $ T.pack "\
    \CREATE TABLE IF NOT EXISTS sense_example (\
    \    japanese TEXT NOT NULL,\
    \    english TEXT NOT NULL,\
    \    source_type TEXT NOT NULL,\
    \    source_ref TEXT NOT NULL,\
    \    source_text TEXT NOT NULL,\
    \    entry_seq INTEGER NOT NULL,\
    \    sense_no INTEGER NOT NULL,\
    \    FOREIGN KEY(entry_seq, sense_no) REFERENCES sense(entry_seq, sense_no)\
    \);"

-- Insert commands
-- Kanji
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

-- Reading
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

-- Sense
insertSense :: Query
insertSense = Query "\
    \INSERT INTO sense (entry_seq, sense_no) \
    \VALUES (?, ?);"

insertSenseRestriction :: Query
insertSenseRestriction = Query "\
    \INSERT INTO sense_restriction (restriction_text, entry_seq, sense_no) \
    \VALUES (?, ?, ?);"

insertSenseXref :: Query
insertSenseXref = Query "\
    \INSERT INTO sense_xref (xref_text, xref_sense_no, entry_seq, sense_no) \
    \VALUES (?, ?, ?, ?);"

insertSenseAntonym :: Query
insertSenseAntonym = Query "\
    \INSERT INTO sense_antonym (antonym_text, entry_seq, sense_no) \
    \VALUES (?, ?, ?);"

insertSensePartOfSpeech :: Query
insertSensePartOfSpeech = Query "\
    \INSERT INTO sense_part_of_speech (pos_text, entry_seq, sense_no) \
    \VALUES (?, ?, ?);"

insertSenseField :: Query
insertSenseField = Query "\
    \INSERT INTO sense_field (field_text, entry_seq, sense_no) \
    \VALUES (?, ?, ?);"

insertSenseMisc :: Query
insertSenseMisc = Query "\
    \INSERT INTO sense_misc (misc_text, entry_seq, sense_no) \
    \VALUES (?, ?, ?);"

insertSenseLanguageSource :: Query
insertSenseLanguageSource = Query "\
    \INSERT INTO sense_language_source (lang_source, lang_type, lang_wasei, lang_text, entry_seq, sense_no) \
    \VALUES (?, ?, ?, ?, ?, ?);"

insertSenseDialectInfo :: Query
insertSenseDialectInfo = Query "\
    \INSERT INTO sense_dialect_info (dialect_text, entry_seq, sense_no) \
    \VALUES (?, ?, ?);"

insertSenseGloss :: Query
insertSenseGloss = Query "\
    \INSERT INTO sense_gloss (gloss_lang, gloss_gender, gloss_type, gloss_text, entry_seq, sense_no) \
    \VALUES (?, ?, ?, ?, ?, ?);"

insertSenseInfo :: Query
insertSenseInfo = Query "\
    \INSERT INTO sense_info (info_text, entry_seq, sense_no) \
    \VALUES (?, ?, ?);"

insertSenseExample :: Query
insertSenseExample = Query "\
    \INSERT INTO sense_example (japanese, english, source_type, source_ref, source_text, entry_seq, sense_no) \
    \VALUES (?, ?, ?, ?, ?, ?, ?);"

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
    -- Kanji
    execute_ db createTableKanjiElement
    execute_ db createTableKanjiPriority
    execute_ db createTableKanjiInfo
    -- Reading
    execute_ db createTableReadingElement
    execute_ db createTableReadingRestriction
    execute_ db createTableReadingInfo
    execute_ db createTableReadingPriority
    -- Sense
    execute_ db createTableSense
    execute_ db createTableSenseRestriction
    execute_ db createTableXRef
    execute_ db createTableSenseAntonym
    execute_ db createTableSensePartOfSpeech
    execute_ db createTableSenseField
    execute_ db createTableSenseMisc
    execute_ db createTableLanguageSource
    execute_ db createTableSenseDialectInfo
    execute_ db createTableSenseGloss
    execute_ db createTableSenseInfo
    execute_ db createTableSenseExample

    -- Insert entries and their related data
    mapM_ (insertEntryData db) entries

    close db

insertEntryData :: Connection -> P.JMdictEntry -> IO ()
insertEntryData db entry = do
    let entryId = P.entrySeq entry
    -- Kanji
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
    -- Readings
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
    -- Sense
    mapM_ 
        (\(sense, sense_idx) -> do
            execute db insertSense (entryId, sense_idx)
            mapM_ 
                (\restriction -> execute db insertSenseRestriction (restriction, entryId, sense_idx)) 
                (P.senseRestrictions sense)
            mapM_ 
                (\xref -> execute db insertSenseXref 
                    (P.crefText xref, 
                    P.crefSenseNo xref, 
                    entryId, sense_idx))
                (P.crossReferences sense)
            mapM_ 
                (\antonym -> execute db insertSenseAntonym (antonym, entryId, sense_idx))  
                (P.antonyms sense)
            mapM_ 
                (\pos -> execute db insertSensePartOfSpeech (pos, entryId, sense_idx))
                (P.partOfSpeech sense)
            mapM_ 
                (\field' -> execute db insertSenseField (field', entryId, sense_idx))
                (P.fields sense)
            mapM_ 
                (\misc -> execute db insertSenseMisc (misc, entryId, sense_idx))
                (P.miscInfo sense)
            mapM_ 
                (\langSrc -> execute db insertSenseLanguageSource 
                    (P.lsLang langSrc, 
                     P.lsType langSrc, 
                     P.lsWasei langSrc, 
                     P.lsText langSrc,
                     entryId, sense_idx))
                (P.languageSources sense)
            mapM_ 
                (\dialect -> execute db insertSenseDialectInfo (dialect, entryId, sense_idx))  
                (P.dialectInfo sense)
            mapM_ 
                (\gloss -> execute db insertSenseGloss 
                    (P.glossLang gloss, 
                     P.glossGender gloss, 
                     P.glossType gloss, 
                     P.glossText gloss,
                     entryId, sense_idx))
                (P.glosses sense)
            mapM_ 
                (\info -> execute db insertSenseInfo (info, entryId, sense_idx))  
                (P.senseInfo sense)
            mapM_ 
                (\ex -> execute db insertSenseExample 
                    (P.exampleJapanese ex, 
                     P.exampleEnglish ex, 
                     P.exampleSourceType ex, 
                     P.exampleSource ex, 
                     P.exampleText ex,
                     entryId, sense_idx))
                (P.example sense)
        ) 
        (Prelude.zip (P.senses entry) ([1..]::[Int]))