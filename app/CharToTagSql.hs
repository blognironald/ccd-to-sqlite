module CharToTagSql where

import qualified CharToTagParser as P
import Database.SQLite.Simple
import Data.Text as T

-- Sql commands
createTableTags :: Query
createTableTags = 
    Query (T.pack "\
        \CREATE TABLE IF NOT EXISTS tags (            \
        \  character    TEXT NOT NULL                 \
        \, tag          TEXT NOT NULL                 \
        \, UNIQUE (character, tag) ON CONFLICT IGNORE \
        \);")

insertTags :: Query
insertTags = 
    Query (T.pack "\
        \INSERT INTO tags (   \
        \  character          \
        \, tag                \
        \) VALUES (?, ?)        \
        \")

tagger :: String -> Text -> [String]   
tagger tag char_ = [T.unpack char_, tag] 

runCharToTagSql :: String -> [P.CharToTag] -> String -> IO ()
runCharToTagSql dbName charToTagList tag = do
    db <- open dbName
    -- character table
    execute_ db createTableTags
    executeMany db insertTags (fmap (tagger tag) charToTagList)
    -- component table
    close db