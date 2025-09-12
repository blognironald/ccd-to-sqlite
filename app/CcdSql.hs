module CcdSql where

import qualified CcdParser as P
import Database.SQLite.Simple
import Data.Text as T
import Data.Maybe


-- Sql commands
createTableCharacter :: Query
createTableCharacter = 
    Query (T.pack "\
        \CREATE TABLE IF NOT EXISTS character (\
        \  character       TEXT PRIMARY KEY    \
        \, strokeCount     INTEGER             \
        \, compositionType TEXT                \
        \, cangie5         TEXT                \
        \, radical         TEXT                \
        \);")

createTableComponent :: Query
createTableComponent =
    Query (T.pack "\
        \CREATE TABLE IF NOT EXISTS component (\
        \  component  TEXT NOT NULL             \
        \, character  TEXT NOT NULL             \
        \, isFirst    INTEGER NOT NULL          \
        \, indexNo    INTEGER NOT NULL          \
        \, isVerified INTEGER NOT NULL          \
        \);")

insertCharacter :: Query
insertCharacter = 
    Query (T.pack "\
        \INSERT INTO character (   \
        \  character               \
        \, strokeCount             \
        \, compositionType         \
        \, cangie5                 \ 
        \, radical)                \
        \VALUES (?, ?, ?, ?, ?)    \
        \")

insertComponent :: Query
insertComponent = 
    Query(T.pack "\
        \INSERT INTO component (   \
        \  component               \
        \, character               \
        \, isFirst                 \
        \, indexNo                 \ 
        \, isVerified)             \
        \VALUES (?, ?, ?, ?, ?)    \
        \")

-- Haskell 'Character' structure to SQL datatype
newtype Parent = Parent P.Character
instance ToRow Parent where
    toRow (Parent ccd) = toRow (
        P.character ccd,
        P.strokeCount ccd,
        (P.getCompositionText . P.compositionType) ccd,
        P.cangie5 ccd,
        P.radical ccd)

-- Haskell 'Component' structure to SQL datatype
data Component = Component {
    component   :: Text,
    character   :: Text,
    isFirst     :: Bool,
    indexNo     :: Int,
    isVerified  :: Bool
} deriving (Show, Eq)

instance ToRow Component where
    toRow c = toRow (
        component c,
        character c,
        isFirst c,
        indexNo c,
        isVerified c)

-- Get component-specific fields from the P.Character
getComponents :: P.Character -> [Component]
getComponents ccd = getFirstComponent ccd ++ getSecondComponent ccd

getFirstComponent :: P.Character -> [Component]
getFirstComponent ccd
    | P.compositionType ccd == P.GraphicalPrimitive = []
    | otherwise = textToList
                    (P.character ccd)
                    (fromMaybe (T.pack "") (P.firstComponent ccd))
                    True
                    (P.isFirstVerified ccd)

getSecondComponent :: P.Character -> [Component]
getSecondComponent ccd
    | P.compositionType ccd == P.GraphicalPrimitive = [] -- No second component
    | P.compositionType ccd == P.RepetitionOfThree  = [] -- Repetition of 1st component
    | P.compositionType ccd == P.RepetitionOfFour   = [] -- Repetition of 1st component
    | otherwise = textToList
                    (P.character ccd)
                    (fromMaybe (T.pack "") (P.secondComponent ccd))
                    False
                    (P.isSecondVerified ccd)

-- Aliases just to clarify the arguments used by textToList
type Character = T.Text
type Components = T.Text
type IsFirst = Bool
type IsVerified = Bool
-- The idea is to break the component text list into separate items like this ↓
--            "儍"      -> "凵?八夊"   -> False   -> False      -> ["凵", "?", "八", "夊"]
textToList :: Character -> Components -> IsFirst -> IsVerified -> [Component]
textToList ch_ co_ if_ iv_ = go 0 (T.chunksOf 1 co_) []
    where
        go _   []     result = result
        go in_ (c:cs) result =
            go (in_ + 1) cs (result ++
                [Component {
                    component  = c,
                    character  = ch_,
                    isFirst    = if_,
                    indexNo    = in_,
                    isVerified = iv_
                }])

-- Run!
runCcdSql :: String -> [P.Character] -> IO ()
runCcdSql dbName characterList = do
    db <- open dbName
    -- character table
    execute_ db createTableCharacter
    executeMany db insertCharacter (fmap Parent characterList)
    -- component table
    execute_ db createTableComponent
    executeMany db insertComponent (Prelude.concatMap getComponents characterList)
    close db