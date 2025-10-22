module GenerateN5Characters where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.SQLite.Simple
 
import ComponentsFromCharacter as C
-- import Data.Text.IO (putStrLn)

queryN5 :: Query
queryN5 = Query (T.pack "\
    \SELECT character \
    \from TAGS \
    \WHERE tag = 'n5'\
    \;")

queryL4 :: Query
queryL4 = Query (T.pack "\
    \SELECT character \
    \from kd_character \
    \WHERE jlpt=4 \
    \ORDER by freq ASC \
    \;")

getN5Components :: String -> IO ()
getN5Components dbName = do
    db <- open dbName
    n5Chars <- query_ db queryN5 :: IO [Only T.Text]
    n5Components <- C.multiple dbName (map fromOnly n5Chars)
    mapM_ T.putStr n5Components >> Prelude.putStrLn ""

getAndConfirmL4 :: String -> IO ()
getAndConfirmL4 dbName = do
    db <- open dbName
    n5Chars <- query_ db queryL4 :: IO [Only T.Text]
    n5Components <- mapM (C.confirmCharacterAndParts db . fromOnly) n5Chars
    let list = removeDups $ concatMap postOrderTraversal n5Components
    mapM_ T.putStr list >> Prelude.putStrLn ""