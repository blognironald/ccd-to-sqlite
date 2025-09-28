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
    \WHERE tag = 'n5';\
    \")

getN5Components :: String -> IO ()
getN5Components dbName = do
    db <- open dbName
    n5Chars <- query_ db queryN5 :: IO [Only T.Text]
    n5Components <- C.multiple dbName (map fromOnly n5Chars)
    mapM_ T.putStr n5Components >> Prelude.putStrLn ""