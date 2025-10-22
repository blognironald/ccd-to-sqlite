module ComponentsFromCharacter where

import qualified Data.Text as T
import Data.Text.IO as T
import Database.SQLite.Simple
import Data.Tree
import Data.Tree.Pretty

import qualified Data.Set as Set
-- import GHC.IO.Handle
-- import GHC.IO.Handle.FD
import Control.Exception
import Data.Char

data CharTreeElem = CharTreeElem {
      character  :: T.Text
    , leftChars  :: [CharTreeElem]
    , rightChars :: [CharTreeElem]
}

postOrderTraversal :: CharTreeElem -> [T.Text]
postOrderTraversal (CharTreeElem node lefts rights) =
  concatMap postOrderTraversal lefts  ++
  concatMap postOrderTraversal rights ++
  [node]

getQuery :: Query
getQuery = Query (T.pack "               \
  \SELECT component                    \
  \FROM ccd_component                  \
  \WHERE character = ? and isFirst = ? \
  \ORDER BY IndexNo asc                \
  \;")

getQueryVerified :: Query
getQueryVerified = Query (T.pack "       \
  \SELECT character, isRonVerified     \
  \FROM ccd_character                  \
  \WHERE character = ? \
  \LIMIT 1               \
  \;")

updateVerification :: Query
updateVerification = Query (T.pack "\
  \UPDATE ccd_character \
  \SET isRonVerified = 1 \
  \where character = ? \
  \;")

deleteOldComponents :: Query 
deleteOldComponents = Query (T.pack "\
  \DELETE FROM ccd_component \
  \WHERE character = ? AND isFirst = ? \
  \;")

insertComponent :: Query
insertComponent = 
    Query(T.pack "\
        \INSERT INTO ccd_component (   \
        \  component               \
        \, character               \
        \, isFirst                 \
        \, indexNo                 \ 
        \, isVerified)             \
        \VALUES (?, ?, ?, ?, ?)    \
        \")

-- buildTree :: Connection -> T.Text -> IO CharTreeElem
buildTree :: Connection -> T.Text -> IO CharTreeElem
buildTree conn inputChar = do
  leftQuery <- query conn getQuery [inputChar, T.pack "1"] :: IO [Only T.Text]
  leftTree <- mapM (buildTree conn) (fmap fromOnly leftQuery)
  rightQuery <- query conn getQuery [inputChar, T.pack "0"] :: IO [Only T.Text]
  rightTree <- mapM (buildTree conn) (fmap fromOnly rightQuery)
  return $ CharTreeElem {
      character = inputChar
    , leftChars = leftTree
    , rightChars = rightTree
  }

buildAndConfirmTree :: Connection -> T.Text -> IO CharTreeElem
buildAndConfirmTree conn inputChar = do
  leftQuery <- query conn getQuery [inputChar, T.pack "1"] :: IO [Only T.Text]
  rightQuery <- query conn getQuery [inputChar, T.pack "0"] :: IO [Only T.Text]
  (left, right) <- checkComponents conn inputChar (fmap fromOnly leftQuery) (fmap fromOnly rightQuery)
  leftTree <- mapM (confirmCharacterAndParts conn) left
  rightTree <- mapM (confirmCharacterAndParts conn) right
  return $ CharTreeElem {
      character = inputChar
    , leftChars = leftTree
    , rightChars = rightTree
  }
  -- return $ CharTreeElem inputChar [] []

checkComponents :: Connection -> T.Text -> [T.Text] -> [T.Text] -> IO ([T.Text], [T.Text])
checkComponents conn inputChar' left right = do
  T.putStrLn $ 
    T.pack "\n"
    <> inputChar' 
    <> T.pack " -> "
    <> T.intercalate (T.pack "") left
    <> if null right then 
        T.pack "" 
      else 
        T.pack " + "
        <> T.intercalate (T.pack "") right
  T.putStr $ T.pack "[ ] ok; replace [l], [r], [b]oth: "

  -- hSetBuffering stdin NoBuffering
  -- hSetEcho stdin False
  c1 <- T.getLine
  -- hSetBuffering stdin LineBuffering
  -- hSetEcho stdin True
  -- Prelude.putStrLn $ "<" ++ [c] ++ ">"
  let c = if T.length c1 < 1 then ' ' else T.head c1
  if isSpace c then
    do
      execute conn updateVerification (Only inputChar')
      return (left, right)
  else
    do
      case c of
        'l' -> do
          result <- replaceLcomponents conn inputChar' left right
          execute conn updateVerification (Only inputChar')
          return result
        'r' -> do
          result <- replaceRcomponents conn inputChar' left right
          execute conn updateVerification (Only inputChar')
          return result
        'b' -> do
          (l, _) <- replaceLcomponents conn inputChar' left right
          (_,r) <- replaceRcomponents conn inputChar' left right
          execute conn updateVerification (Only inputChar')
          return (l,r)
        _   -> checkComponents conn inputChar' left right

replaceLcomponents :: Connection -> T.Text -> [T.Text] -> [T.Text] -> IO ([T.Text], [T.Text])
replaceLcomponents conn inputChar' left right = do
  T.putStr $ T.pack "Replace " <> T.intercalate (T.pack "") left <> T.pack "> "
  input <- T.getLine
  -- T.putStrLn $ T.pack "<" <> input <> T.pack ">"
  execute conn deleteOldComponents [inputChar', T.pack "1"]
  mapM_
    (\(c, i) -> do
      execute conn insertComponent [c, inputChar', T.pack "1", (T.pack . show) i, T.pack "1"]
      )
    (Prelude.zip (T.chunksOf 1 input) ([1..]::[Integer]))
  return (T.chunksOf 1 input, right)

replaceRcomponents :: Connection -> T.Text -> [T.Text] -> [T.Text] -> IO ([T.Text], [T.Text])
replaceRcomponents conn inputChar' left right = do
  T.putStr $ T.pack "Replace " <> T.intercalate (T.pack "") right <> T.pack "> "
  input <- T.getLine
  -- T.putStrLn $ T.pack "<" <> input <> T.pack ">"
  execute conn deleteOldComponents [inputChar', T.pack "0"]
  mapM_
    (\(c, i) -> do
      execute conn insertComponent [c, inputChar', T.pack "0", (T.pack . show) i, T.pack "1"]
      )
    (Prelude.zip (T.chunksOf 1 input) ([1..]::[Integer]))
  return (left, T.chunksOf 1 input)

queryCharacter :: DbName -> T.Text -> IO [T.Text]
queryCharacter dbName char_ = do
    db <- open dbName
    results <- query db getQuery [char_, T.pack "0"] :: IO [Only T.Text]
    return $ map fromOnly results

-- callBuildTree :: String -> T.Text -> IO CharTreeElem
-- callBuildTree dbName char_ = do
--   db <- open dbName
--   buildTree db char_

-- Convert CharTreeElem to Tree String for pretty printing
toTree :: CharTreeElem -> Tree String
toTree elem_ = Node (T.unpack $ character elem_)
    (map toTree (leftChars elem_) ++ map toTree (rightChars elem_))

-- Helper function to pretty print a CharTreeElem
prettyPrintTree :: CharTreeElem -> String
prettyPrintTree = drawVerticalTreeWith 10 . toTree

-- Example usage in callBuildTree
type DbName = String
callBuildTree :: DbName -> T.Text -> IO [T.Text]
callBuildTree dbName char_ = do
  db <- open dbName
  tree <- buildTree db char_
  return $ postOrderTraversal tree
  -- tree <- buildTree db char_
  -- putStrLn $ prettyPrintTree tree 
  -- mapM_ T.putStr (postOrderTraversal tree) >> putStrLn ""
  -- return tree

p :: String -> IO ()
p char_ = do
  db <- open "others//ccd.db3"
  tree <- buildTree db (T.pack char_)
  close db
  Prelude.putStrLn $ prettyPrintTree tree

b :: String -> IO ()
b char_ = do
  db <- open "others//ccd.db3"
  addColumnIsRonVerified db
  tree <- confirmCharacterAndParts db (T.pack char_)
  close db
  Prelude.putStrLn $ prettyPrintTree tree

multiple :: DbName -> [T.Text] -> IO [T.Text]
multiple dbName chars = do
  list <- mapM (callBuildTree dbName) chars
  let result = removeDups (concat list)
  -- mapM_ T.putStr result
  return result

removeDups :: [T.Text] -> [T.Text]
removeDups chars = go chars (Set.fromList []) []
  where
    go [] _ result = result
    go (c:cs) charsSet result
      | Set.member c charsSet = go cs charsSet                result
      | otherwise             = go cs (Set.insert c charsSet) (result ++ [c])

addColumnIsRonVerified :: Connection  -> IO ()
addColumnIsRonVerified conn = do
  let sql = T.pack "ALTER TABLE ccd_character ADD COLUMN isRonVerified INTEGER DEFAULT 0;"
  execute_ conn (Query sql) `catch` handler
  where
    handler :: SomeException -> IO ()
    handler _ = return ()
  
confirmCharacterAndParts :: Connection -> T.Text -> IO CharTreeElem
confirmCharacterAndParts conn character' = do
  addColumnIsRonVerified conn
  results <- query conn getQueryVerified (Only character') :: IO [(T.Text, Bool)]
  case results of
    [(c, True)] -> buildTree conn c
    [(c, False)] -> buildAndConfirmTree conn c
    _  -> return $ CharTreeElem character' [] []