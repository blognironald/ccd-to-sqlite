module ComponentsFromCharacter where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.SQLite.Simple
import Data.Tree
import Data.Tree.Pretty

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
    \FROM component                      \
    \WHERE character = ? and isFirst = ? \
    \ORDER BY IndexNo asc                \
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


queryCharacter :: String -> T.Text -> IO [T.Text]
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
callBuildTree :: String -> T.Text -> IO CharTreeElem
callBuildTree dbName char_ = do
  db <- open dbName
  tree <- buildTree db char_
  putStrLn $ prettyPrintTree tree  -- Optional: add this line to print the tree
  mapM_ T.putStr (postOrderTraversal tree) >> putStrLn ""
  
  return tree
