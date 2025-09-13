module CharToTagParser where

import Data.Text as T
import Data.Text.IO as T
import Data.Void ( Void )
import Text.Megaparsec.Char
import Text.Megaparsec

type CharToTag = T.Text

type Parser = Parsec Void T.Text

pCharToTag :: Parser CharToTag
pCharToTag = singleton <$> (letterChar <* space)

parseAsCharToTag  :: Parser [CharToTag]
parseAsCharToTag  = do
    result <- many pCharToTag
    _ <- eof
    return result

runCharToTagParser :: String -> IO (Either String [CharToTag])
runCharToTagParser input = do
    result <- runParser parseAsCharToTag input <$> T.readFile input
    case result of
        Left  err  -> return $ Left (errorBundlePretty err)
        Right rows -> return $ Right rows