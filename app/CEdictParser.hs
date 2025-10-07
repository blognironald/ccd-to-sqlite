{-# LANGUAGE RecordWildCards #-}

module CEdictParser where

import Prelude
import Data.Text as T
import Data.Text.IO as T
import Text.Megaparsec.Char
import Text.Megaparsec
import Data.Void (Void)
import Data.Char

type Parser = Parsec Void Text

data CedictEntry = CedictEntry {
    ceTraditional    :: Text,      -- Traditional Chinese characters
    ceSimplified     :: Text,      -- Simplified Chinese characters
    cePronunciation  :: Text,      -- Pinyin pronunciation
    ceMeanings       :: [Text]     -- English meanings (split by /)
} deriving (Show, Eq)

pTraditional :: Parser Text
pTraditional = takeWhileP Nothing (not . isSpace)

pSimplified :: Parser Text
pSimplified = takeWhileP Nothing (not . isSpace)

pPronunciation :: Parser Text
pPronunciation = do
    _ <- char '['
    pronun <- takeWhileP Nothing (/= ']')
    _ <- char ']'
    return pronun

pMeaning :: Parser Text
pMeaning = do
    meaning <- takeWhileP Nothing (/= '/')
    _ <- char '/'
    return $ T.strip meaning

pMeanings :: Parser [Text]
pMeanings = do
    _ <- char '/'
    meanings <- manyTill pMeaning (lookAhead eol)
    return $ Prelude.filter (not . T.null) meanings

pCedictEntry :: Parser CedictEntry
pCedictEntry = do
    ceTraditional <- pTraditional <* spaceChar
    ceSimplified <- pSimplified <* spaceChar
    cePronunciation <- pPronunciation <* spaceChar
    ceMeanings <- pMeanings <* eol
    return CedictEntry {..}

pCedictFile :: Parser [CedictEntry]
pCedictFile = do
    skipMany pComment
    result <- many pCedictEntry
    _ <- eof
    return result
    where
        pComment = do
            _ <- char '#'
            _ <- manyTill anySingle eol
            return ()

runCedictParser :: FilePath -> IO (Either String [CedictEntry])
runCedictParser input = do
    result <- runParser pCedictFile input <$> T.readFile input
    case result of
        Left err -> return $ Left (errorBundlePretty err)
        Right rows -> return $ Right rows