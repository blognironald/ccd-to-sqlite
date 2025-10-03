module UnihanReadingsParser where

import Prelude
import Data.Text as T
import Data.Text.IO as T
import Text.Megaparsec.Char
import Text.Megaparsec
import Data.Void (Void)
import Data.Char
import Data.Maybe (catMaybes)
import Numeric (readHex)
import System.Exit (die)
import Data.List as L ( find )
import Database.SQLite.Simple

type Parser = Parsec Void T.Text

data UnihanReading = UnihanReading {
    uniCharacter   :: Text,
    uniDefinition  :: Text
} deriving (Show, Eq)

-- Result type for queries
instance FromRow UnihanReading where
    fromRow = UnihanReading <$> field <*> field

pCodePoint :: Parser Text
pCodePoint = do
    _ <- string $ T.pack "U+"
    hex <- some hexDigitChar
    return $ T.pack $ "U+" ++ hex

pFieldName :: Parser Text
pFieldName = T.pack <$> some (alphaNumChar <|> char '_')

pFieldValue :: Parser Text
pFieldValue = T.pack <$> manyTill anySingle eol

pCommentLine :: Parser ()
pCommentLine = do
    _ <- char '#'
    _ <- manyTill anySingle eol
    return ()

pEmptyLine :: Parser ()
pEmptyLine = do
    _ <- many (char ' ' <|> char '\t')
    _ <- eol
    return ()

codePointToChar :: Text -> Maybe Text
codePointToChar codePoint =
    case T.stripPrefix (T.pack "U+") codePoint of
        Just hexStr ->
            case readHex (T.unpack hexStr) of
                [(value, "")] -> Just $ T.singleton (chr value)
                _ -> Nothing
        Nothing -> Nothing

pDataLine :: Parser (Maybe UnihanReading)
pDataLine = do
    codePoint <- pCodePoint
    _ <- char '\t'
    fieldName <- pFieldName
    _ <- char '\t'
    fieldValue <- pFieldValue

    if fieldName == T.pack "kDefinition"
        then case codePointToChar codePoint of
            Just character -> return $ Just UnihanReading {
                uniCharacter = character,
                uniDefinition = fieldValue
            }
            Nothing -> return Nothing
        else return Nothing

pUnihanLine :: Parser (Maybe UnihanReading)
pUnihanLine = choice
    [ Nothing <$ pCommentLine
    , Nothing <$ pEmptyLine
    , pDataLine
    ]

parseAsUnihanEntry :: Parser [UnihanReading]
parseAsUnihanEntry = do
    result <- many pUnihanLine
    _ <- eof
    return $ catMaybes result

runUnihanParser :: String -> IO (Either String [UnihanReading])
runUnihanParser input = do
    result <- runParser parseAsUnihanEntry input <$> T.readFile input
    case result of
        Left err -> return $ Left (errorBundlePretty err)
        Right rows -> return $ Right rows

runSample :: IO ()
runSample = do
    result <- runUnihanParser "others/Unihan_Readings.txt"
    case result of
        Left err -> die err
        Right entries -> do
            -- Prelude.putStrLn $ "Total entries parsed: " ++ show (Prelude.length entries)
            T.putStr $ prettyPrintEntries entries
            -- mapM_ (T.putStrLn . prettyPrintEntry) entries
            -- let (total, avgLen, maxLen) = getStatistics entries
            -- Prelude.putStrLn $ "Statistics - Total: " ++ show total ++ ", Avg Def Length: " ++ show avgLen ++ ", Max Def Length: " ++ show maxLen
            -- let defs = extractDefinitions entries
            -- mapM_ (\(c, d) -> T.putStrLn $ c <> T.pack ": " <> d) defs

charToCodePoint :: Char -> Text
charToCodePoint c = T.pack $ "U+" ++ toHexString (fromEnum c)
    where
        toHexString n = let hex = showHex n ""
                        in Prelude.map toUpper' $ Prelude.replicate (4 - Prelude.length hex) '0' ++ hex
        showHex n s
            | n < 16 = hexDigit n : s
            | otherwise = showHex (n `div` 16) (hexDigit (n `mod` 16) : s)
        hexDigit n
            | n < 10 = chr (48 + n)
            | n < 16 = chr (55 + n)
            | otherwise = error "Invalid hex digit"
        toUpper' c'
            | isAsciiLower c' = chr (fromEnum c' - 32)
            | otherwise = c'

findByCharacter :: Text -> [UnihanReading] -> Maybe UnihanReading
findByCharacter t = L.find (\e -> uniCharacter e == t)


findByDefinitionWord :: Text -> [UnihanReading] -> [UnihanReading]
findByDefinitionWord searchTerm = Prelude.filter containsWord
    where
        lowerSearchTerm = T.toLower searchTerm
        containsWord entry = T.isInfixOf lowerSearchTerm (T.toLower $ uniDefinition entry)

getAllCharacters :: [UnihanReading] -> Text
getAllCharacters  = T.concat . fmap uniCharacter

getStatistics :: [UnihanReading] -> (Int, Int, Int)
getStatistics entries = (totalEntries, avgDefLength, maxDefLength)
    where
        totalEntries = Prelude.length entries
        defLengths = Prelude.map (T.length . uniDefinition) entries
        avgDefLength = if totalEntries > 0
                       then Prelude.sum defLengths `div` totalEntries
                       else 0
        maxDefLength = if Prelude.null defLengths then 0 else Prelude.maximum defLengths

prettyPrintEntry :: UnihanReading -> Text
prettyPrintEntry entry = T.unlines
    [ T.pack "Character:   " <> uniCharacter entry
    , T.pack "Definition:  " <> uniDefinition entry
    , T.pack "─────────────────────────────────────────────────"
    ]

prettyPrintEntries :: [UnihanReading] -> Text
prettyPrintEntries = T.concat . Prelude.map prettyPrintEntry

extractDefinitions :: [UnihanReading] -> [(Text, Text)]
extractDefinitions = Prelude.map (\e -> (uniCharacter e, uniDefinition e))