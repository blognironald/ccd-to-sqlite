{-# LANGUAGE RecordWildCards #-}

module CcdParser where

import Prelude 
import Data.Text as T
import Data.Text.IO as T
import Text.Megaparsec.Char
import Text.Megaparsec
import Data.Void ( Void )
import Data.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Maybe (isNothing)

type Parser = Parsec Void T.Text

data CompositionType = 
    GraphicalPrimitive              |
    HorizontalComposition           |
    VerticalComposition             |
    Inclusion                       |
    VerticalTopRepetition           |
    HorizontalCompositionOfThree    |
    RepetitionOfThree               |
    RepetitionOfFour                |
    VerticalSepByCrown              |
    GraphicalSuperposition          |
    UnknownComposition
        deriving (Show, Eq)

getCompositionText :: CompositionType -> Maybe T.Text
getCompositionText a = case a of
    GraphicalPrimitive              -> wrap "一"
    HorizontalComposition           -> wrap "吅"
    VerticalComposition             -> wrap "吕"
    Inclusion                       -> wrap "回"
    VerticalTopRepetition           -> wrap "咒"
    HorizontalCompositionOfThree    -> wrap "弼"
    RepetitionOfThree               -> wrap "品"
    RepetitionOfFour                -> wrap "叕"
    VerticalSepByCrown              -> wrap "冖"
    GraphicalSuperposition          -> wrap "+"
    _                               -> Nothing
    where
        wrap = Just . T.pack

getCompositionType :: T.Text -> CompositionType
getCompositionType a = case T.unpack a of
    "一" -> GraphicalPrimitive
    "吅" -> HorizontalComposition
    "吕" -> VerticalComposition
    "回" -> Inclusion
    "咒" -> VerticalTopRepetition
    "弼" -> HorizontalCompositionOfThree
    "品" -> RepetitionOfThree
    "叕" -> RepetitionOfFour
    "冖" -> VerticalSepByCrown
    "+"  -> GraphicalSuperposition
    _    -> UnknownComposition

data Character = Character {
    character           :: Text,
    strokeCount         :: Maybe Int,
    compositionType     :: CompositionType,
    firstComponent      :: Maybe Text,
    isFirstVerified     :: Bool,            
    secondComponent     :: Maybe Text,
    isSecondVerified    :: Bool,            
    cangie5             :: Maybe Text,
    radical             :: Maybe Text
} deriving (Show, Eq)

pComponents :: Parser Text
pComponents = takeWhileP Nothing f
    where
        f a
            | isLetter a = True
            | isSymbol a = True
            | isPunctuation a = True
            | otherwise  = False

pCangie :: Parser Text
pCangie = try (singleton <$> char '1') <|> takeWhileP Nothing isAsciiUpper

pStrokeCount :: Parser Int
pStrokeCount = L.decimal

pStatus :: Parser Text
pStatus = do
    firstPartStatus  <- optional . try $ char '?' <|> char '*'
    slash            <- char '/'
    secondPartStatus <- optional . try $ char '?' <|> char '*'
    let out = Prelude.foldr formStatus "" [firstPartStatus, Just slash, secondPartStatus]
    return $ T.pack out

pVerified :: Parser Bool
pVerified = do
    status <- optional . try $ char '?' <|> char '*'
    return $ isNothing status

formStatus :: Maybe Char -> String -> String
formStatus Nothing a = a
formStatus (Just a) b = a:b

pCharacter :: Parser Character
pCharacter = do
    character <- singleton <$> letterChar <* spaceChar
    strokeCount <- optional pStrokeCount <* spaceChar
    compositionType <- getCompositionType . singleton <$> 
        (   try (char '+') <|> 
            try (char '*') <|> 
            letterChar ) <* spaceChar
    firstComponent <- optional pComponents <* spaceChar <* pStrokeCount <* spaceChar
    secondComponent <- optional pComponents <* spaceChar <* pStrokeCount <* spaceChar
    cangie5 <- optional pCangie <* spaceChar
    isFirstVerified <- pVerified <* char '/'
    isSecondVerified <- pVerified <* spaceChar
    radical <- optional (try (singleton <$> letterChar) <|> singleton <$> char '*') <* eol
    return Character {..}

parseAsCharacter  :: Parser [Character]
parseAsCharacter  = do
    result <- many pCharacter
    _ <- eof
    return result

runCcdParser :: String -> IO (Either String [Character])
runCcdParser input = do
    result <- runParser parseAsCharacter input <$> T.readFile input
    case result of
        Left  err  -> return $ Left (errorBundlePretty err)
        Right rows -> return $ Right rows