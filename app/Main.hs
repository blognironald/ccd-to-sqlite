module Main where

import Options.Applicative
import Data.Char
import System.Exit(die)
-- import qualified Data.Text.IO as T
import Data.Maybe
import Database.SQLite.Simple

import CmdLineArgs
import CcdParser
import CcdSql
import CharToTagParser
import CharToTagSql
import KanjiDic2Parser
import KanjiDic2Sql

-- import System.Environment (withArgs)
-- withArgs ["--op", "ccd",      "-iothers/ccd.txt",       "-oothers/ccd.db3"              ] main
-- withArgs ["--op", "tag",      "-iothers/n5.txt",        "-oothers/ccd.db3",     "-tn5"  ] main
-- withArgs ["--op", "tag",      "-iothers/hsk1.txt",      "-oothers/ccd.db3",     "-thsk1"] main
-- withArgs ["--op", "kanjidic", "-iothers/kanjidic2.xml", "-oothers/kanjidic2.db3"        ] main

main :: IO ()
main = do
    args <- execParser opts
    case map toLower (operation args) of
        "ccd" -> do
            result <- runCcdParser $ input args
            case result of
                Left err -> die err
                Right rows -> do
                    runCcdSql (output args) rows
        "tag" -> do
            result <- runCharToTagParser $ input args
            case result of
                Left err -> die err
                Right chars -> do
                    -- mapM_ T.putStr chars
                    runCharToTagSql (output args) chars (fromMaybe "!" (tag args))
        "kanjidic" -> do
            characters <- kdCharacters <$> parseKanjiDic2 (input args)
            db <- initializeConnection $ output args
            mapM_ (insertCompleteCharacter db) characters
        _     -> putStrLn $ "\nUnidentified operation: " ++ operation args ++ "\n"
    putStrLn "\nDone!"
