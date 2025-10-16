module Main where

import Options.Applicative
import Data.Char
import System.Exit(die)
-- import qualified Data.Text.IO as T
import Data.Maybe
import System.Environment (withArgs)
import Data.Time.Clock
import Text.Printf

import CmdLineArgs
import CcdParser
import CcdSql
import CharToTagParser
import CharToTagSql
import KanjiDic2Parser
import KanjiDic2Sql as KD
import UnihanReadingsParser
import UnihanReadingsSql
import CEdictParser
import CEdictSql
import JMdictParser
import JMdictSql

-- import System.Environment (withArgs)
-- withArgs ["--op", "ccd",      "-iothers/ccd.txt",             "-oothers/ccd.db3"              ] main
-- withArgs ["--op", "tag",      "-iothers/n5.txt",              "-oothers/ccd.db3",     "-tn5"  ] main
-- withArgs ["--op", "tag",      "-iothers/hsk1.txt",            "-oothers/ccd.db3",     "-thsk1"] main
-- withArgs ["--op", "kanjidic", "-iothers/kanjidic2.xml",       "-oothers/kanjidic2.db3"        ] main
-- withArgs ["--op", "unihan",   "-iothers/Unihan_Readings.txt", "-oothers/unihan.db3"           ] main
-- withArgs ["--op", "jmdict",   "-iothers/JMdict_e_examp.xml"           ] main

main :: IO ()
main = do
    start <- getCurrentTime
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
            db <- KD.initializeConnection $ output args
            mapM_ (insertCompleteCharacter db) characters
        "unihan" -> do
            result <- runUnihanParser $ input args
            case result of
                Left err -> die err
                Right entries -> do
                    runUnihanSql (output args) entries
        "cedict" -> do
            result <- runCedictParser $ input args
            case result of
                Left err -> die err
                Right entries -> do
                    runCedictSql (output args) entries
        "jmdict" -> do
            result <- runJMdictParser $ input args
            case result of
                Left err -> die err
                Right entries -> do
                    runJMdictSql (output args) entries
        "all" -> do
            -- withArgs["--op","all","-irandom", "-oothers/master.db3"] main
            let outfile = "-o" ++ output args
            withArgs ["--op", "ccd",      "-iothers/ccd.txt"             , outfile ] main
            withArgs ["--op", "kanjidic", "-iothers/kanjidic2.xml"       , outfile ] main
            withArgs ["--op", "cedict",   "-iothers/cedict_ts.u8"        , outfile ] main
            withArgs ["--op", "unihan",   "-iothers/Unihan_Readings.txt" , outfile ] main
            withArgs ["--op", "jmdict",   "-iothers/JMdict_e_examp.xml"  , outfile ] main
        _     -> putStrLn $ "\nUnidentified operation: " ++ operation args ++ "\n"
    end <- getCurrentTime
    let elapsed = diffUTCTime end start
    printf "Elapsed: %.2f minutes\n" ((realToFrac elapsed/60) :: Double)
    putStrLn "Done!"
