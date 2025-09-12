module Main where

import CmdLineArgs
import Options.Applicative
import CcdParser
import CcdSql
import Data.Char
import System.Exit(die)

-- import System.Environment (withArgs)
-- withArgs ["--op", "ccd", "-iothers/ccd.txt", "-oothers/ccd.db3"] main

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
        _     -> putStrLn $ "\nUnidentified operation: " ++ operation args ++ "\n"
    putStrLn "Done!"
