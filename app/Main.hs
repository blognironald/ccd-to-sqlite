module Main where

import CmdLineArgs
import Options.Applicative

-- import System.Environment (withArgs)
-- withArgs ["--op", "ccd", "-iccd.txt", "-occd.db3"] main

main :: IO ()
main = do
    args <- execParser opts
    putStrLn $ "First argument: " ++ operation args
    putStrLn $ "Second argument: " ++ input args
    putStrLn $ "Third argument: " ++ output args