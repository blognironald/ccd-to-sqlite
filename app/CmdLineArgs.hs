module CmdLineArgs where

import Options.Applicative

data Args = Args
  { operation   :: String
  , input       :: String
  , output      :: String 
  , tag         :: Maybe String
  }

-- Parser for arguments
argsParser :: Parser Args
argsParser = Args
      <$> strOption
          ( long "operation"
         <> long "op"
         <> metavar "OPERATION"
         <> help "Type of parse and sql-ize operation" )
      <*> strOption
          ( long "input"
         <> short 'i'
         <> metavar "TEXT"
         <> help "Path to input file" )
      <*> strOption
          ( long "output"
         <> short 'o'
         <> metavar "DB"
         <> showDefault
         <> value "out.db3"
         <> help "Path to output sqlite db" )
      <*> optional (strOption
          ( long "tag"
         <> short 't'
         <> metavar "TAG"
         <> help "Tag name" ))

-- Program info
opts :: ParserInfo Args
opts = info (argsParser <**> helper)
    ( fullDesc
  <> progDesc "Convert File to Sql Table"
  <> header "ccd-to-sqlite - a simple file to sqlite db processor" )