module Monkey.Options (
  Options (..),
  parser,
  parseOptions,
  execParser,
) where

import Data.Version (showVersion)
import Options.Applicative
import Paths_monkey (version)

data Options = Options
  { unicode :: Bool
  , color :: Bool
  , sourceFile :: Maybe FilePath
  , printVersion :: Bool
  }

parser :: ParserInfo Options
parser =
  info (parseOptions <**> helper) $
    mconcat
      [ fullDesc
      , header ("monkey " <> showVersion version)
      , footer "https://github.com/pnotequalnp/monkey"
      ]

parseOptions :: Parser Options
parseOptions =
  Options
    <$> parseUnicode
    <*> parseColor
    <*> parseSource
    <*> parseVersion

parseSource :: Parser (Maybe FilePath)
parseSource = optional (strArgument (metavar "SOURCE_FILE"))

parseUnicode :: Parser Bool
parseUnicode = not <$> switch (long "ascii" <> help "Use ASCII for errors" <> hidden)

parseColor :: Parser Bool
parseColor = not <$> switch (long "no-color" <> help "Don't show colors in errors" <> hidden)

parseVersion :: Parser Bool
parseVersion = switch (long "version" <> help "Print version information and exit" <> hidden)
