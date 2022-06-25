module Main (main) where

import Control.Monad (when)
import Data.ByteString.Char8 qualified as BS
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8')
import Data.Version (showVersion)
import Effectful
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Prim (runPrim)
import Error.Diagnose (Diagnostic, addFile, defaultStyle, printDiagnostic)
import Monkey (interpret, parseFile)
import Monkey.Options
import Paths_monkey (version)
import System.Exit (die, exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  opts <- execParser parser
  main' opts

main' :: Options -> IO ()
main'
  Options
    { sourceFile
    , unicode
    , color
    , printVersion
    } = runEff do
    when printVersion do
      liftIO $ hPutStrLn stderr (showVersion version) *> exitSuccess
    (fp, encoded) <- liftIO case sourceFile of
      Just fp | fp /= "-" -> (fp,) <$> BS.readFile fp
      _ -> ("<STDIN>",) <$> BS.getContents
    decoded <- liftIO case decodeUtf8' encoded of
      Left err -> die (show err)
      Right decoded -> pure decoded
    res <- runPrim . runErrorNoCallStack @(Diagnostic Text) $ do
      parsed <- parseFile fp decoded
      interpret parsed
    case res of
      Left diag -> do
        let diag' = addFile diag fp (unpack decoded)
        printDiagnostic stderr unicode color 2 defaultStyle diag'
        liftIO exitFailure
      Right () -> pure ()
