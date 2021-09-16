{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Main where

import           Control.Concurrent.Async
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.String
import           Data.Time
import           Options.Applicative
import           System.Environment
import           System.FilePath
import           System.IO
import           System.Process.Typed

data Config = Config
  { bin :: FilePath -- ^ Program to run
  , logpath :: FilePath -- ^ Where to write logs
  }

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  hSetBuffering stdin NoBuffering
  name <- getExecutablePath
  guestArgs <- getArgs
  hostArgs <- fmap lines (readFile (addExtension name ".args"))
  Config {..} <-
    handleParseResult (execParserPure defaultPrefs options hostArgs)
  withProcessWait_
    (setStderr
       createPipe
       (setStdout createPipe (setStdin createPipe (proc bin guestArgs))))
    (\process ->
       withFile
         logpath
         WriteMode
         (\handle -> do
            hSetBuffering handle NoBuffering
            hSetBuffering (getStdin process) NoBuffering
            hSetBuffering (getStdout process) NoBuffering
            hSetBuffering (getStderr process) NoBuffering
            concurrently_
              (runConduitRes
                 (CB.sourceHandle stdin .| logLines handle "stdin" .|
                  CB.sinkHandle (getStdin process)))
              (concurrently_
                 (runConduitRes
                    (CB.sourceHandle (getStdout process) .|
                     logLines handle "stdout" .|
                     CB.sinkHandle stdout))
                 (runConduitRes
                    (CB.sourceHandle (getStderr process) .|
                     logLines handle "stderr" .|
                     CB.sinkHandle stderr)))))
  hFlush stdout
  hFlush stderr

logLines ::
     MonadIO m => Handle -> ByteString -> ConduitM ByteString ByteString m ()
logLines handle label =
  CL.mapM
    (\blob ->
       liftIO
         (do time <- getCurrentTime
             S8.hPutStrLn
               handle
               (fromString (show time) <> " " <> label <>
                S.concat (map ("> " <>) (S8.lines blob)))
             pure blob))

options :: ParserInfo Config
options =
  info
    (config <**> helper)
    (fullDesc <> progDesc "Run the specified program transparently" <>
     header "proclog")

config :: Parser Config
config =
  Config <$>
  strOption (long "bin" <> metavar "PROGRAM" <> help "Path of program to run") <*>
  strOption (long "log" <> metavar "FILE" <> help "Log file location")
