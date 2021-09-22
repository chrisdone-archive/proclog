{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Foldable
import           Data.String
import           Data.Time
import           Data.Traversable
import           Options.Applicative
import           System.Environment
import           System.FilePath
import           System.IO
import           System.Posix.Signals
import           System.Process.Typed

data Config = Config
  { bin :: FilePath -- ^ Program to run
  , logpath :: FilePath -- ^ Where to write logs
  , stdinpath :: Maybe FilePath
    -- ^ If specified, also write a copy of stdin to this file.
  , stdoutpath :: Maybe FilePath
    -- ^ If specified, also write a copy of stdout to this file.
  , stderrpath :: Maybe FilePath
    -- ^ If specified, also write a copy of stderr to this file.
  }

main :: IO ()
main = do
  mainId <- myThreadId
  _ <-
    installHandler softwareTermination (CatchOnce (killThread mainId)) Nothing
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
            hPutStrLn handle ("Arguments: " ++ show guestArgs)
            hSetBuffering handle NoBuffering
            hSetBuffering (getStdin process) NoBuffering
            hSetBuffering (getStdout process) NoBuffering
            hSetBuffering (getStderr process) NoBuffering
            rawinhandle <-
              for
                stdinpath
                (\fp -> do
                   h <- openFile fp WriteMode
                   hSetBuffering h NoBuffering
                   pure h)
            rawerrhandle <- for
              stderrpath
              (\fp -> do
                 h <- openFile fp WriteMode
                 hSetBuffering h NoBuffering
                 pure h)
            rawouthandle <- for
              stdoutpath
              (\fp -> do
                 h <- openFile fp WriteMode
                 hSetBuffering h NoBuffering
                 pure h)
            concurrently_
              (runConduitRes
                 (CB.sourceHandle stdin .| logLines handle "stdin" rawinhandle .|
                  CB.sinkHandle (getStdin process)))
              (concurrently_
                 (runConduitRes
                    (CB.sourceHandle (getStdout process) .|
                     logLines handle "stdout" rawouthandle .|
                     CB.sinkHandle stdout))
                 (runConduitRes
                    (CB.sourceHandle (getStderr process) .|
                     logLines handle "stderr" rawerrhandle .|
                     CB.sinkHandle stderr)))))
  hFlush stdout
  hFlush stderr

logLines ::
     MonadIO m
  => Handle
  -> ByteString
  -> Maybe Handle
  -> ConduitM ByteString ByteString m ()
logLines handle label mrawhandle =
  CL.mapM
    (\blob ->
       liftIO
         (do for_ mrawhandle (flip S8.hPutStr blob)
             time <- getCurrentTime
             S8.hPutStr
               handle
               (fromString (show time) <> " " <> label <>
                S8.unlines (map ("> " <>) (S8.lines blob)))
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
  strOption (long "log" <> metavar "FILE" <> help "Log file location") <*>
  optional
    (strOption
       (long "stdin-file" <> metavar "FILE" <> help "Write stdin to here too")) <*>
  optional
    (strOption
       (long "stdout-file" <> metavar "FILE" <> help "Write stdout to here too")) <*>
  optional
    (strOption
       (long "stderr-file" <> metavar "FILE" <> help "Write stderr to here too"))
