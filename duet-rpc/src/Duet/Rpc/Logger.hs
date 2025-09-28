module Duet.Rpc.Logger
  ( LogEnv
  , initLogger
  , closeLogger
  , withLogger
  , logDebug
  , logInfo
  , logWarn
  , logError
  ) where

import Control.Exception (bracket, displayException)
import Control.Monad (void)
import qualified Data.Text as T
import Duet.Rpc.CLI.Core (LogLevel (..), logLevelSeverity)
import qualified Katip as K
import Katip.Scribes.Handle (mkHandleScribe, ColorStrategy(..))
import System.Environment (lookupEnv)
import System.IO (stderr, openFile, IOMode(..), hPutStrLn)
import System.IO.Error (tryIOError)

newtype LogEnv = LogEnv K.LogEnv

initLogger :: LogLevel -> IO LogEnv
initLogger level = do
  logEnv <- K.initLogEnv "duet-rpc" "production"
  let severity = logLevelSeverity level
  let permit = K.permitItem severity

  let register name scribe env = K.registerScribe name scribe K.defaultScribeSettings env

  maybeLogPath <- lookupEnv "DUET_RPC_LOG"
  logEnv' <- case maybeLogPath of
    Nothing -> do
      stderrScribe <- mkHandleScribe ColorIfTerminal stderr permit K.V2
      register "stderr" stderrScribe logEnv

    Just logPath -> do
      result <- tryIOError (openFile logPath AppendMode)
      case result of
        Right handle -> do
          fileScribe <- mkHandleScribe (ColorLog False) handle permit K.V2
          register "file" fileScribe logEnv

        Left err -> do
          hPutStrLn stderr $ "Warning: Cannot write to log file '" ++ logPath ++ "': " ++ displayException err ++ ". Using stderr instead."
          stderrScribe <- mkHandleScribe ColorIfTerminal stderr permit K.V2
          register "stderr" stderrScribe logEnv

  pure $ LogEnv logEnv'

closeLogger :: LogEnv -> IO ()
closeLogger (LogEnv env) = void $ K.closeScribes env

withLogger :: LogLevel -> (LogEnv -> IO a) -> IO a
withLogger level action = bracket (initLogger level) closeLogger action

logDebug :: LogEnv -> T.Text -> IO ()
logDebug = logWith K.DebugS

logInfo :: LogEnv -> T.Text -> IO ()
logInfo = logWith K.InfoS

logWarn :: LogEnv -> T.Text -> IO ()
logWarn = logWith K.WarningS

logError :: LogEnv -> T.Text -> IO ()
logError = logWith K.ErrorS

logWith :: K.Severity -> LogEnv -> T.Text -> IO ()
logWith severity (LogEnv env) msg = K.runKatipT env $ K.logMsg "" severity (K.logStr msg)
