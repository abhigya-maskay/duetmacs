
module Duet.Rpc.CLI.Shell (runCli) where

import Data.Foldable (for_)
import qualified Data.Text as T

import qualified Options.Applicative as OA

import Duet.Rpc.CLI.Core
  ( CliCommand (..)
  , CliOptions (..)
  , CommandAction (..)
  , cliParserInfo
  , commandActionOf
  , planExecution
  , prefsWithHelp
  )
import Duet.Rpc.Logger (logDebug, withLogger)
import Duet.Rpc.OutputFormatter.Shell (ShellFormatter (..), initShellFormatter)
import Duet.Rpc.VersionManager (renderVersion)

runCli :: IO ()
runCli = do
  cliOpts <- OA.customExecParser prefsWithHelp cliParserInfo
  withLogger (optLogLevel cliOpts) $ \logEnv -> do
    logDebug logEnv "duet-rpc CLI startup"
    formatter <- initShellFormatter cliOpts
    for_ (planExecution cliOpts) (dispatch formatter)
  where
    dispatch :: ShellFormatter -> CliCommand -> IO ()
    dispatch fmt cmd = maybe (pure ()) (`runAction` fmt) (commandActionOf cmd)

    runAction :: CommandAction -> ShellFormatter -> IO ()
    runAction action formatter =
      case action of
        CommandActionVersion -> writeStdout formatter renderVersion
        CommandActionDoctor -> runDoctor formatter
        CommandActionRpc -> runRpc formatter
        CommandActionPrompt -> runPrompt formatter

    runDoctor, runRpc, runPrompt :: ShellFormatter -> IO ()
    runDoctor = writeStdoutWith "Doctor called"
    runRpc = writeStdoutWith "RPC called"
    runPrompt = writeStdoutWith "Prompt called"

    writeStdoutWith :: T.Text -> ShellFormatter -> IO ()
    writeStdoutWith msg fmt = writeStdout fmt msg
