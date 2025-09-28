
module Duet.Rpc.CLI.Shell (runCli) where

import qualified Data.Text as T

import Duet.Rpc.CLI.Core
  ( CliCommand (..)
  , CliInstruction (..)
  , CliOptions (..)
  , CommandAction (..)
  , commandActionOf
  , planExecution
  , parseCli
  )
import Duet.Rpc.Logger (logDebug, withLogger)
import Duet.Rpc.OutputFormatter.Shell (ShellFormatter (..), initShellFormatter)
import Duet.Rpc.VersionManager (renderVersion)

runCli :: IO ()
runCli = do
  cliOpts <- parseCli
  withLogger (optLogLevel cliOpts) $ \logEnv -> do
    logDebug logEnv "duet-rpc CLI startup"
    formatter <- initShellFormatter cliOpts
    case planExecution cliOpts of
      Just instr -> runInstruction formatter instr
      Nothing -> pure ()
  where
    runInstruction :: ShellFormatter -> CliInstruction -> IO ()
    runInstruction fmt (InstrRunCommand cmd) = dispatch fmt cmd

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
