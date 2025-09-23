
module Duet.Rpc.CLI.Shell (runCli) where

import qualified Data.Text as T
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)

import qualified Options.Applicative as OA

import Duet.Rpc.CLI.Core
  ( CliCommand (..)
  , CliInstruction (..)
  , CliOptions (..)
  , CommandAction (..)
  , commandActionOf
  , planExecution
  , parseCli
  , defaultCliOptions
  , noColorLongFlag
  , noColorShortFlag
  )
import Duet.Rpc.Logger (logDebug, withLogger)
import Duet.Rpc.OutputFormatter.Shell (ShellFormatter (..), initShellFormatter)
import Duet.Rpc.VersionManager (renderVersion)

runCli :: IO ()
runCli = do
  args <- getArgs
  progName <- getProgName
  case parseCli args of
    OA.Success cliOpts ->
      withLogger (optLogLevel cliOpts) $ \logEnv -> do
        logDebug logEnv "duet-rpc CLI startup"
        formatter <- initShellFormatter cliOpts
        case planExecution cliOpts of
          Just instr -> runInstruction formatter instr
          Nothing -> pure ()
    OA.Failure failure -> do
      let (msg, code) = OA.renderFailure failure progName
      formatter <- initShellFormatter (formatterOptions args)
      let writer = if code == ExitSuccess then writeStdout else writeStderr
      writer formatter (T.pack msg)
      exitWith code
    OA.CompletionInvoked compl -> do
      completionOutput <- OA.execCompletion compl progName
      putStr completionOutput
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

    formatterOptions :: [String] -> CliOptions
    formatterOptions xs =
      defaultCliOptions { optNoColor = any isNoColorFlag xs }
      where
        isNoColorFlag :: String -> Bool
        isNoColorFlag arg = arg == ("--" ++ noColorLongFlag) || arg == ("-" ++ [noColorShortFlag])
