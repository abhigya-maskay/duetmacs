{-# LANGUAGE OverloadedStrings #-}

module Duet.Rpc.CLI.Shell (runCli) where

import qualified Data.Text as T
import Duet.Rpc.CLI.Core
  ( CliCommand (..)
  , CliInstruction (..)
  , CliOptions (..)
  , LogLevel (..)
  , cliParserInfo
  , planExecution
  , prefsWithHelp
  )
import Duet.Rpc.VersionManager (renderVersion)
import Duet.Rpc.OutputFormatter.Shell (ShellFormatter (..), initShellFormatter)
import qualified Options.Applicative as OA
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)

runCli :: IO ()
runCli = do
  args <- getArgs
  progName <- getProgName
  case OA.execParserPure prefsWithHelp cliParserInfo args of
    OA.Success cliOpts -> do
      formatter <- initShellFormatter cliOpts
      mapM_ (runInstruction formatter) (planExecution cliOpts)
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
    runInstruction fmt InstrShowVersion = dispatch fmt CmdVersion
    runInstruction fmt (InstrRunCommand cmd) = dispatch fmt cmd

    dispatch :: ShellFormatter -> CliCommand -> IO ()
    dispatch fmt cmd =
      let out = writeStdout fmt
       in case cmd of
            CmdVersion -> out renderVersion
            CmdDoctor -> out "Doctor called"
            CmdRpc -> out "RPC called"
            CmdPrompt -> out "Prompt called"

    formatterOptions :: [String] -> CliOptions
    formatterOptions xs =
      CliOptions
        { optShowVersion = False
        , optNoColor = any (`elem` ["--no-color", "-n"]) xs
        , optLogLevel = LogInfo
        , optCommand = Nothing
        }
