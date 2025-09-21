module Duet.Rpc.CLI.Shell (runCli) where

import Duet.Rpc.CLI.Core
  ( CliCommand (..)
  , CliInstruction (..)
  , cliParserInfo
  , planExecution
  , prefsWithHelp
  )
import Duet.Rpc.VersionManager (renderVersion)
import qualified Options.Applicative as OA

runCli :: IO ()
runCli = do
  cliOpts <- OA.customExecParser prefsWithHelp cliParserInfo
  mapM_ runInstruction (planExecution cliOpts)
  where
    runInstruction :: CliInstruction -> IO ()
    runInstruction InstrShowVersion = dispatch CmdVersion
    runInstruction (InstrRunCommand cmd) = dispatch cmd

    dispatch :: CliCommand -> IO ()
    dispatch CmdVersion = putStrLn renderVersion
    dispatch CmdDoctor = putStrLn "Doctor called"
    dispatch CmdRpc = putStrLn "RPC called"
    dispatch CmdPrompt = putStrLn "Prompt called"
