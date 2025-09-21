module Duet.Rpc.CLI.Shell (runCli) where

import qualified Options.Applicative as OA
import Duet.Rpc.CLI.Core
  ( CliCommand (..)
  , CliInstruction (..)
  , cliParserInfo
  , planExecution
  , prefsWithHelp
  )

runCli :: IO ()
runCli = do
  cliOpts <- OA.customExecParser prefsWithHelp cliParserInfo
  mapM_ runInstruction (planExecution cliOpts)
  where
    runInstruction :: CliInstruction -> IO ()
    runInstruction InstrShowVersion = dispatch CmdVersion
    runInstruction (InstrRunCommand cmd) = dispatch cmd

    dispatch :: CliCommand -> IO ()
    dispatch CmdVersion = putStrLn "Version called"
    dispatch CmdDoctor = putStrLn "Doctor called"
    dispatch CmdRpc = putStrLn "RPC called"
    dispatch CmdPrompt = putStrLn "Prompt called"
