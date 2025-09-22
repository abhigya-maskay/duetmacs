module Main where

import Duet.Rpc.CLI.Shell (runCli)
import System.Environment (getArgs, withArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> withArgs ["--help"] runCli
    _ -> runCli
