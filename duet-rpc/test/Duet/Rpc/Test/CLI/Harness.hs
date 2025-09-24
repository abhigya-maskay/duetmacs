module Duet.Rpc.Test.CLI.Harness
  ( CliInvocation (..)
  , CliResult (..)
  , defaultInvocation
  , runCli
  , findDuetRpcExecutable
  , containsAnsi
  ) where

import Control.Monad (filterM)
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified System.FilePath as FP
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode)
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed
  ( proc
  , readProcess
  , setEnv
  , setWorkingDir
  )
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , getCurrentDirectory
  , listDirectory
  )
import System.Info (os)

data CliInvocation = CliInvocation
  { cliArgs :: [String]
  , cliEnv :: Map String String
  }

defaultInvocation :: CliInvocation
defaultInvocation = CliInvocation {cliArgs = [], cliEnv = Map.empty}

data CliResult = CliResult
  { cliExitCode :: ExitCode
  , cliStdout :: Text
  , cliStderr :: Text
  }

runCli :: CliInvocation -> IO CliResult
runCli invocation = withSystemTempDirectory "duet-rpc-test" $ \tmpDir -> do
  exe <- findDuetRpcExecutable
  baseEnv <- getEnvironment
  let process =
        setWorkingDir tmpDir
          . setEnv (mergeEnvs baseEnv (cliEnv invocation))
          $ proc exe (cliArgs invocation)
  (exitCode, outBytes, errBytes) <- readProcess process
  pure
    CliResult
      { cliExitCode = exitCode
      , cliStdout = TE.decodeUtf8 (BL.toStrict outBytes)
      , cliStderr = TE.decodeUtf8 (BL.toStrict errBytes)
      }

mergeEnvs :: [(String, String)] -> Map String String -> [(String, String)]
mergeEnvs base overrides = Map.toList (Map.union overrides (Map.fromList base))

findDuetRpcExecutable :: IO FilePath
findDuetRpcExecutable = do
  override <- lookupEnv "DUET_RPC_TEST_BIN"
  case override of
    Just path -> pure path
    Nothing -> do
      cwd <- getCurrentDirectory
      discovered <- discoverUnderNewstyle cwd
      maybe (fail missingBinaryMessage) pure discovered

discoverUnderNewstyle :: FilePath -> IO (Maybe FilePath)
discoverUnderNewstyle cwd = do
  let buildRoot = cwd FP.</> "dist-newstyle" FP.</> "build"
  exists <- doesDirectoryExist buildRoot
  if not exists
    then pure Nothing
    else do
      archDirs <- existingSubdirs buildRoot
      ghcDirs <- concatMapM existingSubdirs archDirs
      pkgDirs <- concatMapM existingSubdirs ghcDirs
      candidates <- filterM doesFileExist (candidatePaths pkgDirs)
      pure (listToMaybe candidates)

candidatePaths :: [FilePath] -> [FilePath]
candidatePaths pkgDirs =
  [ pkgDir FP.</> "x"
      FP.</> "duet-rpc"
      FP.</> "build"
      FP.</> "duet-rpc"
      FP.</> name
  | pkgDir <- pkgDirs
  , name <- targetNames
  ]

existingSubdirs :: FilePath -> IO [FilePath]
existingSubdirs dir = do
  entries <- listDirectory dir
  let paths = map (dir FP.</>) entries
  filterM doesDirectoryExist paths

concatMapM :: (a -> IO [b]) -> [a] -> IO [b]
concatMapM f = fmap concat . mapM f

targetNames :: [FilePath]
targetNames
  | os == "mingw32" = ["duet-rpc.exe", "duet-rpc"]
  | otherwise = ["duet-rpc"]

missingBinaryMessage :: String
missingBinaryMessage =
  "Unable to locate duet-rpc executable under dist-newstyle; "
    <> "build the project or set DUET_RPC_TEST_BIN"

containsAnsi :: Text -> Bool
containsAnsi = T.any (== '\ESC')
