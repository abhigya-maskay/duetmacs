{-# LANGUAGE StrictData #-}

module Main where

import Data.List (find, intercalate)
import Data.Maybe (fromMaybe)
import Options.Applicative ((<**>))
import qualified Options.Applicative as OA

data LogLevel
  = LogDebug
  | LogInfo
  | LogWarn
  | LogError
  deriving (Eq, Show)

data CliCommand
  = CmdVersion
  | CmdDoctor
  | CmdRpc
  | CmdPrompt
  deriving (Eq, Show)

data CliOptions = CliOptions
  { optShowVersion :: Bool
  , optNoColor :: Bool
  , optLogLevel :: LogLevel
  , optCommand :: Maybe CliCommand
  }
  deriving (Eq, Show)

main :: IO ()
main = OA.customExecParser prefsWithHelp opts >>= runCli

runCli :: CliOptions -> IO ()
runCli cliOpts
  | optShowVersion cliOpts = dispatch CmdVersion
  | otherwise = mapM_ dispatch (optCommand cliOpts)

prefsWithHelp :: OA.ParserPrefs
prefsWithHelp = OA.prefs (OA.showHelpOnEmpty <> OA.showHelpOnError)

dispatch :: CliCommand -> IO ()
dispatch CmdVersion = putStrLn "Version called"
dispatch CmdDoctor = putStrLn "Doctor called"
dispatch CmdRpc = putStrLn "RPC called"
dispatch CmdPrompt = putStrLn "Prompt called"

opts :: OA.ParserInfo CliOptions
opts = OA.info (cliOptionsParser <**> OA.helper) OA.fullDesc
  where
    cliOptionsParser = CliOptions <$> versionFlag <*> noColorFlag <*> logLevelOption <*> commandParser

    versionFlag = OA.switch (OA.long "version" <> OA.short 'V' <> OA.help "Print version information")
    noColorFlag = OA.switch (OA.long "no-color" <> OA.short 'n' <> OA.help "Disable colored output")
    logLevelOption =
      OA.option logLevelReader
        ( OA.long "log-level"
            <> OA.metavar "LEVEL"
            <> OA.value defaultLogLevel
            <> OA.showDefaultWith (const defaultLogLevelName)
            <> OA.help ("Set log level (" <> intercalate "|" logLevelNames <> ")")
        )
    commandParser = OA.optional . OA.hsubparser $ mconcat commandInfos

    commandInfos =
      [ OA.command "version" (OA.info (pure CmdVersion) (OA.progDesc "Show version information"))
      , OA.command "doctor" (OA.info (pure CmdDoctor) (OA.progDesc "Run diagnostics"))
      , OA.command "rpc" (OA.info (pure CmdRpc) (OA.progDesc "Start RPC server"))
      , OA.command "prompt" (OA.info (pure CmdPrompt) (OA.progDesc "Run prompt workflow"))
      ]

    logLevelReader :: OA.ReadM LogLevel
    logLevelReader = OA.eitherReader $ \s ->
      maybe (Left invalidLevel) Right (lookup s logLevels)

    invalidLevel = "Log level must be one of " <> intercalate ", " logLevelNames

defaultLogLevel :: LogLevel
defaultLogLevel = LogInfo

defaultLogLevelName :: String
defaultLogLevelName =
  fromMaybe "info" (fst <$> find ((== defaultLogLevel) . snd) logLevels)

logLevels :: [(String, LogLevel)]
logLevels =
  [ ("debug", LogDebug)
  , ("info", LogInfo)
  , ("warn", LogWarn)
  , ("error", LogError)
  ]

logLevelNames :: [String]
logLevelNames = map fst logLevels
