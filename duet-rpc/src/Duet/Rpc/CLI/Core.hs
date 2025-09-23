module Duet.Rpc.CLI.Core
  ( CliOptions (..)
  , CliInstruction (..)
  , planExecution
  , LogLevel (..)
  , CliCommand (..)
  , prefsWithHelp
  , cliParserInfo
  , noColorLongFlag
  , noColorShortFlag
  ) where

import Data.List (intercalate)
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

data CliInstruction
  = InstrRunCommand CliCommand
  deriving (Eq, Show)

planExecution :: CliOptions -> Maybe CliInstruction
planExecution cliOpts
  | optShowVersion cliOpts = Just (InstrRunCommand CmdVersion)
  | Just cmd <- optCommand cliOpts = Just (InstrRunCommand cmd)
  | otherwise = Nothing

noColorLongFlag :: String
noColorLongFlag = "no-color"

noColorShortFlag :: Char
noColorShortFlag = 'n'

prefsWithHelp :: OA.ParserPrefs
prefsWithHelp = OA.prefs (OA.showHelpOnEmpty <> OA.showHelpOnError)

cliParserInfo :: OA.ParserInfo CliOptions
cliParserInfo =
  OA.info
    (cliOptionsParser <**> OA.helper)
    ( OA.fullDesc
        <> OA.header "duet-rpc [COMMAND] [OPTIONS]"
        <> OA.footer "See 'duet-rpc <command> --help' for more information."
    )
  where
    cliOptionsParser :: OA.Parser CliOptions
    cliOptionsParser = CliOptions <$> versionFlag <*> noColorFlag <*> logLevelOption <*> commandParser

    versionFlag :: OA.Parser Bool
    versionFlag = OA.switch (OA.long "version" <> OA.short 'V' <> OA.help "Print version information")

    noColorFlag :: OA.Parser Bool
    noColorFlag = OA.switch (OA.long noColorLongFlag <> OA.short noColorShortFlag <> OA.help "Disable colored output")

    logLevelOption :: OA.Parser LogLevel
    logLevelOption =
      OA.option logLevelReader
        ( OA.long "log-level"
            <> OA.metavar "LEVEL"
            <> OA.value defaultLogLevel
            <> OA.showDefaultWith (const defaultLogLevelName)
            <> OA.help ("Set log level (" <> intercalate "|" logLevelNames <> ")")
        )

    commandParser :: OA.Parser (Maybe CliCommand)
    commandParser = OA.optional . OA.hsubparser $ mconcat commandInfos

    commandInfos :: [OA.Mod OA.CommandFields CliCommand]
    commandInfos = map mkCommand commands
      where
        mkCommand (name, cmd, desc) =
          OA.command name (OA.info (pure cmd) (OA.progDesc desc))

        commands =
          [ ("version", CmdVersion, "Show version information")
          , ("doctor", CmdDoctor, "Run diagnostics")
          , ("rpc", CmdRpc, "Start RPC server")
          , ("prompt", CmdPrompt, "Run prompt workflow")
          ]

    logLevelReader :: OA.ReadM LogLevel
    logLevelReader = OA.eitherReader $ \s ->
      maybe (Left invalidLevel) Right (lookup s logLevels)

    invalidLevel :: String
    invalidLevel = "Log level must be one of " <> intercalate ", " logLevelNames

    defaultLogLevel :: LogLevel
    defaultLogLevel = LogInfo

    defaultLogLevelName :: String
    defaultLogLevelName = "info"

    logLevels :: [(String, LogLevel)]
    logLevels =
      [ ("debug", LogDebug)
      , ("info", LogInfo)
      , ("warn", LogWarn)
      , ("error", LogError)
      ]

    logLevelNames :: [String]
    logLevelNames = map fst logLevels
