module Duet.Rpc.CLI.Core
  ( CliOptions (..),
    LogLevel (..),
    CliCommand (..),
    CommandAction (..),
    commandActions,
    commandInfos,
    commandDescriptions,
    commandActionOf,
    planExecution,
    defaultCliOptions,
    LogLevelInfo (..),
    logLevelInfos,
    logLevelSeverity,
    logLevelNames,
    prefsWithHelp,
    cliParserInfo,
    noColorLongFlag,
    noColorShortFlag,
  )
where

import Data.List (find, intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Katip as K
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
  deriving (Eq, Ord, Show)

data CliOptions = CliOptions
  { optShowVersion :: Bool,
    optNoColor :: Bool,
    optLogLevel :: LogLevel,
    optCommand :: Maybe CliCommand
  }
  deriving (Eq, Show)

data CommandAction
  = CommandActionVersion
  | CommandActionDoctor
  | CommandActionRpc
  | CommandActionPrompt
  deriving (Eq, Show)

data CommandInfo = CommandInfo
  { commandName :: String,
    commandValue :: CliCommand,
    commandDesc :: String,
    commandAction :: CommandAction
  }

data CommandRegistry = CommandRegistry
  { registryInfos :: [CommandInfo],
    registryActions :: Map.Map CliCommand CommandAction
  }

data LogLevelInfo = LogLevelInfo
  { logLevelName :: String,
    logLevelValue :: LogLevel,
    logLevelKatipSeverity :: K.Severity
  }

logLevelInfos :: [LogLevelInfo]
logLevelInfos =
  [ LogLevelInfo "debug" LogDebug K.DebugS,
    LogLevelInfo "info" LogInfo K.InfoS,
    LogLevelInfo "warn" LogWarn K.WarningS,
    LogLevelInfo "error" LogError K.ErrorS
  ]

logLevelSeverity :: LogLevel -> K.Severity
logLevelSeverity lvl = maybe K.WarningS logLevelKatipSeverity (find matches logLevelInfos)
  where
    matches LogLevelInfo {logLevelValue} = logLevelValue == lvl

cliCommands :: CommandRegistry
cliCommands = fromCommandInfos commandInfos

commandInfos :: [CommandInfo]
commandInfos =
  [ CommandInfo "version" CmdVersion "Show version information" CommandActionVersion,
    CommandInfo "doctor" CmdDoctor "Run diagnostics" CommandActionDoctor,
    CommandInfo "rpc" CmdRpc "Start RPC server" CommandActionRpc,
    CommandInfo "prompt" CmdPrompt "Run prompt workflow" CommandActionPrompt
  ]

commandActions :: [(CliCommand, CommandAction)]
commandActions = Map.toList $ registryActions cliCommands

commandDescriptions :: [(CliCommand, String)]
commandDescriptions = map toPair commandInfos
  where
    toPair CommandInfo {commandValue, commandDesc} = (commandValue, commandDesc)

commandActionOf :: CliCommand -> Maybe CommandAction
commandActionOf cmd = Map.lookup cmd (registryActions cliCommands)

defaultCliOptions :: CliOptions
defaultCliOptions =
  CliOptions
    { optShowVersion = False,
      optNoColor = False,
      optLogLevel = LogWarn,
      optCommand = Nothing
    }

fromCommandInfos :: [CommandInfo] -> CommandRegistry
fromCommandInfos infos =
  CommandRegistry
    { registryInfos = infos,
      registryActions = Map.fromList (map toPair infos)
    }
  where
    toPair CommandInfo {commandValue, commandAction} = (commandValue, commandAction)

planExecution :: CliOptions -> Maybe CliCommand
planExecution CliOptions {..}
  | optShowVersion = Just CmdVersion
  | Just cmd <- optCommand = Just cmd
  | otherwise = Nothing

noColorLongFlag :: String
noColorLongFlag = "no-color"

noColorShortFlag :: Char
noColorShortFlag = 'n'

prefsWithHelp :: OA.ParserPrefs
prefsWithHelp = OA.prefs (OA.showHelpOnEmpty <> OA.showHelpOnError)

cliParserInfo :: OA.ParserInfo CliOptions
cliParserInfo = buildCliParserInfo cliCommands

buildCliParserInfo :: CommandRegistry -> OA.ParserInfo CliOptions
buildCliParserInfo registry =
  OA.info
    (cliOptionsParser registry <**> OA.helper)
    ( OA.fullDesc
        <> OA.header "duet-rpc [COMMAND] [OPTIONS]"
        <> OA.footer "See 'duet-rpc <command> --help' for more information."
    )
  where
    cliOptionsParser :: CommandRegistry -> OA.Parser CliOptions
    cliOptionsParser reg =
      CliOptions <$> versionFlag <*> noColorFlag <*> logLevelOption <*> commandParser reg

    versionFlag :: OA.Parser Bool
    versionFlag = OA.switch (OA.long "version" <> OA.short 'V' <> OA.help "Print version information")

    noColorFlag :: OA.Parser Bool
    noColorFlag = OA.switch (OA.long noColorLongFlag <> OA.short noColorShortFlag <> OA.help "Disable colored output")

    logLevelOption :: OA.Parser LogLevel
    logLevelOption =
      OA.option
        logLevelReader
        ( OA.long "log-level"
            <> OA.metavar "LEVEL"
            <> OA.value defaultLogLevel
            <> OA.showDefaultWith (const defaultLogLevelName)
            <> OA.help ("Set log level (" <> intercalate "|" logLevelNames <> ")")
        )

    commandParser :: CommandRegistry -> OA.Parser (Maybe CliCommand)
    commandParser reg = OA.optional . OA.hsubparser $ foldMap mkCommand (registryInfos reg)
      where
        mkCommand :: CommandInfo -> OA.Mod OA.CommandFields CliCommand
        mkCommand CommandInfo {..} =
          OA.command commandName (OA.info (pure commandValue) (OA.progDesc commandDesc))

    logLevelReader :: OA.ReadM LogLevel
    logLevelReader = OA.eitherReader $ \s ->
      maybe (Left invalidLevel) Right (lookup s logLevelLookup)

    invalidLevel :: String
    invalidLevel = "Log level must be one of " <> intercalate ", " logLevelNames

    defaultLogLevel :: LogLevel
    defaultLogLevel = LogWarn

    defaultLogLevelName :: String
    defaultLogLevelName = logLevelName defaultLogInfo

    defaultLogInfo :: LogLevelInfo
    defaultLogInfo =
      fromMaybe warnInfo (find ((== LogWarn) . logLevelValue) logLevelInfos)

    warnInfo :: LogLevelInfo
    warnInfo = LogLevelInfo "warn" LogWarn K.WarningS

    logLevelLookup :: [(String, LogLevel)]
    logLevelLookup = map toTuple logLevelInfos

    toTuple :: LogLevelInfo -> (String, LogLevel)
    toTuple LogLevelInfo {logLevelName, logLevelValue} = (logLevelName, logLevelValue)

logLevelNames :: [String]
logLevelNames = map logLevelName logLevelInfos
