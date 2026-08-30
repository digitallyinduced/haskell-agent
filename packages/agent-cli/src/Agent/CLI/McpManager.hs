-- | Interactive management of local stdio MCP servers.
module Agent.CLI.McpManager
    ( McpEntry(..)
    , McpEntryStatus(..)
    , McpManagerAction(..)
    , McpManagerState(..)
    , applyMcpManagerKey
    , initialMcpManagerState
    , parseMcpCommand
    , renderMcpManagerFrame
    , runMcpManager
    , suggestMcpName
    ) where

import Agent.CLI.Config
    ( HarnessConfig(..)
    , McpServerConfig(..)
    , loadHarnessConfigSnapshot
    , modifyHarnessConfig
    )
import Agent.CLI.Input (readApprovalLine)
import Agent.CLI.Picker
    ( PickerKey(..)
    , runOverlay
    )
import Agent.CLI.Style
    ( roleError
    , roleMuted
    , rolePrompt
    , roleSuccess
    , roleWarn
    )
import Agent.MCP (McpToolRegistration(..))
import Agent.Tools.Types (AppTool(..))
import Data.Char (isAlphaNum, isSpace, toLower)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.FilePath (dropExtension, takeFileName)
import System.IO
    ( hFlush
    , hIsTerminalDevice
    , isEOF
    , stderr
    , stdin
    )
import System.OsPath (OsPath)

data McpEntryStatus
    = McpDisabled
    | McpPendingRestart
    | McpReady !Int
    | McpUnavailable !Text
    deriving (Eq, Show)

data McpEntry = McpEntry
    { mcpEntryName :: !Text
    , mcpEntryConfig :: !McpServerConfig
    , mcpEntryStatus :: !McpEntryStatus
    , mcpEntryTools :: ![(Text, Text)]
    , mcpEntryWarnings :: ![Text]
    }
    deriving (Eq, Show)

data McpManagerState = McpManagerState
    { mcpManagerEntries :: ![McpEntry]
    , mcpManagerIndex :: !Int
    , mcpManagerExpanded :: !(Maybe Text)
    , mcpManagerRestartPending :: !Bool
    , mcpManagerNotice :: !(Maybe (Bool, Text))
    }
    deriving (Eq, Show)

data McpManagerAction
    = McpManagerClose
    | McpManagerRestart
    | McpManagerAdd
    | McpManagerToggle !Text
    | McpManagerRemove !Text
    deriving (Eq, Show)

initialMcpManagerState
    :: HarnessConfig
    -> [McpToolRegistration]
    -> [Text]
    -> Set Text
    -> Maybe (Bool, Text)
    -> McpManagerState
initialMcpManagerState config registrations warnings pending notice =
    McpManagerState
        { mcpManagerEntries =
            [ entryFor label server
            | (label, server) <- Map.toAscList config.configMcpServers
            ]
        , mcpManagerIndex = 0
        , mcpManagerExpanded = Nothing
        , mcpManagerRestartPending = not (Set.null pending)
        , mcpManagerNotice = notice
        }
  where
    toolsByServer =
        Map.fromListWith (<>)
            [ ( registration.mcpRegistrationServer
              , [ ( tool.appToolName
                  , firstLine tool.appToolDescription
                  )
                ]
              )
            | registration <- registrations
            , let tool = registration.mcpRegistrationTool
            ]
    entryFor label server =
        let entryWarnings = warningsFor label warnings
            tools = Map.findWithDefault [] label toolsByServer
            failed =
                find (Text.isInfixOf " failed to start:") entryWarnings
            status
                | not server.mcpEnabled = McpDisabled
                | label `Set.member` pending = McpPendingRestart
                | Just warning <- failed =
                    McpUnavailable (failureSummary warning)
                | otherwise = McpReady (length tools)
        in McpEntry
            { mcpEntryName = label
            , mcpEntryConfig = server
            , mcpEntryStatus = status
            , mcpEntryTools = tools
            , mcpEntryWarnings = entryWarnings
            }

applyMcpManagerKey
    :: PickerKey
    -> McpManagerState
    -> Either McpManagerAction McpManagerState
applyMcpManagerKey key state = case key of
    PickerKeyCancel -> Left McpManagerClose
    PickerKeyConfirm ->
        case selectedEntry state of
            Nothing -> Right state
            Just entry ->
                Right state
                    { mcpManagerExpanded =
                        if state.mcpManagerExpanded == Just entry.mcpEntryName
                            then Nothing
                            else Just entry.mcpEntryName
                    }
    PickerKeyChar 'r' -> Left McpManagerRestart
    PickerKeyChar 'R' -> Left McpManagerRestart
    PickerKeyChar 'a' -> Left McpManagerAdd
    PickerKeyChar 'A' -> Left McpManagerAdd
    PickerKeyChar ' ' -> selectedAction McpManagerToggle
    PickerKeyChar 'e' -> selectedAction McpManagerToggle
    PickerKeyChar 'E' -> selectedAction McpManagerToggle
    PickerKeyChar 'x' -> selectedAction McpManagerRemove
    PickerKeyChar 'X' -> selectedAction McpManagerRemove
    PickerKeyUp -> Right (moveSelection (-1) state)
    PickerKeyDown -> Right (moveSelection 1 state)
    _ -> Right state
  where
    selectedAction constructor =
        maybe (Right state) (Left . constructor . (.mcpEntryName))
            (selectedEntry state)

renderMcpManagerFrame :: Bool -> McpManagerState -> Text
renderMcpManagerFrame color state =
    Text.intercalate "\n" $
        [ rolePrompt color "MCP servers"
            <> roleMuted color
                (" · " <> Text.pack (show (length state.mcpManagerEntries))
                    <> " local")
            <> if state.mcpManagerRestartPending
                then roleWarn color " · restart pending"
                else ""
        ]
            <> maybe [] renderNotice state.mcpManagerNotice
            <> body
            <> [ roleMuted color
                    "↑↓/jk or scroll · enter details · a add · space enable/disable · x remove"
               , roleMuted color
                    "r restart/refresh · esc/q close"
               ]
  where
    renderNotice (success, message) =
        [ (if success then roleSuccess else roleError) color message ]
    body = case state.mcpManagerEntries of
        [] ->
            [ roleWarn color "No local MCP servers configured."
            , roleMuted color
                "Press a to add a stdio server."
            ]
        entries ->
            concat $
                zipWith (renderEntry color state) [0 ..] entries

runMcpManager
    :: Bool
    -> OsPath
    -> [McpToolRegistration]
    -> [Text]
    -> IO Bool
runMcpManager color home registrations warnings = do
    loadHarnessConfigSnapshot home >>= \case
        Left err -> do
            Text.hPutStrLn stderr (roleError color err)
            pure False
        Right (revision, config) -> do
            tty <- hIsTerminalDevice stdin
            if not tty
                then do
                    Text.hPutStrLn stderr $
                        renderMcpManagerFrame color
                            (initialMcpManagerState
                                config registrations warnings Set.empty Nothing)
                    hFlush stderr
                    pure False
                else loop revision config Set.empty False Nothing
  where
    loop revision config pending changed notice = do
        let state =
                initialMcpManagerState
                    config registrations warnings pending notice
        runOverlay (renderMcpManagerFrame color) applyMcpManagerKey state
            >>= \case
                Nothing -> pure changed
                Just McpManagerClose -> pure changed
                Just McpManagerRestart -> pure True
                Just McpManagerAdd ->
                    promptNewServer config >>= \case
                        Left err ->
                            loop revision config pending changed
                                (Just (False, err))
                        Right Nothing ->
                            loop revision config pending changed Nothing
                        Right (Just (label, server)) ->
                            persist
                                (config
                                    { configMcpServers =
                                        Map.insert label server
                                            config.configMcpServers
                                    })
                                (Set.insert label pending)
                                ("Added " <> label)
                Just (McpManagerToggle label) ->
                    case Map.lookup label config.configMcpServers of
                        Nothing ->
                            loop revision config pending changed
                                (Just (False, "MCP server no longer exists"))
                        Just server ->
                            let enabled = not server.mcpEnabled
                                updated =
                                    config
                                        { configMcpServers =
                                            Map.insert label
                                                (server { mcpEnabled = enabled })
                                                config.configMcpServers
                                        }
                                message =
                                    label <> if enabled
                                        then " enabled"
                                        else " disabled"
                            in persist updated (Set.insert label pending) message
                Just (McpManagerRemove label) -> do
                    confirmed <-
                        confirm
                            ("Remove MCP server " <> quote label <> "? [y/N] ")
                    if not confirmed
                        then loop revision config pending changed Nothing
                        else
                            persist
                                (config
                                    { configMcpServers =
                                        Map.delete label config.configMcpServers
                                    })
                                (Set.insert label pending)
                                ("Removed " <> label)
      where
        persist updated pending' message =
            modifyHarnessConfig home
                (\current _ ->
                    if current /= revision
                        then Left
                            "MCP configuration changed; reopen /mcp and retry"
                        else Right (updated, ())) >>= \case
                Left err ->
                    loop revision config pending changed (Just (False, err))
                Right (nextRevision, _, ()) ->
                    loop nextRevision updated pending' True
                        (Just (True, message))

promptNewServer
    :: HarnessConfig
    -> IO (Either Text (Maybe (Text, McpServerConfig)))
promptNewServer config =
    promptLine "Command: " >>= \case
        Nothing -> pure (Right Nothing)
        Just raw
            | Text.null (Text.strip raw) -> pure (Right Nothing)
            | otherwise -> case parseMcpCommand raw of
                Left err -> pure (Left err)
                Right (command, arguments) -> do
                    let suggested = suggestMcpName command arguments
                    promptLine ("Name [" <> suggested <> "]: ") >>= \case
                        Nothing -> pure (Right Nothing)
                        Just rawName ->
                            let label =
                                    if Text.null (Text.strip rawName)
                                        then suggested
                                        else Text.strip rawName
                            in if Map.member label config.configMcpServers
                                then pure
                                    (Left
                                        ("MCP server " <> quote label
                                            <> " already exists"))
                                else pure . Right . Just $
                                    ( label
                                    , McpServerConfig
                                        { mcpEnabled = True
                                        , mcpCommand = command
                                        , mcpArgs = arguments
                                        , mcpCwd = Nothing
                                        , mcpEnv = Map.empty
                                        , mcpStartupTimeoutSeconds = 30
                                        , mcpRequestTimeoutSeconds = 60
                                        }
                                    )

parseMcpCommand :: Text -> Either Text (Text, [Text])
parseMcpCommand input = do
    arguments <- parseWords (Text.unpack input)
    case map Text.pack arguments of
        command : rest
            | not (Text.null (Text.strip command)) ->
                Right (command, rest)
        _ -> Left "MCP command must not be empty"

suggestMcpName :: Text -> [Text] -> Text
suggestMcpName command arguments =
    fromMaybe "mcp-server" (find usable candidates)
  where
    candidates = map normalizeCandidate $
        case Text.toLower (baseName command) of
            "nix" ->
                dropLauncherOptions ["run"] arguments
                    <> [command]
            "npx" ->
                dropLauncherOptions [] arguments
                    <> [command]
            "node" ->
                dropLauncherOptions [] arguments
                    <> [command]
            _ -> command : arguments
    dropLauncherOptions subcommands =
        dropWhile
            (\value ->
                "-" `Text.isPrefixOf` value
                    || Text.toLower value `elem` subcommands)
    normalizeCandidate =
        Text.map normalizeChar
            . Text.pack . dropExtension . Text.unpack . baseName
    baseName =
        Text.pack . takeFileName . Text.unpack
    normalizeChar char
        | isAlphaNum char || char `elem` ['-', '_', '.'] = toLower char
        | otherwise = '-'
    usable candidate =
        not (Text.null candidate)
            && candidate `notElem` ["run", "exec", "npx", "node", "nix"]
            && not ("-" `Text.isPrefixOf` candidate)

renderEntry :: Bool -> McpManagerState -> Int -> McpEntry -> [Text]
renderEntry color state index entry =
    [ prefix
        <> enabledMarker
        <> " "
        <> entry.mcpEntryName
        <> " "
        <> renderStatus color entry.mcpEntryStatus
    ]
        <> if state.mcpManagerExpanded == Just entry.mcpEntryName
            then renderDetails color entry
            else []
  where
    prefix =
        if index == state.mcpManagerIndex
            then rolePrompt color "› "
            else "  "
    enabledMarker =
        if entry.mcpEntryConfig.mcpEnabled
            then roleSuccess color "●"
            else roleMuted color "○"

renderStatus :: Bool -> McpEntryStatus -> Text
renderStatus color = \case
    McpDisabled -> roleMuted color "[disabled]"
    McpPendingRestart -> roleWarn color "[restart pending]"
    McpReady count ->
        roleSuccess color "[ready]"
            <> roleMuted color
                (" · " <> Text.pack (show count) <> plural count " tool")
    McpUnavailable reason ->
        roleError color "[unavailable]"
            <> roleMuted color (" · " <> truncateText 72 reason)

renderDetails :: Bool -> McpEntry -> [Text]
renderDetails color entry =
    [ roleMuted color ("    command: " <> renderCommand server)
    ]
        <> maybe []
            (\cwd -> [roleMuted color ("    cwd: " <> cwd)])
            server.mcpCwd
        <> unlessEmpty envNames
            [ roleMuted color
                ("    env: " <> Text.intercalate ", " envNames
                    <> " (values hidden)")
            ]
        <> [ roleMuted color
                ("    timeouts: startup "
                    <> Text.pack (show server.mcpStartupTimeoutSeconds)
                    <> "s · request "
                    <> Text.pack (show server.mcpRequestTimeoutSeconds)
                    <> "s")
           ]
        <> toolLines
        <> warningLines
  where
    server = entry.mcpEntryConfig
    envNames = Map.keys server.mcpEnv
    toolLines = case entry.mcpEntryTools of
        [] -> [roleMuted color "    tools: none exposed"]
        tools ->
            roleMuted color
                ("    tools (" <> Text.pack (show (length tools)) <> "):")
                : [ roleMuted color
                        ("      " <> name
                            <> if Text.null description
                                then ""
                                else " — " <> truncateText 88 description)
                  | (name, description) <- tools
                  ]
    warningLines =
        [ roleWarn color ("    warning: " <> warningSummary warning)
        | warning <- entry.mcpEntryWarnings
        , not (" failed to start:" `Text.isInfixOf` warning)
        ]

renderCommand :: McpServerConfig -> Text
renderCommand server =
    Text.intercalate " " $
        map shellQuote (server.mcpCommand : server.mcpArgs)

shellQuote :: Text -> Text
shellQuote value
    | not (Text.null value)
    , Text.all safe value = value
    | otherwise =
        "'" <> Text.replace "'" "'\\''" value <> "'"
  where
    safe char =
        isAlphaNum char || char `elem` ("-._/:@+=," :: String)

warningsFor :: Text -> [Text] -> [Text]
warningsFor label =
    filter (Text.isPrefixOf ("MCP server " <> label <> " "))

failureSummary :: Text -> Text
failureSummary warning =
    let marker = " failed to start: "
        (_, suffix) = Text.breakOn marker warning
    in if Text.null suffix
        then warning
        else Text.strip (Text.drop (Text.length marker) suffix)

warningSummary :: Text -> Text
warningSummary warning =
    let marker = " skipped "
        (_, suffix) = Text.breakOn marker warning
    in if Text.null suffix
        then warning
        else "skipped " <> Text.strip (Text.drop (Text.length marker) suffix)

selectedEntry :: McpManagerState -> Maybe McpEntry
selectedEntry state =
    case drop state.mcpManagerIndex state.mcpManagerEntries of
        entry : _ -> Just entry
        [] -> Nothing

moveSelection :: Int -> McpManagerState -> McpManagerState
moveSelection delta state
    | count == 0 = state { mcpManagerIndex = 0 }
    | otherwise =
        state
            { mcpManagerIndex =
                (state.mcpManagerIndex + delta) `mod` count
            }
  where
    count = length state.mcpManagerEntries

promptLine :: Text -> IO (Maybe Text)
promptLine prompt = do
    Text.hPutStr stderr prompt
    hFlush stderr
    eof <- isEOF
    if eof then pure Nothing else Just <$> Text.getLine

confirm :: Text -> IO Bool
confirm prompt =
    readApprovalLine prompt >>= \case
        Just answer ->
            pure (Text.toLower (Text.strip answer) `elem` ["y", "yes"])
        Nothing -> pure False

parseWords :: String -> Either Text [String]
parseWords = go WordUnquoted False False [] []
  where
    go quote escaped started current completed = \case
        []
            | escaped -> Left "MCP command ends with an incomplete escape"
            | quote /= WordUnquoted ->
                Left "MCP command contains an unterminated quote"
            | started -> Right (reverse (reverse current : completed))
            | otherwise -> Right (reverse completed)
        char : rest
            | escaped ->
                go quote False True (char : current) completed rest
            | quote == WordSingle ->
                if char == '\''
                    then go WordUnquoted False True current completed rest
                    else go quote False True (char : current) completed rest
            | quote == WordDouble ->
                case char of
                    '"' -> go WordUnquoted False True current completed rest
                    '\\' -> go quote True True current completed rest
                    _ -> go quote False True (char : current) completed rest
            | isSpace char ->
                if started
                    then
                        go WordUnquoted False False []
                            (reverse current : completed) rest
                    else go WordUnquoted False False [] completed rest
            | otherwise -> case char of
                '\'' -> go WordSingle False True current completed rest
                '"' -> go WordDouble False True current completed rest
                '\\' -> go WordUnquoted True True current completed rest
                _ -> go WordUnquoted False True (char : current) completed rest

data WordQuote
    = WordUnquoted
    | WordSingle
    | WordDouble
    deriving (Eq)

firstLine :: Text -> Text
firstLine = Text.strip . Text.takeWhile (/= '\n')

truncateText :: Int -> Text -> Text
truncateText limit value
    | Text.length value <= limit = value
    | otherwise = Text.take (max 0 (limit - 1)) value <> "…"

plural :: Int -> Text -> Text
plural count noun = noun <> if count == 1 then "" else "s"

unlessEmpty :: [a] -> [b] -> [b]
unlessEmpty values output
    | null values = []
    | otherwise = output

quote :: Text -> Text
quote value = "'" <> value <> "'"
