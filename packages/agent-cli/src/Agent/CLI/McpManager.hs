-- | Interactive management of local stdio and remote HTTP MCP servers.
module Agent.CLI.McpManager
    ( McpAddField(..)
    , McpAddForm(..)
    , McpEntry(..)
    , McpEntryStatus(..)
    , McpManagerAction(..)
    , McpManagerState(..)
    , applyMcpManagerKey
    , authorizedMcpUrls
    , decodeMcpManagerKey
    , emptyMcpAddForm
    , initialMcpManagerState
    , mcpEntryTransport
    , parseMcpCommand
    , renderMcpManagerFrame
    , resolveMcpAddForm
    , runMcpManager
    , submitMcpAddForm
    , suggestMcpName
    ) where

import Agent.CLI.Config
    ( HarnessConfig(..)
    , McpServerConfig(..)
    , loadHarnessConfigSnapshot
    , modifyHarnessConfig
    )
import Agent.CLI.McpAdd
    ( mcpServerForTarget
    , parseMcpAddName
    , parseMcpCommand
    , parseMcpTarget
    , suggestMcpName
    , suggestMcpTargetName
    )
import Agent.CLI.McpOAuth
    ( defaultLoginOptions
    , loginMcpWithResult
    )
import Agent.CLI.McpOAuthStore (mcpOAuthStorePath)
import Agent.CLI.Picker
    ( PickerKey(..)
    , decodePickerKey
    , runOverlayWithDecoder
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
import Data.Char (isAlphaNum, isPrint)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory.OsPath (doesFileExist)
import System.IO
    ( hFlush
    , hIsTerminalDevice
    , stderr
    , stdin
    )
import System.OsPath (OsPath)

data McpEntryStatus
    = McpDisabled
    | McpPendingRestart
    | McpNeedsAuth
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

data McpAddField
    = McpAddTargetField
    | McpAddNameField
    deriving (Eq, Show)

data McpAddForm = McpAddForm
    { mcpAddTarget :: !Text
    , mcpAddTargetCursor :: !Int
    , mcpAddName :: !Text
    , mcpAddNameCursor :: !Int
    , mcpAddFocus :: !McpAddField
    }
    deriving (Eq, Show)

emptyMcpAddForm :: McpAddForm
emptyMcpAddForm =
    McpAddForm
        { mcpAddTarget = ""
        , mcpAddTargetCursor = 0
        , mcpAddName = ""
        , mcpAddNameCursor = 0
        , mcpAddFocus = McpAddTargetField
        }

data McpManagerState = McpManagerState
    { mcpManagerEntries :: ![McpEntry]
    , mcpManagerIndex :: !Int
    , mcpManagerExpanded :: !(Maybe Text)
    , mcpManagerRestartPending :: !Bool
    , mcpManagerNotice :: !(Maybe (Bool, Text))
    , mcpManagerAddForm :: !(Maybe McpAddForm)
    , mcpManagerConfirmRemove :: !(Maybe Text)
    }
    deriving (Eq, Show)

data McpManagerAction
    = McpManagerClose
    | McpManagerRestart
    | McpManagerSubmitAdd !Text !McpServerConfig
    | McpManagerToggle !Text
    | McpManagerRemove !Text
    | McpManagerAuth !Text
    deriving (Eq, Show)

initialMcpManagerState
    :: HarnessConfig
    -> [McpToolRegistration]
    -> [Text]
    -> Set Text
    -> Set Text
    -> Maybe (Bool, Text)
    -> McpManagerState
initialMcpManagerState config registrations warnings pending authorized notice =
    McpManagerState
        { mcpManagerEntries =
            [ entryFor label server
            | (label, server) <- Map.toAscList config.configMcpServers
            ]
        , mcpManagerIndex = 0
        , mcpManagerExpanded = Nothing
        , mcpManagerRestartPending = not (Set.null pending)
        , mcpManagerNotice = notice
        , mcpManagerAddForm = Nothing
        , mcpManagerConfirmRemove = Nothing
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
                | needsAuthorization server failed = McpNeedsAuth
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
    needsAuthorization server failed = case server.mcpUrl of
        Nothing -> False
        Just url ->
            maybe False isAuthFailure failed
                || (Set.notMember url authorized && isJust failed)

authorizedMcpUrls :: OsPath -> HarnessConfig -> IO (Set Text)
authorizedMcpUrls home config =
    Set.fromList <$> foldMap hasToken (Map.elems config.configMcpServers)
  where
    hasToken server = case server.mcpUrl of
        Nothing -> pure []
        Just url -> do
            exists <- doesFileExist (mcpOAuthStorePath home url)
            pure (if exists then [url] else [])

decodeMcpManagerKey :: String -> Maybe PickerKey
decodeMcpManagerKey = \case
    "q" -> Just (PickerKeyChar 'q')
    "Q" -> Just (PickerKeyChar 'Q')
    "j" -> Just (PickerKeyChar 'j')
    "J" -> Just (PickerKeyChar 'J')
    "k" -> Just (PickerKeyChar 'k')
    "K" -> Just (PickerKeyChar 'K')
    raw -> decodePickerKey raw

applyMcpManagerKey
    :: PickerKey
    -> McpManagerState
    -> Either McpManagerAction McpManagerState
applyMcpManagerKey key state =
    case state.mcpManagerAddForm of
        Just form -> applyAddFormKey key state form
        Nothing ->
            case state.mcpManagerConfirmRemove of
                Just name -> applyConfirmRemoveKey key state name
                Nothing -> applyBrowseKey key state

applyBrowseKey
    :: PickerKey
    -> McpManagerState
    -> Either McpManagerAction McpManagerState
applyBrowseKey key state = case key of
    PickerKeyCancel -> Left McpManagerClose
    PickerKeyChar 'q' -> Left McpManagerClose
    PickerKeyChar 'Q' -> Left McpManagerClose
    PickerKeyConfirm ->
        case selectedEntry state of
            Nothing -> Right state
            Just entry ->
                Right state
                    { mcpManagerExpanded =
                        if state.mcpManagerExpanded == Just entry.mcpEntryName
                            then Nothing
                            else Just entry.mcpEntryName
                    , mcpManagerNotice = Nothing
                    }
    PickerKeyChar 'r' -> Left McpManagerRestart
    PickerKeyChar 'R' -> Left McpManagerRestart
    PickerKeyChar 'a' -> openAddForm
    PickerKeyChar 'A' -> openAddForm
    PickerKeyChar ' ' -> selectedAction McpManagerToggle
    PickerKeyChar 'e' -> selectedAction McpManagerToggle
    PickerKeyChar 'E' -> selectedAction McpManagerToggle
    PickerKeyChar 'x' -> confirmSelected
    PickerKeyChar 'X' -> confirmSelected
    PickerKeyChar 'i' -> selectedAction McpManagerAuth
    PickerKeyChar 'I' -> selectedAction McpManagerAuth
    PickerKeyUp -> Right (moveSelection (-1) state)
    PickerKeyChar 'k' -> Right (moveSelection (-1) state)
    PickerKeyChar 'K' -> Right (moveSelection (-1) state)
    PickerKeyDown -> Right (moveSelection 1 state)
    PickerKeyChar 'j' -> Right (moveSelection 1 state)
    PickerKeyChar 'J' -> Right (moveSelection 1 state)
    _ -> Right state
  where
    openAddForm =
        Right state
            { mcpManagerAddForm = Just emptyMcpAddForm
            , mcpManagerConfirmRemove = Nothing
            , mcpManagerNotice = Nothing
            }
    confirmSelected =
        maybe (Right state)
            (\entry ->
                Right state
                    { mcpManagerConfirmRemove = Just entry.mcpEntryName
                    , mcpManagerNotice = Nothing
                    })
            (selectedEntry state)
    selectedAction constructor =
        maybe (Right state) (Left . constructor . (.mcpEntryName))
            (selectedEntry state)

applyConfirmRemoveKey
    :: PickerKey
    -> McpManagerState
    -> Text
    -> Either McpManagerAction McpManagerState
applyConfirmRemoveKey key state name = case key of
    PickerKeyChar 'y' -> Left (McpManagerRemove name)
    _ ->
        Right state
            { mcpManagerConfirmRemove = Nothing
            , mcpManagerNotice = Nothing
            }

applyAddFormKey
    :: PickerKey
    -> McpManagerState
    -> McpAddForm
    -> Either McpManagerAction McpManagerState
applyAddFormKey key state form = case key of
    PickerKeyCancel ->
        Right state
            { mcpManagerAddForm = Nothing
            , mcpManagerNotice = Nothing
            }
    PickerKeyTab -> Right (setForm (switchAddField 1 form))
    PickerKeyBackTab -> Right (setForm (switchAddField (-1) form))
    PickerKeyUp -> Right (setForm (form { mcpAddFocus = McpAddTargetField }))
    PickerKeyDown -> Right (setForm (form { mcpAddFocus = McpAddNameField }))
    PickerKeyLeft -> Right (setForm (moveAddCursor (-1) form))
    PickerKeyRight -> Right (setForm (moveAddCursor 1 form))
    PickerKeyBackspace -> Right (setForm (deleteAddChar form))
    PickerKeyConfirm ->
        case submitMcpAddForm existingNames form of
            Left err ->
                Right state
                    { mcpManagerAddForm = Just form
                    , mcpManagerNotice = Just (False, err)
                    }
            Right (label, server) -> Left (McpManagerSubmitAdd label server)
    PickerKeyChar char
        | isPrint char -> Right (setForm (insertAddChar char form))
        | otherwise -> Right state
  where
    existingNames = Set.fromList (map (.mcpEntryName) state.mcpManagerEntries)
    setForm next =
        state
            { mcpManagerAddForm = Just next
            , mcpManagerNotice = Nothing
            }

submitMcpAddForm
    :: Set Text
    -> McpAddForm
    -> Either Text (Text, McpServerConfig)
submitMcpAddForm existing form = do
    (label, server) <- resolveMcpAddForm form
    if Set.member label existing
        then Left ("MCP server " <> quote label <> " already exists")
        else Right (label, server)

resolveMcpAddForm :: McpAddForm -> Either Text (Text, McpServerConfig)
resolveMcpAddForm form = do
    target <- parseMcpTarget form.mcpAddTarget
    let suggested = suggestMcpTargetName target
    label <-
        if Text.null (Text.strip form.mcpAddName)
            then parseMcpAddName suggested
            else parseMcpAddName form.mcpAddName
    pure (label, mcpServerForTarget target)

renderMcpManagerFrame :: Bool -> McpManagerState -> Text
renderMcpManagerFrame color state =
    case state.mcpManagerAddForm of
        Just form -> renderAddForm color state form
        Nothing -> renderBrowseFrame color state

renderBrowseFrame :: Bool -> McpManagerState -> Text
renderBrowseFrame color state =
    Text.intercalate "\n" $
        [ rolePrompt color "MCP servers"
            <> roleMuted color
                (" · " <> Text.pack (show (length state.mcpManagerEntries)))
            <> if state.mcpManagerRestartPending
                then roleWarn color " · restart pending"
                else ""
        ]
            <> maybe [] renderNotice state.mcpManagerNotice
            <> maybe [] (renderRemoveConfirm color) state.mcpManagerConfirmRemove
            <> body
            <> [ roleMuted color
                    "↑↓/jk · enter details · a add · i auth · space enable/disable · x remove"
               , roleMuted color
                    "r restart/refresh · esc/q close"
               ]
  where
    renderNotice (success, message) =
        [ (if success then roleSuccess else roleError) color message ]
    body = case state.mcpManagerEntries of
        [] ->
            [ roleWarn color "No MCP servers configured."
            , roleMuted color
                "Press a to add a remote URL or a local stdio command."
            ]
        entries ->
            concat $
                zipWith (renderEntry color state) [0 ..] entries

renderRemoveConfirm :: Bool -> Text -> [Text]
renderRemoveConfirm color name =
    [ roleWarn color
        ("Remove " <> quote name <> "? Press y to confirm, any other key to cancel")
    ]

renderAddForm :: Bool -> McpManagerState -> McpAddForm -> Text
renderAddForm color state form =
    Text.intercalate "\n" $
        [ rolePrompt color "Add MCP server"
        , roleMuted color
            "Paste a remote http(s) URL or a local stdio command."
        ]
            <> maybe [] renderNotice state.mcpManagerNotice
            <> [ ""
               , fieldLabel McpAddTargetField "URL / Command"
               , fieldBox McpAddTargetField form.mcpAddTarget form.mcpAddTargetCursor Nothing
               , ""
               , fieldLabel McpAddNameField "Name"
               , fieldBox McpAddNameField form.mcpAddName form.mcpAddNameCursor
                    (Just namePlaceholder)
               , ""
               , roleMuted color
                    "Enter submit  |  Tab/Shift+Tab field  |  Esc cancel"
               ]
  where
    renderNotice (success, message) =
        [ (if success then roleSuccess else roleError) color message ]
    focused = form.mcpAddFocus
    namePlaceholder
        | isHttpTarget form.mcpAddTarget = "Auto generated by URL"
        | Text.null (Text.strip form.mcpAddTarget) = "Auto generated"
        | otherwise = "Auto generated by command"
    fieldLabel field title =
        (if focused == field then rolePrompt else roleMuted) color title
    fieldBox field value cursor placeholder =
        let shown
                | Text.null value
                , Just hint <- placeholder =
                    if focused == field
                        then rolePrompt color "▍" <> roleMuted color hint
                        else roleMuted color hint
                | otherwise =
                    insertCursor color (focused == field) value cursor
        in (if focused == field then rolePrompt else roleMuted) color "❯ "
            <> shown

isHttpTarget :: Text -> Bool
isHttpTarget input =
    let lower = Text.toLower (Text.strip input)
    in "https://" `Text.isPrefixOf` lower
        || "http://" `Text.isPrefixOf` lower

insertCursor :: Bool -> Bool -> Text -> Int -> Text
insertCursor color focused value cursor
    | not focused = value
    | otherwise =
        let bounded = max 0 (min (Text.length value) cursor)
            (before, after) = Text.splitAt bounded value
        in before <> rolePrompt color "▍" <> after

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
            authorized <- authorizedMcpUrls home config
            if not tty
                then do
                    Text.hPutStrLn stderr $
                        renderMcpManagerFrame color
                            (initialMcpManagerState
                                config registrations warnings Set.empty
                                authorized Nothing)
                    hFlush stderr
                    pure False
                else loop revision config Set.empty False authorized Nothing
  where
    loop revision config pending changed authorized notice = do
        let state =
                initialMcpManagerState
                    config registrations warnings pending authorized notice
        runOverlayWithDecoder
            decodeMcpManagerKey
            (renderMcpManagerFrame color)
            applyMcpManagerKey
            state
            >>= \case
                Nothing -> pure changed
                Just McpManagerClose -> pure changed
                Just McpManagerRestart -> pure True
                Just (McpManagerSubmitAdd label server) ->
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
                            loop revision config pending changed authorized
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
                Just (McpManagerRemove label) ->
                    persist
                        (config
                            { configMcpServers =
                                Map.delete label config.configMcpServers
                            })
                        (Set.insert label pending)
                        ("Removed " <> label)
                Just (McpManagerAuth label) ->
                    case Map.lookup label config.configMcpServers of
                        Nothing ->
                            loop revision config pending changed authorized
                                (Just (False, "MCP server no longer exists"))
                        Just server -> case server.mcpUrl of
                            Nothing ->
                                loop revision config pending changed authorized
                                    (Just
                                        ( False
                                        , label
                                            <> " is a local stdio server; OAuth is only used for HTTP servers"
                                        ))
                            Just url ->
                                loginMcpWithResult defaultLoginOptions url >>= \case
                                    Left err ->
                                        loop revision config pending changed authorized
                                            (Just (False, err))
                                    Right message -> do
                                        nextAuthorized <-
                                            authorizedMcpUrls home config
                                        loop revision config
                                            (Set.insert label pending)
                                            True
                                            nextAuthorized
                                            (Just (True, message))
      where
        persist updated pending' message =
            modifyHarnessConfig home
                (\current _ ->
                    if current /= revision
                        then Left
                            "MCP configuration changed; reopen /mcp and retry"
                        else Right (updated, ())) >>= \case
                Left err ->
                    loop revision config pending changed authorized
                        (Just (False, err))
                Right (nextRevision, _, ()) -> do
                    nextAuthorized <- authorizedMcpUrls home updated
                    loop nextRevision updated pending' True
                        nextAuthorized
                        (Just (True, message))

mcpEntryTransport :: McpEntry -> Text
mcpEntryTransport entry =
    case entry.mcpEntryConfig.mcpUrl of
        Just _ -> "http"
        Nothing -> "stdio"

renderEntry :: Bool -> McpManagerState -> Int -> McpEntry -> [Text]
renderEntry color state index entry =
    [ prefix
        <> enabledMarker
        <> " "
        <> entry.mcpEntryName
        <> " "
        <> renderStatus color entry.mcpEntryStatus
        <> roleMuted color (" · " <> mcpEntryTransport entry)
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
    McpNeedsAuth -> roleWarn color "[needs auth]"
    McpReady count ->
        roleSuccess color "[ready]"
            <> roleMuted color
                (" · " <> Text.pack (show count) <> plural count " tool")
    McpUnavailable reason ->
        roleError color "[unavailable]"
            <> roleMuted color (" · " <> truncateText 72 reason)

renderDetails :: Bool -> McpEntry -> [Text]
renderDetails color entry =
    [ roleMuted color ("    " <> targetLabel <> ": " <> targetValue)
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
    (targetLabel, targetValue) = case server.mcpUrl of
        Just url -> ("url", url)
        Nothing -> ("command", renderCommand server)
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

isAuthFailure :: Text -> Bool
isAuthFailure warning =
    let lower = Text.toLower warning
    in any (`Text.isInfixOf` lower)
        [ "oauth"
        , "unauthorized"
        , "401"
        , "403"
        , "www-authenticate"
        ]

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

switchAddField :: Int -> McpAddForm -> McpAddForm
switchAddField delta form =
    form
        { mcpAddFocus =
            if (fieldIndex form.mcpAddFocus + delta) `mod` 2 == 0
                then McpAddTargetField
                else McpAddNameField
        }
  where
    fieldIndex = \case
        McpAddTargetField -> 0
        McpAddNameField -> 1

moveAddCursor :: Int -> McpAddForm -> McpAddForm
moveAddCursor delta form =
    case form.mcpAddFocus of
        McpAddTargetField ->
            form { mcpAddTargetCursor = bound form.mcpAddTarget form.mcpAddTargetCursor }
        McpAddNameField ->
            form { mcpAddNameCursor = bound form.mcpAddName form.mcpAddNameCursor }
  where
    bound value cursor =
        max 0 (min (Text.length value) (cursor + delta))

insertAddChar :: Char -> McpAddForm -> McpAddForm
insertAddChar char form =
    case form.mcpAddFocus of
        McpAddTargetField ->
            let (next, cursor) = insertAt form.mcpAddTargetCursor char form.mcpAddTarget
            in form { mcpAddTarget = next, mcpAddTargetCursor = cursor }
        McpAddNameField ->
            let (next, cursor) = insertAt form.mcpAddNameCursor char form.mcpAddName
            in form { mcpAddName = next, mcpAddNameCursor = cursor }

deleteAddChar :: McpAddForm -> McpAddForm
deleteAddChar form =
    case form.mcpAddFocus of
        McpAddTargetField ->
            let (next, cursor) = deleteAt form.mcpAddTargetCursor form.mcpAddTarget
            in form { mcpAddTarget = next, mcpAddTargetCursor = cursor }
        McpAddNameField ->
            let (next, cursor) = deleteAt form.mcpAddNameCursor form.mcpAddName
            in form { mcpAddName = next, mcpAddNameCursor = cursor }

insertAt :: Int -> Char -> Text -> (Text, Int)
insertAt cursor char value =
    let bounded = max 0 (min (Text.length value) cursor)
        (before, after) = Text.splitAt bounded value
    in (before <> Text.singleton char <> after, bounded + 1)

deleteAt :: Int -> Text -> (Text, Int)
deleteAt cursor value
    | cursor <= 0 = (value, 0)
    | otherwise =
        let bounded = min (Text.length value) cursor
            (before, after) = Text.splitAt bounded value
        in (Text.dropEnd 1 before <> after, bounded - 1)

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
