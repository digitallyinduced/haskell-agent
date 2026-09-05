-- | Meta Console planning, approval, and host-side action execution.
module Agent.CLI.Runtime.Repl.MetaConsole
    ( handleMetaConsoleRequest
    ) where

import Agent.CLI.Command
    ( ReplAction(ReplSetEffort, ReplToggleFast, ReplSetModel,
                 ReplSetShell, ReplToggleComputerUse, ReplSetComputerUse,
                 ReplToggleAlwaysApprove, ReplSetAgentLimit,
                 ReplEnableCodeMode, ReplSkills)
    , SlashCatalog
    , parseReplLineWithCatalog
    )
import Agent.CLI.Config
    ( HarnessConfig(..)
    , McpServerConfig(..)
    , loadHarnessConfig
    , updateHarnessConfig
    )
import Agent.CLI.Input ( readApprovalLine )
import Agent.CLI.Login ( connectProviderAccount )
import Agent.CLI.McpOAuth ( loginMcp )
import Agent.CLI.Options ( ApprovalPolicy(..) )
import Agent.CLI.Render ( clearThinking, putTextLn, renderEvent )
import Agent.CLI.Runtime.MetaConsole
    ( MetaSecretValue(..)
    , applyMetaConfigActions
    , buildMetaContext
    , isMetaConfigAction
    , metaConfigRequiresRestart
    , runMetaPlanner
    )
import Agent.CLI.Runtime.Repl.Context
    ( ReplHandlerContext(..)
    , displayReplError
    , displayReplInfo
    , withReplSuspended
    )
import Agent.CLI.Runtime.Repl.Selection ( selectRequestedAccount )
import Agent.CLI.Runtime.Types
    ( RunResult(RunQuit, RunRestart) )
import Agent.CLI.Secret ( promptSecretLine )
import Agent.CLI.Session
    ( Persistence(..)
    , SessionHandle(sessionMeta)
    , SessionMeta(metaId)
    , ensureSession
    )
import Agent.CLI.SessionEnv ( SessionEnv(..) )
import Agent.CLI.Style
    ( glyphOk, glyphSession, roleError, roleMuted, roleSuccess )
import Agent.CLI.TUI.App
    ( emitUiEvent
    , requestFullscreenChoiceWithBody
    , requestFullscreenSecret
    , requestFullscreenText
    )
import Agent.CLI.Terminal ( resolveColor )
import Agent.Loop ( LoopEvent(ActivityUpdated) )
import Agent.Provider ( providerSlug )
import Agent.TUI.Model
    ( UiEvent(UiSetNotice, UiSystemMessage)
    , progressNotice
    )
import Control.Exception.Safe
    ( displayException, finally, tryAny )
import Control.Monad ( foldM )
import Data.Aeson ( Value )
import Data.IORef ( readIORef )
import Data.Text ( Text )
import System.IO ( stderr )
import qualified Agent.CLI.MetaConsole as Meta
    ( MetaAction(..)
    , MetaPlan(..)
    , formatMetaError
    , metaPlanMutates
    , metaPlanPreviews
    )
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
    ( intercalate, map, null, pack, strip, toLower )
import qualified Data.Text.IO as Text ( hPutStrLn )

data MetaConsoleRuntime = MetaConsoleRuntime
    { metaReplContext :: !ReplHandlerContext
    , metaSlashCatalog :: !SlashCatalog
    , metaExecuteCommand :: !(Text -> IO RunResult)
    }

handleMetaConsoleRequest
    :: ReplHandlerContext
    -> SlashCatalog
    -> (Text -> IO RunResult)
    -> Text
    -> IO RunResult
handleMetaConsoleRequest replContext slashCatalog executeCommand rawRequest
    | Text.null request =
        metaFailure runtime "Meta Console request must not be empty"
    | otherwise =
        loadHarnessConfig env.sessionHome >>= \case
            Left err -> metaFailure runtime err
            Right config -> do
                plannerContext <- buildMetaContext env config
                obtainMetaPlan runtime plannerContext request 0 >>= \case
                    Left err -> metaFailure runtime err
                    Right Nothing -> do
                        displayMetaNotice runtime "Meta Console cancelled"
                        metaContinue runtime
                    Right (Just plan) -> do
                        approved <- approveMetaPlan runtime plan
                        if approved
                            then applyMetaPlan runtime config plan
                            else do
                                displayMetaNotice
                                    runtime
                                    "Meta Console changes cancelled"
                                metaContinue runtime
  where
    env = metaEnv runtime
    request = Text.strip rawRequest
    runtime =
        MetaConsoleRuntime
            { metaReplContext = replContext
            , metaSlashCatalog = slashCatalog
            , metaExecuteCommand = executeCommand
            }

obtainMetaPlan
    :: MetaConsoleRuntime
    -> Value
    -> Text
    -> Int
    -> IO (Either Text (Maybe Meta.MetaPlan))
obtainMetaPlan runtime plannerContext original clarificationCount =
    withMetaActivity runtime "Meta Console · interpreting…" $
        runMetaPlanner (metaEnv runtime) plannerContext original >>= \case
            Left err ->
                pure (Left (Meta.formatMetaError err))
            Right plan ->
                case plan.metaActions of
                    [Meta.MetaClarify question]
                        | clarificationCount >= 2 ->
                            pure
                                (Left
                                    "Meta Console still needs clarification after two replies")
                        | otherwise ->
                            askMetaClarification runtime question >>= \case
                                Nothing -> pure (Right Nothing)
                                Just answer ->
                                    obtainMetaPlan
                                        runtime
                                        plannerContext
                                        (original
                                            <> "\n\nClarification question: "
                                            <> question
                                            <> "\nClarification answer: "
                                            <> answer)
                                        (clarificationCount + 1)
                    _ -> pure (Right (Just plan))

askMetaClarification
    :: MetaConsoleRuntime
    -> Text
    -> IO (Maybe Text)
askMetaClarification runtime question =
    case (metaEnv runtime).sessionFullscreen of
        Just fullscreen ->
            requestFullscreenText
                fullscreen
                "Meta Console clarification"
                question
                ""
        Nothing ->
            readApprovalLine
                ("\nMeta Console needs clarification:\n"
                    <> safeMetaText question
                    <> "\nanswer> ")

approveMetaPlan
    :: MetaConsoleRuntime
    -> Meta.MetaPlan
    -> IO Bool
approveMetaPlan runtime plan
    | not (Meta.metaPlanMutates plan) = pure True
    | otherwise =
        readIORef (metaEnv runtime).sessionPolicy >>= \case
            ApproveAll -> do
                showMetaPreview runtime "Meta Console will apply" plan
                pure True
            DenyMutating -> do
                displayMetaError
                    runtime
                    "Meta Console changes are blocked by the current deny-mutations policy"
                pure False
            PromptMutating ->
                case (metaEnv runtime).sessionFullscreen of
                    Just fullscreen ->
                        requestFullscreenChoiceWithBody
                            fullscreen
                            "Apply Meta Console changes?"
                            (metaPreviewBody plan)
                            0
                            [ ( "Apply changes"
                              , "Execute only the typed actions shown above"
                              )
                            , ( "Cancel"
                              , "Leave configuration unchanged"
                              )
                            ]
                            >>= pure . (== Just 0)
                    Nothing -> do
                        showMetaPreview runtime "Meta Console proposes" plan
                        readApprovalLine
                            "Apply these changes? [y/N] "
                            >>= pure . maybe False isYes

showMetaPreview
    :: MetaConsoleRuntime
    -> Text
    -> Meta.MetaPlan
    -> IO ()
showMetaPreview runtime heading plan =
    displayReplInfo
        runtime.metaReplContext
        (heading <> "\n" <> metaPreviewBody plan)
        do
            color <- resolveColor stderr
            Text.hPutStrLn stderr
                (roleMuted color
                    (heading <> "\n" <> metaPreviewBody plan))

metaPreviewBody :: Meta.MetaPlan -> Text
metaPreviewBody plan =
    safeMetaText plan.metaSummary
        <> "\n"
        <> Text.intercalate
            "\n"
            [ Text.pack (show index)
                <> ". "
                <> safeMetaText preview
            | (index, preview) <-
                zip [(1 :: Int) ..] (Meta.metaPlanPreviews plan)
            ]

applyMetaPlan
    :: MetaConsoleRuntime
    -> HarnessConfig
    -> Meta.MetaPlan
    -> IO RunResult
applyMetaPlan runtime initial plan =
    collectMetaSecrets runtime plan.metaActions >>= \case
        Left err -> metaFailure runtime err
        Right secrets -> do
            configResult <-
                if any isMetaConfigAction plan.metaActions
                    then
                        updateHarnessConfig
                            (metaEnv runtime).sessionHome
                            (applyMetaConfigActions
                                secrets
                                plan.metaActions)
                    else pure (Right initial)
            case configResult of
                Left err -> metaFailure runtime err
                Right appliedConfig ->
                    executeMetaHostActions
                        runtime
                        appliedConfig
                        plan.metaActions
                        >>= \case
                            Left err -> metaFailure runtime err
                            Right terminalResult -> do
                                let success =
                                        (if Meta.metaPlanMutates plan
                                            then "Meta Console applied\n"
                                            else "Meta Console\n")
                                            <> metaPreviewBody plan
                                displayMetaSuccess runtime success
                                case terminalResult of
                                    Just result -> pure result
                                    Nothing
                                        | metaPlanNeedsRestart
                                            plan.metaActions ->
                                            requestMetaRestart runtime
                                        | otherwise ->
                                            metaContinue runtime

collectMetaSecrets
    :: MetaConsoleRuntime
    -> [Meta.MetaAction]
    -> IO (Either Text [MetaSecretValue])
collectMetaSecrets runtime =
    foldM (collectOneMetaSecret runtime) (Right [])

collectOneMetaSecret
    :: MetaConsoleRuntime
    -> Either Text [MetaSecretValue]
    -> Meta.MetaAction
    -> IO (Either Text [MetaSecretValue])
collectOneMetaSecret _ result@(Left _) _ = pure result
collectOneMetaSecret runtime (Right values) action = case action of
    Meta.MetaSetMcpSecretEnv server key ->
        promptMetaSecret
            runtime
            ("MCP " <> server <> " · " <> key)
            ("Enter the value for environment variable "
                <> key
                <> " on MCP server "
                <> server
                <> ". It stays local and is never sent to the model.")
            >>= \case
                Nothing ->
                    pure
                        (Left
                            ("secret input for MCP server '"
                                <> server
                                <> "' was cancelled"))
                Just value ->
                    pure
                        (Right
                            (values
                                <> [ MetaMcpSecretValue
                                        server key value
                                   ]))
    Meta.MetaSetLspSecretEnv server key ->
        promptMetaSecret
            runtime
            ("LSP " <> server <> " · " <> key)
            ("Enter the value for environment variable "
                <> key
                <> " on LSP server "
                <> server
                <> ". It stays local and is never sent to the model.")
            >>= \case
                Nothing ->
                    pure
                        (Left
                            ("secret input for LSP server '"
                                <> server
                                <> "' was cancelled"))
                Just value ->
                    pure
                        (Right
                            (values
                                <> [ MetaLspSecretValue
                                        server key value
                                   ]))
    _ -> pure (Right values)

promptMetaSecret
    :: MetaConsoleRuntime
    -> Text
    -> Text
    -> IO (Maybe Text)
promptMetaSecret runtime title body =
    case (metaEnv runtime).sessionFullscreen of
        Just fullscreen ->
            requestFullscreenSecret fullscreen title body
        Nothing ->
            promptSecretLine
                (metaEnv runtime).sessionStdinControl
                body
                (Just
                    "Meta Console configuration; the value is written only to the local config file")

executeMetaHostActions
    :: MetaConsoleRuntime
    -> HarnessConfig
    -> [Meta.MetaAction]
    -> IO (Either Text (Maybe RunResult))
executeMetaHostActions runtime config =
    foldM
        (executeOneMetaHostAction runtime config)
        (Right Nothing)

executeOneMetaHostAction
    :: MetaConsoleRuntime
    -> HarnessConfig
    -> Either Text (Maybe RunResult)
    -> Meta.MetaAction
    -> IO (Either Text (Maybe RunResult))
executeOneMetaHostAction _ _ result@(Left _) _ = pure result
executeOneMetaHostAction _ _ result@(Right (Just _)) _ = pure result
executeOneMetaHostAction runtime config (Right Nothing) action = case action of
    Meta.MetaConnectAccount requestedProvider -> do
        color <- resolveColor stderr
        tryAny
            (withReplSuspended
                runtime.metaReplContext
                (connectProviderAccount color requestedProvider))
            >>= \case
                Left err ->
                    pure
                        (Left
                            ("Could not connect "
                                <> providerSlug requestedProvider
                                <> ": "
                                <> Text.pack (displayException err)))
                Right Nothing ->
                    pure
                        (Left
                            ("Connecting "
                                <> providerSlug requestedProvider
                                <> " was cancelled or did not complete"))
                Right (Just _) -> pure (Right Nothing)
    Meta.MetaSelectAccount requestedProvider selector ->
        selectRequestedAccount
            (metaEnv runtime)
            requestedProvider
            selector
    Meta.MetaLoginMcpOAuth name ->
        case Map.lookup name config.configMcpServers >>= (.mcpUrl) of
            Nothing ->
                pure
                    (Left
                        ("Remote MCP server '"
                            <> name
                            <> "' is not configured"))
            Just url ->
                tryAny
                    (withReplSuspended
                        runtime.metaReplContext
                        (loginMcp url))
                    >>= \case
                        Left _ ->
                            pure
                                (Left
                                    "MCP OAuth login failed; the login flow did not complete")
                        Right () -> pure (Right Nothing)
    Meta.MetaSessionCommand command ->
        runMetaSessionCommand runtime command
    Meta.MetaInform _ -> pure (Right Nothing)
    _ -> pure (Right Nothing)

runMetaSessionCommand
    :: MetaConsoleRuntime
    -> Text
    -> IO (Either Text (Maybe RunResult))
runMetaSessionCommand runtime command =
    case parseReplLineWithCatalog runtime.metaSlashCatalog command of
        action
            | safeMetaSessionAction action -> do
                result <- runtime.metaExecuteCommand command
                pure $
                    Right case result of
                        RunQuit -> Nothing
                        terminalResult -> Just terminalResult
        _ ->
            pure
                (Left
                    ("Meta Console rejected unsupported session command: "
                        <> command))

safeMetaSessionAction :: ReplAction -> Bool
safeMetaSessionAction = \case
    ReplSetEffort{} -> True
    ReplToggleFast -> True
    ReplSetModel{} -> True
    ReplSetShell{} -> True
    ReplToggleComputerUse -> True
    ReplSetComputerUse{} -> True
    ReplToggleAlwaysApprove -> True
    ReplSetAgentLimit{} -> True
    ReplEnableCodeMode -> True
    ReplSkills True -> True
    _ -> False

metaPlanNeedsRestart :: [Meta.MetaAction] -> Bool
metaPlanNeedsRestart actions =
    metaConfigRequiresRestart actions
        || any
            (\case
                Meta.MetaLoginMcpOAuth{} -> True
                _ -> False)
            actions

requestMetaRestart :: MetaConsoleRuntime -> IO RunResult
requestMetaRestart runtime = do
    color <- resolveColor stderr
    let report message =
            case env.sessionFullscreen of
                Nothing ->
                    putTextLn stderr
                        (roleMuted color (glyphSession <> message))
                Just fullscreen ->
                    emitUiEvent fullscreen (UiSystemMessage message)
    case env.sessionPersist of
        PersistenceDisabled -> do
            report
                "restart the agent to apply Meta Console changes"
            pure RunQuit
        PersistenceEnabled slotRef -> do
            handle <- ensureSession slotRef
            report "restarting to apply Meta Console changes…"
            pure (RunRestart handle.sessionMeta.metaId)
  where
    env = metaEnv runtime

displayMetaNotice :: MetaConsoleRuntime -> Text -> IO ()
displayMetaNotice runtime message =
    displayReplInfo runtime.metaReplContext message do
        color <- resolveColor stderr
        Text.hPutStrLn stderr (roleMuted color message)

displayMetaSuccess :: MetaConsoleRuntime -> Text -> IO ()
displayMetaSuccess runtime message =
    displayReplInfo runtime.metaReplContext message do
        color <- resolveColor stderr
        Text.hPutStrLn stderr
            (roleSuccess color (glyphOk <> message))

displayMetaError :: MetaConsoleRuntime -> Text -> IO ()
displayMetaError runtime message =
    displayReplError runtime.metaReplContext message do
        color <- resolveColor stderr
        Text.hPutStrLn stderr (roleError color message)

metaFailure :: MetaConsoleRuntime -> Text -> IO RunResult
metaFailure runtime err = do
    let safeError = safeMetaText err
    displayMetaError runtime safeError
    metaContinue runtime

metaContinue :: MetaConsoleRuntime -> IO RunResult
metaContinue runtime =
    runtime.metaReplContext.handlerContinueWith ""

metaEnv :: MetaConsoleRuntime -> SessionEnv
metaEnv runtime =
    runtime.metaReplContext.handlerSessionEnv

withMetaActivity
    :: MetaConsoleRuntime
    -> Text
    -> IO a
    -> IO a
withMetaActivity runtime message action = do
    case env.sessionFullscreen of
        Nothing ->
            renderEvent env.sessionRender (ActivityUpdated message)
        Just fullscreen ->
            emitUiEvent fullscreen
                (UiSetNotice (Just (progressNotice message)))
    action `finally`
        case env.sessionFullscreen of
            Nothing -> clearThinking env.sessionRender
            Just fullscreen ->
                emitUiEvent fullscreen (UiSetNotice Nothing)
  where
    env = metaEnv runtime

isYes :: Text -> Bool
isYes =
    (`elem` ["y", "yes"])
        . Text.toLower
        . Text.strip

safeMetaText :: Text -> Text
safeMetaText =
    Text.map
        (\character ->
            if character < ' ' && character `notElem` ['\n', '\t']
                then ' '
                else character)
