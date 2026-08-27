-- | Session interaction helpers shared by turn handling and the REPL.
module Agent.CLI.Session.Interaction
    ( buildPromptState
    , runBtwQuestion
    , setSessionEffort
    , setSessionEffortText
    , syncFullscreenPrompt
    ) where

import Agent.CLI.Btw
    ( formatBtwError
    , runBtwWithCancel
    )
import Agent.CLI.CancelWatch (withEscCancel)
import Agent.CLI.Command
    ( currentEffort
    , currentModel
    , setReasoningEffort
    )
import Agent.CLI.Interrupt (withTurnCancel)
import Agent.CLI.Options
    ( ApprovalPolicy
    , normalizeReasoningEffortForDialect
    , reasoningEffortsForDialect
    )
import Agent.CLI.Render
    ( RenderConfig(..)
    , putTextLn
    , renderAssistantText
    )
import Agent.CLI.ReplMode
    ( replModeFromState
    , replModeLabel
    )
import Agent.CLI.Session
    ( Persistence(..)
    , PersistenceState(..)
    , SessionHandle(..)
    , SessionMeta(..)
    , SessionCreate(..)
    , writeSessionMeta
    )
import Agent.CLI.SessionEnv (SessionEnv(..))
import Agent.CLI.Session.History
    ( readLiveAttachments
    , readLiveTranscript
    )
import Agent.CLI.Style (roleError)
import Agent.CLI.Terminal (resolveColor)
import Agent.CLI.TUI.App
    ( emitUiEvent
    )
import Agent.Loop (TokenUsage)
import Agent.Dialect (DialectId, dialectId)
import Agent.ReasoningEffort
    ( ReasoningEffort
    , parseReasoningEffort
    , reasoningEffortText
    )
import Agent.Responses.Types (ResponseCreateParams)
import Agent.Tools.PlanMode
    ( PlanModeEnv(..)
    , PlanModeState
    )
import Agent.TUI.Model
    ( PromptState(..)
    , UiEvent(..)
    , progressNotice
    )
import Control.Monad (forM_)
import Data.IORef
    ( modifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text

-- | Publish the current session prompt metadata to a retained fullscreen
-- runtime before replaying a pending turn after a provider rebuild.
syncFullscreenPrompt :: SessionEnv -> IO ()
syncFullscreenPrompt env =
    forM_ env.sessionFullscreen \runtime -> do
        planState <- readIORef env.sessionPlanMode.planStateRef
        params <- readIORef env.sessionParams
        policy <- readIORef env.sessionPolicy
        account <- readIORef env.sessionAccount
        usage <- readIORef env.sessionUsage
        attachments <- readLiveAttachments env.sessionConversation
        emitUiEvent runtime $ UiSetPrompt $
            buildPromptState
                (dialectId env.sessionDialect)
                params
                planState
                policy
                account
                (isJust env.sessionSelectAccount)
                usage
                (length attachments)

buildPromptState
    :: DialectId
    -> ResponseCreateParams
    -> PlanModeState
    -> ApprovalPolicy
    -> Text
    -> Bool
    -> TokenUsage
    -> Int
    -> PromptState
buildPromptState activeDialect params planState policy account accountSelectable usage attachments =
    PromptState
        { promptModel = currentModel params
        , promptEffort =
            reasoningEffortText $
                normalizeReasoningEffortForDialect
                    activeDialect
                    (currentEffort params)
        , promptEffortOptions =
            map reasoningEffortText
                (reasoningEffortsForDialect activeDialect)
        , promptMode =
            replModeLabel (replModeFromState planState policy)
        , promptAccount = account
        , promptAccountSelectable = accountSelectable
        , promptUsage = usage
        , promptLimitStatus = Nothing
        , promptAttachments = attachments
        }

setSessionEffort :: SessionEnv -> ReasoningEffort -> IO ()
setSessionEffort env level = do
    modifyIORef' env.sessionParams (setReasoningEffort level)
    let levelText = reasoningEffortText level
    case env.sessionFullscreen of
        Just runtime ->
            emitUiEvent runtime (UiSetPromptEffort levelText)
        Nothing -> pure ()
    case env.sessionPersist of
        PersistenceDisabled -> pure ()
        PersistenceEnabled slotRef -> do
            slot <- readIORef slotRef
            case slot of
                PersistencePending pending sessionId tempDir ->
                    writeIORef slotRef
                        (PersistencePending
                            pending { createEffort = levelText }
                            sessionId
                            tempDir)
                PersistenceActive handle -> do
                    let meta =
                            handle.sessionMeta { metaEffort = levelText }
                    writeSessionMeta
                        handle.sessionPool
                        handle.sessionMetaPath
                        meta
                    writeIORef slotRef
                        (PersistenceActive handle { sessionMeta = meta })

setSessionEffortText :: SessionEnv -> Text -> IO ()
setSessionEffortText env level =
    case parseReasoningEffort level of
        Left err -> ioError (userError (Text.unpack err))
        Right effort -> setSessionEffort env effort

runBtwQuestion :: Bool -> SessionEnv -> Text -> IO ()
runBtwQuestion registerCancel env question = do
    let fullscreen = env.sessionFullscreen
        stdoutHandle = env.sessionRender.renderStdout
        stderrHandle = env.sessionRender.renderStderr
    color <- resolveColor stdoutHandle
    transcriptRef <- newIORef =<< readLiveTranscript env.sessionConversation
    forM_ fullscreen \runtime ->
        emitUiEvent runtime
            (UiSetNotice (Just (progressNotice "btw · asking…")))
    result <-
        runBtwWithCancel
            (\cancel action ->
                if registerCancel
                    then
                        withTurnCancel env.sessionInterrupt cancel $
                            case fullscreen of
                                Nothing
                                    | env.sessionBackground -> action
                                    | otherwise ->
                                        withEscCancel
                                            cancel
                                            env.sessionEscPaused
                                            action
                                Just _ -> action
                    else action)
            env.sessionBtwBackend
            env.sessionParams
            transcriptRef
            question
    forM_ fullscreen \runtime ->
        emitUiEvent runtime (UiSetNotice Nothing)
    case result of
        Left err -> do
            errorColor <- resolveColor stderrHandle
            let message = formatBtwError err
            case fullscreen of
                Just runtime ->
                    emitUiEvent runtime (UiErrorMessage message)
                Nothing ->
                    putTextLn stderrHandle (roleError errorColor message)
        Right answer ->
            case fullscreen of
                Just runtime ->
                    emitUiEvent runtime (UiAssistantHistory answer)
                Nothing ->
                    putTextLn stdoutHandle (renderAssistantText color answer)
