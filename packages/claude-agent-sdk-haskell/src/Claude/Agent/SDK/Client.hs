-- | Stateful, bidirectional client over a persistent Claude Code subprocess.
module Claude.Agent.SDK.Client
    ( ClaudeSDKClient
    , ClaudeSDKTurn
    , withClaudeSDKClient
    , withClaudeSDKClientWithHandlers
    , withClaudeSDKClientWithTransport
    , withClaudeSDKClientWithTransportAndHandlers
    , withClaudeSDKClientWithoutTools
    , withClaudeSDKTurn
    , sendQuery
    , sendQueryContent
    , receiveMessage
    , resolveTurnUsage
    , acceptTurnSessionId
    , acceptConversationReset
    , turnSessionId
    , turnIsNewSession
    , turnProcessExit
    , turnDiagnostic
    , turnStreamStartupTimeoutMicros
    , turnStreamInactivityTimeoutMicros
    , turnTimeoutMicros
    , initializationResult
    , sendControlRequest
    , interrupt
    , setPermissionMode
    , setModel
    , getContextUsage
    , stopTask
    , abort
    ) where

import Claude.Agent.SDK.Control
    ( ClaudeAgentHandlers(..)
    )
import Claude.Agent.SDK.Errors
    ( ClaudeSDKError(..)
    )
import Claude.Agent.SDK.Internal.MessageParser
    ( decodeMessageLine
    )
import Claude.Agent.SDK.Internal.ControlRuntime
    ( ControlRuntime
    , runtimeInitializationResult
    , runtimeReadMessage
    , runtimeSendRequest
    , runtimeWrite
    , startControlRuntime
    , stopControlRuntime
    )
import Claude.Agent.SDK.Internal.Client.Usage
    ( UsageAccounting(..)
    , cumulativeUsage
    , reconcileCumulativeUsage
    )
import Claude.Agent.SDK.Internal.Transport.SubprocessCLI
    ( newSubprocessCLITransport
    )
import Claude.Agent.SDK.Transport
    ( Transport(..)
    , TransportFactory
    , TransportMode(..)
    , TransportRequest(..)
    )
import Claude.Agent.SDK.Types
    ( ClaudeAgentOptions(..)
    , ConversationResetMessage(..)
    , Message
    , ModelUsage(..)
    , UserContentBlock(..)
    , Usage(..)
    , addUsage
    , emptyUsage
    )
import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
    ( Async
    , asyncWithUnmask
    , cancel
    , poll
    )
import Control.Concurrent.MVar
    ( MVar
    , newMVar
    , withMVar
    )
import Control.Exception.Safe
    ( SomeException
    , bracket
    , finally
    , fromException
    , mask
    , onException
    , throwIO
    , tryAny
    )
import Control.Monad (unless, when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Bits ((.&.), (.|.))
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.UUID.Types as UUID
import System.Entropy (getEntropy)
import System.Exit (ExitCode)

data ClaudeSDKClient = ClaudeSDKClient
    { clientOptions :: !ClaudeAgentOptions
    , clientTransportFactory :: !TransportFactory
    , clientHandlers :: !(Maybe ClaudeAgentHandlers)
    , clientInitialPrevious :: !(Maybe Text)
    , clientState :: !(IORef ClientState)
    , clientInterruptEpoch :: !(IORef Int)
    , clientCommitLock :: !(MVar ())
    , clientTurnLock :: !(MVar ())
    }

data ClientState = ClientState
    { stateRunning :: !(Maybe RunningClient)
    , stateHadCompletedTurn :: !Bool
    , stateNeedsFreshSession :: !Bool
    , stateInitialPreviousConsumed :: !Bool
    }

data RunningClient = RunningClient
    { runningSessionId :: !(IORef (Maybe Text))
    , runningStartMode :: !TransportMode
    , runningModel :: !(Maybe Text)
    , runningEffort :: !(Maybe Text)
    , runningTransport :: !Transport
    , runningControlRuntime :: !(Maybe ControlRuntime)
    , runningShutdownTimeoutMicros :: !Int
    , runningUsageAccounting :: !(IORef UsageAccounting)
    , runningClosed :: !(IORef Bool)
    , runningCloseLock :: !(MVar ())
    }

data ClaudeSDKTurn = ClaudeSDKTurn
    { turnRunning :: !RunningClient
    , turnIsNew :: !Bool
    , turnOptions :: !ClaudeAgentOptions
    }

-- | Allocate a serialized client and clean up the complete child process group
-- when the callback exits.
withClaudeSDKClient
    :: ClaudeAgentOptions
    -> (ClaudeSDKClient -> IO a)
    -> IO a
withClaudeSDKClient options =
    withClaudeSDKClientWithTransport
        options
        (subprocessTransportFactory options)

-- | Allocate a client with bidirectional SDK control handlers. Unlike the
-- legacy constructor, this starts one supervised stdout reader and completes
-- the SDK @initialize@ handshake before the first turn is returned.
withClaudeSDKClientWithHandlers
    :: ClaudeAgentOptions
    -> ClaudeAgentHandlers
    -> (ClaudeSDKClient -> IO a)
    -> IO a
withClaudeSDKClientWithHandlers options handlers =
    withClaudeSDKClientWithTransportAndHandlers
        options
        (subprocessTransportFactory options)
        handlers

-- | Allocate a client backed by a caller-supplied transport factory. The
-- factory is invoked for every fresh start or resume and the SDK owns the
-- complete lifecycle of each returned transport.
withClaudeSDKClientWithTransport
    :: ClaudeAgentOptions
    -> TransportFactory
    -> (ClaudeSDKClient -> IO a)
    -> IO a
withClaudeSDKClientWithTransport options transportFactory =
    withClient options transportFactory Nothing

-- | Handler-aware variant of 'withClaudeSDKClientWithTransport'.
withClaudeSDKClientWithTransportAndHandlers
    :: ClaudeAgentOptions
    -> TransportFactory
    -> ClaudeAgentHandlers
    -> (ClaudeSDKClient -> IO a)
    -> IO a
withClaudeSDKClientWithTransportAndHandlers options transportFactory handlers =
    withClient options transportFactory (Just handlers)

withClient
    :: ClaudeAgentOptions
    -> TransportFactory
    -> Maybe ClaudeAgentHandlers
    -> (ClaudeSDKClient -> IO a)
    -> IO a
withClient options transportFactory handlers =
    bracket acquire release
  where
    acquire = do
        state <- newIORef emptyClientState
        interruptEpoch <- newIORef 0
        commitLock <- newMVar ()
        turnLock <- newMVar ()
        pure ClaudeSDKClient
            { clientOptions = options
            , clientTransportFactory = transportFactory
            , clientHandlers = handlers
            , clientInitialPrevious = options.resume
            , clientState = state
            , clientInterruptEpoch = interruptEpoch
            , clientCommitLock = commitLock
            , clientTurnLock = turnLock
            }
    release client = do
        state <- readIORef client.clientState
        mapM_ stopRunningClient state.stateRunning
        writeIORef client.clientState emptyClientState

-- | Convenience client for one-shot auxiliary requests that must not execute
-- Claude Code's built-in tools.
withClaudeSDKClientWithoutTools
    :: ClaudeAgentOptions
    -> (ClaudeSDKClient -> IO a)
    -> IO a
withClaudeSDKClientWithoutTools options =
    withClaudeSDKClient options { tools = Just [] }

-- | Select or create the process for one turn. The host predicate and commit
-- action provide an advanced transaction boundary used by embedding
-- applications that can roll back their own transcript after a completed
-- model turn.
withClaudeSDKTurn
    :: ClaudeSDKClient
    -> IO Bool
    -> Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> (ClaudeSDKTurn -> IO (Either ClaudeSDKError (a, IO ())))
    -> IO (Either ClaudeSDKError a)
withClaudeSDKTurn
    client
    hostTranscriptMatches
    previous
    model
    effort
    callback =
    withMVar client.clientTurnLock \_ ->
        mask \restore -> do
            interruptEpoch <- readIORef client.clientInterruptEpoch
            prepared <- tryAny do
                validateClientOptions client.clientOptions
                (selectedPrevious, consumeInitialPrevious) <-
                    selectPrevious client previous
                matches <- hostTranscriptMatches
                unless matches (invalidateContinuation client)
                turn <-
                    prepareTurn
                        client
                        selectedPrevious
                        (nonEmptyText (model <|> client.clientOptions.model))
                        (nonEmptyText (effort <|> client.clientOptions.effort))
                when consumeInitialPrevious $
                    atomicModifyIORef' client.clientState \state ->
                        ( state
                            { stateInitialPreviousConsumed = True
                            }
                        , ()
                        )
                pure turn
            case prepared of
                Left exception ->
                    pure $
                        Left
                            (connectionError
                                "Failed to start Claude Code"
                                exception)
                Right turn -> do
                    currentInterruptEpoch <-
                        readIORef client.clientInterruptEpoch
                    if currentInterruptEpoch /= interruptEpoch
                        then do
                            abortAndInvalidateTurn client turn
                            pure $
                                Left $
                                    CLIConnectionError
                                        "Claude Code turn was interrupted."
                        else do
                            result <-
                                tryAny $
                                    restore (callback turn)
                                        `onException`
                                            abortAndInvalidateTurn client turn
                            case result of
                                Left exception ->
                                    pure $
                                        Left
                                            (connectionError
                                                "Claude Code turn failed"
                                                exception)
                                Right (Left err) -> do
                                    abortAndInvalidateTurn client turn
                                    pure (Left err)
                                Right (Right (value, commit)) ->
                                    finishSuccessfulTurn
                                        client
                                        interruptEpoch
                                        turn
                                        value
                                        commit
                                        `onException`
                                            abortAndInvalidateTurn client turn

-- | Send one SDK streaming-input user message.
sendQuery
    :: ClaudeSDKTurn
    -> Text
    -> IO (Either ClaudeSDKError ())
sendQuery turn prompt =
    sendQueryContent turn [UserTextBlock prompt]

-- | Send one SDK streaming-input user message with structured content.
sendQueryContent
    :: ClaudeSDKTurn
    -> [UserContentBlock]
    -> IO (Either ClaudeSDKError ())
sendQueryContent turn content = do
    sessionId <- fromMaybe "" <$> turnSessionId turn
    writeRunningClient turn.turnRunning
        ( LazyByteString.toStrict
            ( Aeson.encode
                (Aeson.object
                    [ "type" Aeson..= ("user" :: Text)
                    , "message" Aeson..= Aeson.object
                        [ "role" Aeson..= ("user" :: Text)
                        , "content" Aeson..= map userContentValue content
                        ]
                    , "parent_tool_use_id" Aeson..= Aeson.Null
                    , "session_id" Aeson..= sessionId
                    , "origin" Aeson..= Aeson.object
                        [ "kind" Aeson..= ("human" :: Text)
                        ]
                    ]
                )
                <> "\n"
            )
        )

userContentValue :: UserContentBlock -> Aeson.Value
userContentValue = \case
    UserTextBlock{text} ->
        Aeson.object
            [ "type" Aeson..= ("text" :: Text)
            , "text" Aeson..= text
            ]
    UserImageBlock{mediaType, imageBytes} ->
        Aeson.object
            [ "type" Aeson..= ("image" :: Text)
            , "source" Aeson..= Aeson.object
                [ "type" Aeson..= ("base64" :: Text)
                , "media_type" Aeson..= mediaType
                , "data" Aeson..=
                    TextEncoding.decodeUtf8 (Base64.encode imageBytes)
                ]
            ]

-- | Receive and parse one complete stream-json record. Blank lines are
-- ignored; 'Nothing' denotes EOF.
receiveMessage
    :: ClaudeSDKTurn
    -> IO (Either ClaudeSDKError (Maybe Message))
receiveMessage turn =
    case turn.turnRunning.runningControlRuntime of
        Just runtime ->
            runtimeReadMessage runtime
        Nothing ->
            receiveLegacy
  where
    receiveLegacy =
        turn.turnRunning.runningTransport.transportRead >>= \case
            Left err -> pure (Left err)
            Right Nothing -> pure (Right Nothing)
            Right (Just bytes)
                | ByteString.null (trimAsciiWhitespace bytes) ->
                    receiveLegacy
                | otherwise ->
                    case decodeMessageLine bytes of
                        Left CLIJSONDecodeError{}
                            | not (looksLikeJsonObject bytes) ->
                                -- Claude Code can occasionally write
                                -- diagnostic lines such as [SandboxDebug] to
                                -- stdout.
                                receiveLegacy
                        parsed ->
                            pure (Just <$> parsed)

-- | Convert a process-cumulative @modelUsage@ snapshot into a per-turn delta.
-- When the snapshot is absent, the per-result fallback is remembered as debt
-- and subtracted from a later cumulative delta to avoid double counting.
resolveTurnUsage
    :: ClaudeSDKTurn
    -> Usage
    -> Map.Map Text ModelUsage
    -> IO Usage
resolveTurnUsage turn fallback modelUsage =
    case cumulativeUsage modelUsage of
        Nothing ->
            atomicModifyIORef'
                turn.turnRunning.runningUsageAccounting
                \accounting ->
                    ( accounting
                        { usagePendingFallback =
                            addUsage
                                accounting.usagePendingFallback
                                fallback
                        }
                    , fallback
                    )
        Just current ->
            atomicModifyIORef'
                turn.turnRunning.runningUsageAccounting
                \accounting ->
                    let (reported, pending) =
                            reconcileCumulativeUsage accounting current
                    in
                        ( UsageAccounting
                            { usageCumulativeBaseline = Just current
                            , usagePendingFallback = pending
                            }
                        , reported
                        )

-- | The resolved Claude session ID, when known. New sessions know it before
-- their first query; named resumes and @--continue@ learn it from the result.
turnSessionId :: ClaudeSDKTurn -> IO (Maybe Text)
turnSessionId turn =
    readIORef turn.turnRunning.runningSessionId

-- | Validate or adopt the session ID returned by a terminal result.
acceptTurnSessionId
    :: ClaudeSDKTurn
    -> Text
    -> IO (Either ClaudeSDKError ())
acceptTurnSessionId turn actual =
    atomicModifyIORef'
        turn.turnRunning.runningSessionId
        \expected ->
            case expected of
                Nothing ->
                    ( Just (canonicalSessionId actual)
                    , Right ()
                    )
                Just expectedSession
                    | sameSessionId expectedSession actual ->
                        (expected, Right ())
                    | otherwise ->
                        ( expected
                        , Left $
                            CLIProtocolError
                                ( "Claude Code returned session "
                                    <> actual
                                    <> " while "
                                    <> expectedSession
                                    <> " was active."
                                )
                        )

-- | Adopt the conversation selected by a @conversation_reset@ event and
-- clear process-cumulative usage accounting. Claude Code starts cumulative
-- model usage from a new baseline after a reset even when the subprocess is
-- reused.
acceptConversationReset
    :: ClaudeSDKTurn
    -> ConversationResetMessage
    -> IO ()
acceptConversationReset turn reset = do
    writeIORef
        turn.turnRunning.runningSessionId
        (canonicalSessionId <$> (reset.newConversationId <|> reset.sessionId))
    writeIORef
        turn.turnRunning.runningUsageAccounting
        UsageAccounting
            { usageCumulativeBaseline = Nothing
            , usagePendingFallback = emptyUsage
            }

turnIsNewSession :: ClaudeSDKTurn -> Bool
turnIsNewSession = (.turnIsNew)

turnProcessExit :: ClaudeSDKTurn -> IO (Maybe ExitCode)
turnProcessExit turn =
    turn.turnRunning.runningTransport.transportProcessExit

turnDiagnostic :: ClaudeSDKTurn -> IO Text
turnDiagnostic turn =
    turn.turnRunning.runningTransport.transportDiagnostic

turnStreamStartupTimeoutMicros :: ClaudeSDKTurn -> Int
turnStreamStartupTimeoutMicros turn =
    turn.turnOptions.streamStartupTimeoutMicros

turnStreamInactivityTimeoutMicros :: ClaudeSDKTurn -> Int
turnStreamInactivityTimeoutMicros turn =
    turn.turnOptions.streamInactivityTimeoutMicros

turnTimeoutMicros :: ClaudeSDKTurn -> Int
turnTimeoutMicros turn =
    turn.turnOptions.turnTimeoutMicros

-- | Result object returned by the SDK initialize handshake. Legacy clients
-- that do not enable control handlers return 'Nothing'.
initializationResult :: ClaudeSDKTurn -> Maybe Aeson.Value
initializationResult turn =
    runtimeInitializationResult
        <$> turn.turnRunning.runningControlRuntime

-- | Send one raw forward-compatible control request payload.
sendControlRequest
    :: ClaudeSDKTurn
    -> Aeson.Value
    -> IO (Either ClaudeSDKError Aeson.Value)
sendControlRequest turn request =
    case turn.turnRunning.runningControlRuntime of
        Nothing ->
            pure $
                Left $
                    CLIProtocolError
                        "Control requests require a handler-aware client."
        Just runtime ->
            runtimeSendRequest runtime request

interrupt
    :: ClaudeSDKTurn
    -> IO (Either ClaudeSDKError ())
interrupt turn =
    fmap (fmap (const ())) $
        sendControlRequest turn $
            Aeson.object ["subtype" Aeson..= ("interrupt" :: Text)]

setPermissionMode
    :: ClaudeSDKTurn
    -> Text
    -> IO (Either ClaudeSDKError ())
setPermissionMode turn mode =
    fmap (fmap (const ())) $
        sendControlRequest turn $
            Aeson.object
                [ "subtype" Aeson..= ("set_permission_mode" :: Text)
                , "mode" Aeson..= mode
                ]

setModel
    :: ClaudeSDKTurn
    -> Maybe Text
    -> IO (Either ClaudeSDKError ())
setModel turn selected =
    fmap (fmap (const ())) $
        sendControlRequest turn $
            Aeson.object
                [ "subtype" Aeson..= ("set_model" :: Text)
                , "model" Aeson..= selected
                ]

getContextUsage
    :: ClaudeSDKTurn
    -> IO (Either ClaudeSDKError Aeson.Value)
getContextUsage turn =
    sendControlRequest turn $
        Aeson.object
            [ "subtype" Aeson..= ("get_context_usage" :: Text)
            ]

stopTask
    :: ClaudeSDKTurn
    -> Text
    -> IO (Either ClaudeSDKError ())
stopTask turn taskId =
    fmap (fmap (const ())) $
        sendControlRequest turn $
            Aeson.object
                [ "subtype" Aeson..= ("stop_task" :: Text)
                , "task_id" Aeson..= taskId
                ]

-- | Interrupt the active turn. Handler-aware clients first use the in-band
-- control request, preserving the process and conversation when Claude Code
-- acknowledges it. Legacy clients and failed control requests fall back to a
-- force-close and fresh session.
abort :: ClaudeSDKClient -> IO ()
abort client =
    mask \restore -> do
        running <-
            withMVar client.clientCommitLock \_ -> do
                atomicModifyIORef' client.clientInterruptEpoch \epoch ->
                    (epoch + 1, ())
                atomicModifyIORef' client.clientState \state ->
                    ( state
                        { stateHadCompletedTurn = False
                        , stateNeedsFreshSession = True
                        }
                    , state.stateRunning
                    )
        case running of
            Nothing ->
                pure ()
            Just active
                | Nothing <- active.runningControlRuntime -> do
                    restore (forceCloseRunningClient active)
                    clearRunningClient client active
            Just active -> do
                interrupted <-
                    case active.runningControlRuntime of
                        Nothing -> pure False
                        Just runtime ->
                            either (const False) (const True)
                                <$> restore
                                    (runtimeSendRequest runtime
                                        (Aeson.object
                                            [ "subtype" Aeson..=
                                                ("interrupt" :: Text)
                                            ]))
                if interrupted
                    then
                        atomicModifyIORef' client.clientState \state ->
                            ( state
                                { stateHadCompletedTurn = True
                                , stateNeedsFreshSession = False
                                }
                            , ()
                            )
                    else do
                        atomicModifyIORef' client.clientState \state ->
                            ( state
                                { stateHadCompletedTurn = False
                                , stateNeedsFreshSession = True
                                }
                            , ()
                            )
                        restore (forceCloseRunningClient active)
                        clearRunningClient client active

emptyClientState :: ClientState
emptyClientState = ClientState
    { stateRunning = Nothing
    , stateHadCompletedTurn = False
    , stateNeedsFreshSession = False
    , stateInitialPreviousConsumed = False
    }

selectPrevious
    :: ClaudeSDKClient
    -> Maybe Text
    -> IO (Maybe Text, Bool)
selectPrevious client explicit =
    case nonEmptyText explicit of
        Just selected ->
            pure (Just selected, False)
        Nothing -> do
            consumed <-
                (.stateInitialPreviousConsumed)
                    <$> readIORef client.clientState
            let initial =
                    if consumed
                        then Nothing
                        else nonEmptyText client.clientInitialPrevious
            pure (initial, isJust initial)

prepareTurn
    :: ClaudeSDKClient
    -> Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> IO ClaudeSDKTurn
prepareTurn client previous model effort = do
    oldState <- readIORef client.clientState
    transportUnavailable <- case oldState.stateRunning of
        Nothing ->
            pure False
        Just running -> do
            ready <-
                running.runningTransport.transportIsReady
            processExited <-
                if ready
                    then
                        isJust
                            <$> running.runningTransport.transportProcessExit
                    else
                        pure False
            pure (not ready || processExited)
    decision <-
        decideProcess
            client
            oldState
            transportUnavailable
            previous
            model
            effort
    (running, newState, isNewSession) <- case decision of
        Reuse running ->
            pure (running, oldState, False)
        Restart mode completed -> do
            mapM_
                ( if transportUnavailable
                    then forceCloseRunningClient
                    else stopRunningClient
                )
                oldState.stateRunning
            writeIORef client.clientState
                emptyClientState
                    { stateNeedsFreshSession =
                        oldState.stateNeedsFreshSession
                    , stateInitialPreviousConsumed =
                        oldState.stateInitialPreviousConsumed
                    }
            running <-
                startRunningClient
                    client.clientTransportFactory
                    client.clientHandlers
                    model
                    effort
                    mode
            let state = ClientState
                    { stateRunning = Just running
                    , stateHadCompletedTurn = completed
                    , stateNeedsFreshSession = False
                    , stateInitialPreviousConsumed =
                        oldState.stateInitialPreviousConsumed
                    }
            writeIORef client.clientState state
            pure (running, state, isNewStart mode)
    writeIORef client.clientState newState
    pure ClaudeSDKTurn
        { turnRunning = running
        , turnIsNew = isNewSession
        , turnOptions = client.clientOptions
        }

data ProcessDecision
    = Reuse !RunningClient
    | Restart !TransportMode !Bool

decideProcess
    :: ClaudeSDKClient
    -> ClientState
    -> Bool
    -> Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> IO ProcessDecision
decideProcess client state transportUnavailable previous model effort =
    if state.stateNeedsFreshSession
        then freshStart
        else
            case state.stateRunning of
                Nothing ->
                    initialStart
                Just running ->
                    decideRunning running
  where
    initialStart =
        case nonEmptyText previous of
            Just target ->
                pure (Restart (TransportResume target) True)
            Nothing
                | client.clientOptions.continueConversation ->
                    pure (Restart TransportContinue True)
                | otherwise ->
                    freshStartFromOptions

    decideRunning running = do
        currentSession <- readIORef running.runningSessionId
        let requested = nonEmptyText previous
            requestedDifferent =
                case requested of
                    Nothing -> False
                    Just target ->
                        not
                            ( runningMatches
                                running
                                currentSession
                                target
                            )
        if requestedDifferent
            then
                pure $
                    Restart
                        (TransportResume
                            (fromMaybe "" requested))
                        True
            else if transportUnavailable
                then
                    if state.stateHadCompletedTurn
                        then
                            pure $
                                Restart
                                    (continuationMode
                                        running
                                        currentSession)
                                    True
                        else freshStart
                else if modeChanged running model effort
                    then
                        pure $
                            Restart
                                ( if state.stateHadCompletedTurn
                                    then
                                        continuationMode
                                            running
                                            currentSession
                                    else running.runningStartMode
                                )
                                state.stateHadCompletedTurn
                    else
                        pure (Reuse running)

    freshStartFromOptions =
        case client.clientOptions.sessionId of
            Nothing ->
                freshStart
            Just rawSessionId ->
                case validSessionId rawSessionId of
                    Just sessionId ->
                        pure (Restart (TransportNew sessionId) False)
                    Nothing ->
                        throwIO $
                            CLIProtocolError
                                "ClaudeAgentOptions.sessionId must be a valid UUID."

    freshStart = do
        sessionId <- newSessionId
        pure (Restart (TransportNew sessionId) False)

runningMatches
    :: RunningClient
    -> Maybe Text
    -> Text
    -> Bool
runningMatches running currentSession target =
    maybe False (`sameSessionId` target) currentSession
        || case running.runningStartMode of
            TransportResume started ->
                Text.strip started == Text.strip target
            _ ->
                False

continuationMode
    :: RunningClient
    -> Maybe Text
    -> TransportMode
continuationMode running currentSession =
    case currentSession of
        Just sessionId ->
            TransportResume sessionId
        Nothing ->
            running.runningStartMode

validateClientOptions :: ClaudeAgentOptions -> IO ()
validateClientOptions options
    | options.continueConversation
    , options.resume /= Nothing
        || options.sessionId /= Nothing =
        throwIO $
            CLIProtocolError
                "continueConversation cannot be combined with resume or sessionId."
    | options.resume /= Nothing
    , options.sessionId /= Nothing =
        throwIO $
            CLIProtocolError
                "resume cannot be combined with sessionId."
    | otherwise =
        pure ()

modeChanged
    :: RunningClient
    -> Maybe Text
    -> Maybe Text
    -> Bool
modeChanged running model effort =
    running.runningModel /= model
        || running.runningEffort /= effort

markTurnCompleted :: ClaudeSDKClient -> ClaudeSDKTurn -> IO ()
markTurnCompleted client turn =
    atomicModifyIORef' client.clientState \state ->
        let sameProcess =
                case state.stateRunning of
                    Just running ->
                        running.runningSessionId
                            == turn.turnRunning.runningSessionId
                    Nothing -> False
        in
            ( if sameProcess
                then state { stateHadCompletedTurn = True }
                else state
            , ()
            )

finishSuccessfulTurn
    :: ClaudeSDKClient
    -> Int
    -> ClaudeSDKTurn
    -> a
    -> IO ()
    -> IO (Either ClaudeSDKError a)
finishSuccessfulTurn client turnEpoch turn value commit = do
    outcome <-
        withMVar client.clientCommitLock \_ -> do
            currentEpoch <- readIORef client.clientInterruptEpoch
            if currentEpoch /= turnEpoch
                then do
                    markTurnCompleted client turn
                    pure (Left Nothing)
                else do
                    committed <- tryAny commit
                    case committed of
                        Left exception ->
                            pure (Left (Just exception))
                        Right () -> do
                            markTurnCompleted client turn
                            pure (Right value)
    case outcome of
        Right completed ->
            pure (Right completed)
        Left Nothing ->
            pure $
                Left $
                    CLIConnectionError
                        "Claude Code turn was interrupted."
        Left (Just exception) -> do
            abortAndInvalidateTurn client turn
            pure $
                Left
                    (connectionError
                        "Failed to commit Claude Code turn"
                        exception)

waitForTransportExit :: Int -> Transport -> IO ()
waitForTransportExit closeTimeoutMicros transport =
    go closeTimeoutMicros
  where
    go remaining = do
        processExit <- transport.transportProcessExit
        case processExit of
            Just _ ->
                pure ()
            Nothing
                | remaining <= 0 ->
                    pure ()
                | otherwise -> do
                    let delay = min gracefulClosePollMicros remaining
                    threadDelay delay
                    go (remaining - delay)

gracefulCloseTimeoutMicros :: Int
gracefulCloseTimeoutMicros = 5 * 1_000_000

gracefulClosePollMicros :: Int
gracefulClosePollMicros = 10_000

invalidateContinuation :: ClaudeSDKClient -> IO ()
invalidateContinuation client =
    atomicModifyIORef' client.clientState \state ->
        ( state
            { stateHadCompletedTurn = False
            , stateNeedsFreshSession = True
            }
        , ()
        )

startRunningClient
    :: TransportFactory
    -> Maybe ClaudeAgentHandlers
    -> Maybe Text
    -> Maybe Text
    -> TransportMode
    -> IO RunningClient
startRunningClient transportFactory handlers model effort mode = do
    transport <-
        transportFactory TransportRequest
            { transportMode = mode
            , transportModel = model
            , transportEffort = effort
            }
    connected <-
        transport.transportConnect
            `onException` closeTransportQuietly transport
    case connected of
        Left err -> do
            closeTransportQuietly transport
            throwIO err
        Right () -> do
            sessionIdRef <-
                newIORef $
                    case mode of
                        TransportNew sessionId ->
                            Just sessionId
                        TransportResume target ->
                            validSessionId target
                        TransportContinue ->
                            Nothing
            usageAccounting <-
                newIORef UsageAccounting
                    { usageCumulativeBaseline = Nothing
                    , usagePendingFallback = emptyUsage
                    }
            closed <- newIORef False
            closeLock <- newMVar ()
            controlRuntime <-
                case handlers of
                    Nothing -> pure Nothing
                    Just configured ->
                        (Just
                            <$> startControlRuntime
                                transport
                                configured)
                            `onException`
                                closeTransportQuietly transport
            pure RunningClient
                { runningSessionId = sessionIdRef
                , runningStartMode = mode
                , runningModel = model
                , runningEffort = effort
                , runningTransport = transport
                , runningControlRuntime = controlRuntime
                , runningShutdownTimeoutMicros =
                    maybe
                        gracefulCloseTimeoutMicros
                        (.shutdownTimeoutMicros)
                        handlers
                , runningUsageAccounting = usageAccounting
                , runningClosed = closed
                , runningCloseLock = closeLock
                }

subprocessTransportFactory
    :: ClaudeAgentOptions
    -> TransportFactory
subprocessTransportFactory options request =
    newSubprocessCLITransport
        options
        request.transportMode
        request.transportModel
        request.transportEffort

stopRunningClient :: RunningClient -> IO ()
stopRunningClient running =
    (do
        endInputWorker <-
            asyncWithUnmask \unmask ->
                unmask running.runningTransport.transportEndInput
        endInputResult <-
            pollAsyncUntil
                running.runningShutdownTimeoutMicros
                endInputWorker
        case endInputResult of
            Nothing ->
                cancel endInputWorker
            Just (Left exception) ->
                throwIO exception
            Just (Right ()) ->
                pure ()
        waitForTransportExit
            running.runningShutdownTimeoutMicros
            running.runningTransport)
        `finally`
            forceCloseRunningClient running

pollAsyncUntil
    :: Int
    -> Async a
    -> IO (Maybe (Either SomeException a))
pollAsyncUntil maximumWaitMicros worker =
    go (max 0 maximumWaitMicros)
  where
    go remaining =
        poll worker >>= \case
            Just result ->
                pure (Just result)
            Nothing
                | remaining <= 0 ->
                    pure Nothing
                | otherwise -> do
                    let delay = min gracefulClosePollMicros remaining
                    threadDelay delay
                    go (remaining - delay)

abortRunningClient :: RunningClient -> IO Bool
abortRunningClient running =
    tryAny (forceCloseRunningClient running) >>= \case
        Left _ ->
            pure False
        Right () ->
            pure True

forceCloseRunningClient :: RunningClient -> IO ()
forceCloseRunningClient running =
    case running.runningControlRuntime of
        Nothing ->
            closeRunningClient
                running
                running.runningTransport.transportClose
        Just runtime ->
            closeRunningClient running do
                runtimeResult <-
                    tryAny (stopControlRuntime runtime)
                running.runningTransport.transportClose
                either throwIO pure runtimeResult

writeRunningClient
    :: RunningClient
    -> ByteString.ByteString
    -> IO (Either ClaudeSDKError ())
writeRunningClient running bytes =
    case running.runningControlRuntime of
        Just runtime -> runtimeWrite runtime bytes
        Nothing ->
            running.runningTransport.transportWrite bytes

closeRunningClient :: RunningClient -> IO () -> IO ()
closeRunningClient running closeAction =
    mask \restore ->
        withMVar running.runningCloseLock \_ -> do
            closed <- readIORef running.runningClosed
            unless closed do
                restore closeAction
                writeIORef running.runningClosed True

closeTransportQuietly :: Transport -> IO ()
closeTransportQuietly transport = do
    _ <- tryAny transport.transportClose
    pure ()

abortAndInvalidateTurn
    :: ClaudeSDKClient
    -> ClaudeSDKTurn
    -> IO ()
abortAndInvalidateTurn client turn = do
    atomicModifyIORef' client.clientState \state ->
        let ownsTurn =
                case state.stateRunning of
                    Just running ->
                        running.runningSessionId
                            == turn.turnRunning.runningSessionId
                    Nothing -> False
        in
            ( if ownsTurn
                then state
                    { stateHadCompletedTurn = False
                    , stateNeedsFreshSession = True
                    }
                else state
            , ()
            )
    closed <- abortRunningClient turn.turnRunning
    when closed $
        clearRunningClient client turn.turnRunning

clearRunningClient :: ClaudeSDKClient -> RunningClient -> IO ()
clearRunningClient client expected =
    atomicModifyIORef' client.clientState \state ->
        let ownsRunning =
                case state.stateRunning of
                    Just running ->
                        running.runningSessionId
                            == expected.runningSessionId
                    Nothing -> False
        in
            ( if ownsRunning
                then state { stateRunning = Nothing }
                else state
            , ()
            )

isNewStart :: TransportMode -> Bool
isNewStart = \case
    TransportNew _ -> True
    TransportResume _ -> False
    TransportContinue -> False

connectionError :: Text -> SomeException -> ClaudeSDKError
connectionError prefix exception =
    case fromException exception of
        Just sdkError -> sdkError
        Nothing ->
            CLIConnectionError
                (prefix <> ": " <> Text.pack (show exception))

nonEmptyText :: Maybe Text -> Maybe Text
nonEmptyText =
    (>>= \value ->
        let stripped = Text.strip value
        in if Text.null stripped
            then Nothing
            else Just stripped)

validSessionId :: Text -> Maybe Text
validSessionId value =
    UUID.toText <$> UUID.fromText (Text.strip value)

canonicalSessionId :: Text -> Text
canonicalSessionId value =
    fromMaybe (Text.strip value) (validSessionId value)

sameSessionId :: Text -> Text -> Bool
sameSessionId left right =
    canonicalSessionId left == canonicalSessionId right

newSessionId :: IO Text
newSessionId = do
    randomBytes <- getEntropy 16
    let bytes = ByteString.unpack randomBytes
        versioned =
            replaceAt 6 setVersion
                (replaceAt 8 setVariant bytes)
        setVersion byte = (byte .&. 0x0f) .|. 0x40
        setVariant byte = (byte .&. 0x3f) .|. 0x80
    case
        UUID.fromByteString
            (LazyByteString.fromStrict
                (ByteString.pack versioned))
        of
        Just uuid -> pure (UUID.toText uuid)
        Nothing ->
            fail "Failed to construct a random Claude session UUID"

replaceAt :: Int -> (a -> a) -> [a] -> [a]
replaceAt index update values =
    case splitAt index values of
        (before, value : after) ->
            before <> (update value : after)
        _ -> values

trimAsciiWhitespace
    :: ByteString.ByteString
    -> ByteString.ByteString
trimAsciiWhitespace =
    ByteString.dropWhileEnd isAsciiWhitespace
        . ByteString.dropWhile isAsciiWhitespace
  where
    isAsciiWhitespace byte =
        byte `elem` [9, 10, 13, 32]

looksLikeJsonObject :: ByteString.ByteString -> Bool
looksLikeJsonObject bytes =
    case ByteString.uncons (trimAsciiWhitespace bytes) of
        Just (123, _) -> True
        _ -> False
