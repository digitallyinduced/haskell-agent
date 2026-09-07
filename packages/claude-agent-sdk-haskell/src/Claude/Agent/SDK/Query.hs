-- | One-shot and per-turn query functions, analogous to the official SDK's
-- top-level @query()@ API.
module Claude.Agent.SDK.Query
    ( QueryResult(..)
    , query
    , queryWithProgress
    , queryContent
    , queryContentWithProgress
    , queryClient
    , queryClientContent
    , queryTurn
    , queryTurnWithProgress
    , queryTurnContent
    , queryTurnContentWithProgress
    , queryTurnWithMessageValidator
    , queryTurnContentWithMessageValidator
    , queryTurnContentWithMessageValidatorAndProgress
    , receiveResponse
    , receiveResponseWithMessageValidator
    , receiveResponseWithMessageValidatorAndProgress
    ) where

import Claude.Agent.SDK.Client
    ( ClaudeSDKClient
    , ClaudeSDKTurn
    , acceptConversationReset
    , acceptTurnSessionId
    , receiveMessage
    , sendQueryContent
    , turnDiagnostic
    , turnProcessExit
    , turnStreamInactivityTimeoutMicros
    , turnStreamStartupTimeoutMicros
    , turnTimeoutMicros
    , withClaudeSDKClient
    , withClaudeSDKTurn
    )
import Claude.Agent.SDK.Errors
    ( ClaudeSDKError(..)
    )
import Claude.Agent.SDK.Internal.Query
    ( QueryAccumulator
    , consumeQueryMessageWithProgress
    , emptyQueryAccumulator
    )
import Claude.Agent.SDK.Types
    ( ClaudeAgentOptions(..)
    , Message
    , QueryProgress(..)
    , ResultMessage(..)
    , UserContentBlock(..)
    , messageSessionId
    )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
    ( ExceptT(..)
    , except
    , runExceptT
    , throwE
    )
import Data.Text (Text)
import qualified Data.Text as Text
import System.Exit (ExitCode)
import System.Timeout (timeout)

-- | A completed query together with the canonical message sequence that
-- produced its terminal result.
data QueryResult = QueryResult
    { queryMessages :: ![Message]
    , queryResultMessage :: !ResultMessage
    } deriving (Eq, Show)

-- | Run a self-contained query and close its client afterward.
query
    :: ClaudeAgentOptions
    -> Text
    -> (Message -> IO ())
    -> IO (Either ClaudeSDKError ResultMessage)
query options prompt onMessage =
    queryContent options [UserTextBlock prompt] onMessage

queryWithProgress
    :: ClaudeAgentOptions
    -> Text
    -> (QueryProgress -> IO ())
    -> (Message -> IO ())
    -> IO (Either ClaudeSDKError ResultMessage)
queryWithProgress options prompt onProgress onMessage =
    queryContentWithProgress
        options
        [UserTextBlock prompt]
        onProgress
        onMessage

queryTurnContentWithProgress
    :: ClaudeSDKTurn
    -> [UserContentBlock]
    -> (QueryProgress -> IO ())
    -> (Message -> IO ())
    -> IO (Either ClaudeSDKError ResultMessage)
queryTurnContentWithProgress turn content onProgress onMessage =
    fmap queryResultOnly $
        queryTurnContentWithMessageValidatorAndProgress
            turn
            content
            (const (pure (Right ())))
            onProgress
            onMessage

-- | Run a self-contained query with structured user content.
queryContent
    :: ClaudeAgentOptions
    -> [UserContentBlock]
    -> (Message -> IO ())
    -> IO (Either ClaudeSDKError ResultMessage)
queryContent options content onMessage =
    queryContentWithProgress
        options
        content
        (const (pure ()))
        onMessage

queryContentWithProgress
    :: ClaudeAgentOptions
    -> [UserContentBlock]
    -> (QueryProgress -> IO ())
    -> (Message -> IO ())
    -> IO (Either ClaudeSDKError ResultMessage)
queryContentWithProgress options content onProgress onMessage =
    withClaudeSDKClient options \client ->
        withClaudeSDKTurn
            client
            (pure True)
            Nothing
            options.model
            options.effort
            \turn -> do
                result <-
                    queryTurnContentWithProgress
                        turn
                        content
                        onProgress
                        onMessage
                pure ((, pure ()) <$> result)

-- | Submit a prompt through a persistent client and continue its active
-- conversation by default. Use 'withClaudeSDKTurn' directly when an embedding
-- application needs explicit host-transcript rollback or session selection.
queryClient
    :: ClaudeSDKClient
    -> Text
    -> (Message -> IO ())
    -> IO (Either ClaudeSDKError ResultMessage)
queryClient client prompt onMessage =
    queryClientContent client [UserTextBlock prompt] onMessage

-- | Submit structured user content through a persistent client.
queryClientContent
    :: ClaudeSDKClient
    -> [UserContentBlock]
    -> (Message -> IO ())
    -> IO (Either ClaudeSDKError ResultMessage)
queryClientContent client content onMessage =
    withClaudeSDKTurn
        client
        (pure True)
        Nothing
        Nothing
        Nothing
        \turn -> do
            result <- queryTurnContent turn content onMessage
            pure ((, pure ()) <$> result)

-- | Submit a prompt and receive one complete response. Visible records are
-- held until the terminal result so UUID deduplication and refusal-fallback
-- retractions can be applied transactionally. The active session ID is
-- checked before any buffered messages reach the callback.
queryTurn
    :: ClaudeSDKTurn
    -> Text
    -> (Message -> IO ())
    -> IO (Either ClaudeSDKError ResultMessage)
queryTurn turn prompt onMessage =
    queryTurnContent turn [UserTextBlock prompt] onMessage

queryTurnWithProgress
    :: ClaudeSDKTurn
    -> Text
    -> (QueryProgress -> IO ())
    -> (Message -> IO ())
    -> IO (Either ClaudeSDKError ResultMessage)
queryTurnWithProgress turn prompt onProgress onMessage =
    queryTurnContentWithProgress
        turn
        [UserTextBlock prompt]
        onProgress
        onMessage

-- | Submit structured user content and receive one complete response.
queryTurnContent
    :: ClaudeSDKTurn
    -> [UserContentBlock]
    -> (Message -> IO ())
    -> IO (Either ClaudeSDKError ResultMessage)
queryTurnContent turn content onMessage =
    queryTurnContentWithMessageValidator
        turn
        content
        (const (pure (Right ())))
        onMessage

-- | Variant of 'queryTurn' that observes and may reject each parsed wire
-- message immediately, before transactional response buffering.
queryTurnWithMessageValidator
    :: ClaudeSDKTurn
    -> Text
    -> (Message -> IO (Either ClaudeSDKError ()))
    -> (Message -> IO ())
    -> IO (Either ClaudeSDKError ResultMessage)
queryTurnWithMessageValidator turn prompt =
    queryTurnContentWithMessageValidator turn [UserTextBlock prompt]

-- | Structured-content variant of 'queryTurnWithMessageValidator'.
queryTurnContentWithMessageValidator
    :: ClaudeSDKTurn
    -> [UserContentBlock]
    -> (Message -> IO (Either ClaudeSDKError ()))
    -> (Message -> IO ())
    -> IO (Either ClaudeSDKError ResultMessage)
queryTurnContentWithMessageValidator turn content validateMessage onMessage = do
    fmap queryResultOnly $
        queryTurnContentWithMessageValidatorAndProgress
            turn
            content
            validateMessage
            (const (pure ()))
            onMessage

-- | Like 'queryTurnContentWithMessageValidator', with a live observer that
-- runs after query routing and canonicalization state updates. It sees only
-- records belonging to the submitted human turn; autonomous/background
-- records remain hidden.
queryTurnContentWithMessageValidatorAndProgress
    :: ClaudeSDKTurn
    -> [UserContentBlock]
    -> (Message -> IO (Either ClaudeSDKError ()))
    -> (QueryProgress -> IO ())
    -> (Message -> IO ())
    -> IO (Either ClaudeSDKError QueryResult)
queryTurnContentWithMessageValidatorAndProgress
    turn content validateMessage onProgress onMessage =
        runExceptT $
            withSDKTimeout
                (turnTimeoutMicros turn)
                ( timeoutError
                    turn
                    ( "did not complete within the turn timeout; it may already "
                        <> "have changed files or created remote side effects, so "
                        <> "inspect the workspace, Git history, and pull requests "
                        <> "before retrying"
                    )
                )
                do
                    ExceptT (sendQueryContent turn content)
                    receiveResponseT
                        turn
                        validateMessage
                        onProgress
                        onMessage

-- | Receive messages until the matching successful 'ResultMessage'.
receiveResponse
    :: ClaudeSDKTurn
    -> (Message -> IO ())
    -> IO (Either ClaudeSDKError ResultMessage)
receiveResponse turn onMessage =
    receiveResponseWithMessageValidator
        turn
        (const (pure (Right ())))
        onMessage

-- | Receive a complete response while validating every parsed wire message
-- before it enters the transactional accumulator.
receiveResponseWithMessageValidator
    :: ClaudeSDKTurn
    -> (Message -> IO (Either ClaudeSDKError ()))
    -> (Message -> IO ())
    -> IO (Either ClaudeSDKError ResultMessage)
receiveResponseWithMessageValidator turn validateMessage onMessage =
    fmap queryResultOnly $
        receiveResponseWithMessageValidatorAndProgress
            turn
            validateMessage
            (const (pure ()))
            onMessage

-- | Receive a response with classified live query progress.
receiveResponseWithMessageValidatorAndProgress
    :: ClaudeSDKTurn
    -> (Message -> IO (Either ClaudeSDKError ()))
    -> (QueryProgress -> IO ())
    -> (Message -> IO ())
    -> IO (Either ClaudeSDKError QueryResult)
receiveResponseWithMessageValidatorAndProgress
    turn validateMessage onProgress onMessage =
        runExceptT $
            receiveResponseT
                turn
                validateMessage
                onProgress
                onMessage

receiveResponseT
    :: ClaudeSDKTurn
    -> (Message -> IO (Either ClaudeSDKError ()))
    -> (QueryProgress -> IO ())
    -> (Message -> IO ())
    -> ExceptT ClaudeSDKError IO QueryResult
receiveResponseT turn validateMessage onProgress onMessage =
    go emptyQueryAccumulator False
  where
    go
        :: QueryAccumulator
        -> Bool
        -> ExceptT ClaudeSDKError IO QueryResult
    go accumulator sawOutput = do
        outcome <- liftIO (receiveOutcome turn sawOutput)
        message <- messageFromReceiveOutcome turn sawOutput outcome
        ExceptT (validateMessage message)
        responseStep <- except (consumeResponseMessage accumulator message)
        case responseStep of
            ResponseContinues nextAccumulator progress -> do
                validateLiveProgressSession turn message progress
                liftIO (mapM_ onProgress progress)
                go
                    nextAccumulator
                    (sawOutput || hasOwnOutputProgress progress)
            ResponseCompleted progress messages result -> do
                ExceptT (acceptTurnSessionId turn result.sessionId)
                liftIO (mapM_ onProgress progress)
                liftIO (mapM_ onMessage messages)
                pure QueryResult
                    { queryMessages = messages
                    , queryResultMessage = result
                    }

data ReceiveOutcome
    = ReceiveTimedOut
    | ReceiveEOF
    | ReceiveTransportError !ClaudeSDKError
    | ReceivedMessage !Message

receiveOutcome
    :: ClaudeSDKTurn
    -> Bool
    -> IO ReceiveOutcome
receiveOutcome turn sawOutput = do
    received <-
        timeout
            (max 1 (streamTimeoutMicros turn sawOutput))
            (receiveMessage turn)
    pure case received of
        Nothing -> ReceiveTimedOut
        Just (Left err) -> ReceiveTransportError err
        Just (Right Nothing) -> ReceiveEOF
        Just (Right (Just message)) -> ReceivedMessage message

streamTimeoutMicros :: ClaudeSDKTurn -> Bool -> Int
streamTimeoutMicros turn sawOutput
    | sawOutput = turnStreamInactivityTimeoutMicros turn
    | otherwise = turnStreamStartupTimeoutMicros turn

messageFromReceiveOutcome
    :: ClaudeSDKTurn
    -> Bool
    -> ReceiveOutcome
    -> ExceptT ClaudeSDKError IO Message
messageFromReceiveOutcome turn sawOutput = \case
    ReceiveTimedOut ->
        liftIO
            ( timeoutError
                turn
                ( if sawOutput
                    then "stopped producing structured output"
                    else "did not produce structured output"
                )
            )
            >>= throwE
    ReceiveEOF ->
        liftIO (prematureExitError turn) >>= throwE
    ReceiveTransportError err ->
        throwE err
    ReceivedMessage message ->
        pure message

data ResponseStep
    = ResponseContinues
        !QueryAccumulator
        ![QueryProgress]
    | ResponseCompleted
        ![QueryProgress]
        ![Message]
        !ResultMessage

consumeResponseMessage
    :: QueryAccumulator
    -> Message
    -> Either ClaudeSDKError ResponseStep
consumeResponseMessage accumulator message = do
    (nextAccumulator, progress, completion) <-
        consumeQueryMessageWithProgress accumulator message
    pure case completion of
        Nothing ->
            ResponseContinues nextAccumulator progress
        Just (messages, result) ->
            ResponseCompleted progress messages result

queryResultOnly
    :: Either ClaudeSDKError QueryResult
    -> Either ClaudeSDKError ResultMessage
queryResultOnly =
    fmap \QueryResult{queryResultMessage} -> queryResultMessage

validateLiveProgressSession
    :: ClaudeSDKTurn
    -> Message
    -> [QueryProgress]
    -> ExceptT ClaudeSDKError IO ()
validateLiveProgressSession turn message progress
    | QueryConversationReset reset : _ <- progress = do
        liftIO (acceptConversationReset turn reset)
    | null progress = pure ()
    | otherwise =
        case messageSessionId message of
            Nothing -> pure ()
            Just sessionId -> ExceptT (acceptTurnSessionId turn sessionId)

hasOwnOutputProgress :: [QueryProgress] -> Bool
hasOwnOutputProgress =
    any \case
        QueryMessageObserved{} -> True
        QueryMessagesRetracted{} -> True
        QueryConversationReset{} -> True

withSDKTimeout
    :: Int
    -> IO ClaudeSDKError
    -> ExceptT ClaudeSDKError IO a
    -> ExceptT ClaudeSDKError IO a
withSDKTimeout durationMicros timeoutFailure action =
    ExceptT do
        completed <-
            timeout
                (max 1 durationMicros)
                (runExceptT action)
        case completed of
            Nothing -> Left <$> timeoutFailure
            Just result -> pure result

timeoutError :: ClaudeSDKTurn -> Text -> IO ClaudeSDKError
timeoutError turn reason = do
    diagnostic <- turnDiagnostic turn
    pure $
        CLIConnectionError
            ( "Claude Code "
                <> reason
                <> "."
                <> diagnosticSuffix diagnostic
            )

prematureExitError :: ClaudeSDKTurn -> IO ClaudeSDKError
prematureExitError turn = do
    processExit <- turnProcessExit turn
    diagnostic <- turnDiagnostic turn
    pure ProcessError
        { message =
            "Claude Code closed its structured output before completing the turn"
        , exitCode = processExit
        , stderr = diagnostic
        }

_exitSuffix :: Maybe ExitCode -> Text
_exitSuffix = \case
    Nothing -> ""
    Just exitCode ->
        " (" <> Text.pack (show exitCode) <> ")"

diagnosticSuffix :: Text -> Text
diagnosticSuffix diagnostic
    | Text.null (Text.strip diagnostic) =
        ""
    | otherwise =
        "\nClaude Code stderr:\n"
            <> Text.takeEnd 2_000 diagnostic
