-- | One-shot and per-turn query functions, analogous to the official SDK's
-- top-level @query()@ API.
module Claude.Agent.SDK.Query
    ( query
    , queryClient
    , queryTurn
    , queryTurnWithMessageValidator
    , receiveResponse
    , receiveResponseWithMessageValidator
    ) where

import Claude.Agent.SDK.Client
    ( ClaudeSDKClient
    , ClaudeSDKTurn
    , acceptTurnSessionId
    , receiveMessage
    , sendQuery
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
    , consumeQueryMessage
    , emptyQueryAccumulator
    )
import Claude.Agent.SDK.Types
    ( ClaudeAgentOptions(..)
    , Message
    , ResultMessage(..)
    )
import Data.Text (Text)
import qualified Data.Text as Text
import System.Exit (ExitCode)
import System.Timeout (timeout)

-- | Run a self-contained query and close its client afterward.
query
    :: ClaudeAgentOptions
    -> Text
    -> (Message -> IO ())
    -> IO (Either ClaudeSDKError ResultMessage)
query options prompt onMessage =
    withClaudeSDKClient options \client ->
        withClaudeSDKTurn
            client
            (pure True)
            Nothing
            options.model
            options.effort
            \turn -> do
                result <- queryTurn turn prompt onMessage
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
    withClaudeSDKTurn
        client
        (pure True)
        Nothing
        Nothing
        Nothing
        \turn -> do
            result <- queryTurn turn prompt onMessage
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
    queryTurnWithMessageValidator
        turn
        prompt
        (const (pure (Right ())))
        onMessage

-- | Variant of 'queryTurn' that observes and may reject each parsed wire
-- message immediately, before transactional response buffering. Embedders can
-- use this for authentication or policy checks that must fail before a turn
-- reaches its terminal result.
queryTurnWithMessageValidator
    :: ClaudeSDKTurn
    -> Text
    -> (Message -> IO (Either ClaudeSDKError ()))
    -> (Message -> IO ())
    -> IO (Either ClaudeSDKError ResultMessage)
queryTurnWithMessageValidator turn prompt validateMessage onMessage = do
    completed <-
        timeout
            (max 1 (turnTimeoutMicros turn))
            do
                sendQuery turn prompt >>= \case
                    Left err -> pure (Left err)
                    Right () ->
                        receiveResponseWithMessageValidator
                            turn
                            validateMessage
                            onMessage
    case completed of
        Nothing ->
            Left <$> timeoutError
                turn
                "did not complete within the turn timeout"
        Just result ->
            pure result

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
    go emptyQueryAccumulator False
  where
    go
        :: QueryAccumulator
        -> Bool
        -> IO (Either ClaudeSDKError ResultMessage)
    go accumulator sawOutput = do
        maybeMessage <-
            timeout
                ( max 1 $
                    if sawOutput
                        then
                            turnStreamInactivityTimeoutMicros turn
                        else
                            turnStreamStartupTimeoutMicros turn
                )
                (receiveMessage turn)
        case maybeMessage of
            Nothing ->
                Left <$> timeoutError
                    turn
                    ( if sawOutput
                        then "stopped producing structured output"
                        else "did not produce structured output"
                    )
            Just (Left err) ->
                pure (Left err)
            Just (Right Nothing) ->
                Left <$> prematureExitError turn
            Just (Right (Just message)) -> do
                validated <- validateMessage message
                case validated of
                    Left err ->
                        pure (Left err)
                    Right () ->
                        case consumeQueryMessage accumulator message of
                            Left err ->
                                pure (Left err)
                            Right (nextAccumulator, Nothing) ->
                                go nextAccumulator True
                            Right (_, Just (messages, result)) -> do
                                accepted <-
                                    acceptTurnSessionId
                                        turn
                                        result.sessionId
                                case accepted of
                                    Left err ->
                                        pure (Left err)
                                    Right () -> do
                                        mapM_ onMessage messages
                                        pure (Right result)

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
