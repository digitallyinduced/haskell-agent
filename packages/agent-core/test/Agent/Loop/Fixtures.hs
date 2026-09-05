-- | Small deterministic backends and registries shared by loop behavior specs.
module Agent.Loop.Fixtures
    ( emptyTestTelemetry
    , testConfig
    , registryFromHandlers
    , registryFromPolicies
    , registryFromTools
    , resourceTool
    , noArgsAppTool
    , asyncNoArgsTool
    , asyncFunctionToolCall
    , completedCallIds
    , concurrencyProbeMicros
    , EchoArgs(..)
    , echoArgsDecoder
    , functionResult
    , scriptedBackend
    , endlessToolsBackend
    , stateMarker
    , appendStateMarker
    ) where

import Agent.Cancel (newCancelFlag)
import Agent.Error (ApiError(..))
import qualified Agent.Json.Decode as Json
import Agent.Loop
import Agent.Responses.Types (ResponseItem(..), TaggedObject(..))
import Agent.Telemetry (TurnTelemetry(..))
import Agent.ToolArgs (objectArgs, reqText)
import Agent.ToolDispatch
import Agent.Tools.Scheduling (ToolAccess(..), ToolResource(..), ToolResourceClaim(..))
import Agent.Tools.Types
    ( AppTool
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , ToolRegistry
    , jsonAppToolWithExecution
    , mkToolRegistry
    , withAsyncToolCalls
    , withToolResourceClaims
    )
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text

emptyTestTelemetry :: TurnTelemetry
emptyTestTelemetry = TurnTelemetry
    { telemetryDurationMs = Nothing
    , telemetryApiDurationMs = Nothing
    , telemetryCostUsd = Nothing
    , telemetryStopReason = Nothing
    , telemetryProviderTurns = Nothing
    , telemetryModels = mempty
    , telemetryStructuredOutput = Nothing
    }

testConfig :: Backend -> IO LoopConfig
testConfig backend = do
    cancel <- newCancelFlag
    state <- newIORef emptyBackendSnapshot
    pure LoopConfig
        { loopBackend = backend
        , loopBackendState = BackendStateStore
            { readBackendState = readIORef state
            , commitBackendState = \snapshot -> do
                writeIORef state snapshot
                pure snapshot
            }
        , loopTools = registryFromHandlers
            [ typedTool "echo" echoArgsDecoder $ \EchoArgs { message } ->
                pure (Right ("echo:" <> message))
            ]
        , loopDispatch = defaultLoopDispatch
        , loopMaxTurns = defaultLoopMaxTurns
        , loopOnEvent = \_ -> pure ()
        , loopApprove = \_ -> pure (Right True)
        , loopReadSteering = pure []
        , loopCommitSteering = \_ -> pure ()
        , loopInterrupt = pure ()
        , loopCancel = cancel
        }

registryFromHandlers :: [ToolHandler] -> ToolRegistry
registryFromHandlers =
    registryFromPolicies . map (\handler -> (ParallelSafe, handler))

registryFromPolicies :: [(ToolExecutionPolicy, ToolHandler)] -> ToolRegistry
registryFromPolicies tools =
    either (error . Text.unpack) id $ mkToolRegistry
        [ jsonAppToolWithExecution
            (handlerName handler)
            ""
            []
            AlwaysReadOnly
            execution
            handler
        | (execution, handler) <- tools
        ]

registryFromTools :: [AppTool] -> ToolRegistry
registryFromTools =
    either (error . Text.unpack) id . mkToolRegistry

resourceTool :: Text -> Text -> IO (Either Text Text) -> AppTool
resourceTool name resource action =
    withToolResourceClaims
        (\_ ->
            pure $ Right
                [ ToolResourceClaim ToolWrite
                    (ToolNamedResource resource)
                ])
        (jsonAppToolWithExecution
            name
            ""
            []
            AlwaysReadOnly
            TurnSequential
            (noArgsTool name action))

noArgsAppTool :: Text -> IO (Either Text Text) -> AppTool
noArgsAppTool name action =
    jsonAppToolWithExecution
        name
        ""
        []
        AlwaysReadOnly
        ParallelSafe
        (noArgsTool name action)

asyncNoArgsTool :: Text -> IO (Either Text Text) -> AppTool
asyncNoArgsTool name action =
    withAsyncToolCalls (noArgsAppTool name action)

asyncFunctionToolCall :: Text -> Text -> Text -> ToolCall
asyncFunctionToolCall callId name arguments =
    withToolCallMode AsyncToolCall
        (functionToolCall callId name arguments)

completedCallIds :: [TurnInput] -> [Text]
completedCallIds inputs =
    [ result.callId
    | CompletedTool result <- inputs
    ]

concurrencyProbeMicros :: Int
concurrencyProbeMicros = 5000000

data EchoArgs = EchoArgs { message :: Text }

echoArgsDecoder :: Json.Decoder EchoArgs
echoArgsDecoder = objectArgs $ \object -> EchoArgs <$> reqText object "message"

functionResult :: Text -> Text -> ToolCallResult
functionResult callId output = ToolCallResultWithOutcome
    { callId
    , output
    , toolResultImages = []
    , toolResultOutcome = ToolSucceeded
    , callKind = FunctionCallKind
    }

scriptedBackend
    :: IORef [(Maybe Text, [TurnInput])]
    -> [Either ApiError TurnOutput]
    -> IO Backend
scriptedBackend submissions answers = do
    remaining <- newIORef answers
    pure $ Backend \state prev inputs _onEvent -> do
        modifyIORef' submissions (++ [(prev, inputs)])
        atomicModifyIORef' remaining \case
            [] -> ([], Left (ConnectionError "scripted backend exhausted"))
            next : rest ->
                ( rest
                , fmap
                    (\output -> BackendResult
                        { backendOutput = output
                        , backendState = appendStateMarker state
                        })
                    next
                )

endlessToolsBackend :: IO Backend
endlessToolsBackend = do
    counter <- newIORef (0 :: Int)
    pure $ Backend \state _prev _inputs _onEvent -> do
        n <- atomicModifyIORef' counter \i -> (i + 1, i + 1)
        let responseId = "resp-" <> Text.pack (show n)
        pure $ Right BackendResult
            { backendOutput = emptyTurnOutput responseId
                [functionToolCall "c1" "echo" "{\"message\":\"again\"}"]
                Nothing
            , backendState = appendStateMarker state
            }

stateMarker :: ResponseItem
stateMarker = UnknownResponseItem TaggedObject
    { tag = "test_state"
    }

appendStateMarker :: BackendSnapshot -> BackendSnapshot
appendStateMarker snapshot =
    advanceBackendSnapshot snapshot
        (snapshot.backendItems <> [stateMarker])
        snapshot.backendContinuation
