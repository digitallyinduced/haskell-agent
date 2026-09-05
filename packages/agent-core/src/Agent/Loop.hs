{-# LANGUAGE PatternSynonyms #-}

-- | Provider-neutral agent loop: submit a user turn, dispatch tool calls,
-- feed results back, and repeat until the model answers in visible text or
-- hits a cap.
module Agent.Loop
    ( Backend(Backend, submitTurn, submitTurnWithCallbacks)
    , BackendCallbacks(..)
    , BackendMiddleware
    , BackendContinuation(..)
    , BackendRevision(..)
    , BackendResult(..)
    , BackendSnapshot(..)
    , BackendStateStore(..)
    , FileAttachment(..)
    , ImageAttachment(..)
    , LoopConfig(..)
    , LoopExecution(..)
    , LoopEvent(..)
    , NativeAgentStatus(..)
    , LoopError(..)
    , LoopProgress(..)
    , LoopResult(..)
    , TokenUsage(..)
    , TurnAttachment(..)
    , TurnCompletion(..)
    , TurnInput(..)
    , TurnOutput(..)
    , addTokenUsage
    , advanceBackendSnapshot
    , backendContinuationToken
    , backendWithCallbacks
    , clearBackendContinuation
    , defaultLoopMaxTurns
    , defaultLoopMaxEmptyContinuations
    , defaultLoopDispatch
    , emptyTokenUsage
    , emptyBackendSnapshot
    , emptyTurnOutput
    , estimateTokensFromChars
    , generationTokensPerSecond
    , initialBackendSnapshot
    , liveTokenRateMinMillis
    , liveTokensPerSecond
    , mapTurnInputUserText
    , runLoop
    , runLoopInputs
    , runLoopInputsDetailed
    , tokensPerSecond
    , tokenUsageDecoder
    , turnInputFiles
    , turnInputImages
    , userMessageWithAttachments
    ) where

import Agent.Loop.Internal
import Agent.ToolDispatch (ToolDispatchConfig(..))
import Control.Exception.Safe (SomeException)
import qualified Data.Text as Text

-- | Decorate one provider/model submission.
--
-- This is the model-step analogue of WAI middleware. Values compose with
-- ordinary function composition: in @(outer . inner) backend@, @outer@
-- observes the request first and the result last.
--
-- A backend submission may stream events and announce async tool calls. A
-- middleware that retries a provider step must therefore preserve the complete
-- callback set and account for any host-side effects already admitted.
type BackendMiddleware = Backend -> Backend

defaultLoopMaxTurns :: Int
defaultLoopMaxTurns = 2000

defaultLoopMaxEmptyContinuations :: Int
defaultLoopMaxEmptyContinuations = 2

defaultLoopDispatch :: ToolDispatchConfig
defaultLoopDispatch = ToolDispatchConfig
    { toolDispatchUnknownTool = \name -> "Unknown tool: " <> name
    , toolDispatchFormatResult = \case
        Left err -> "Error: " <> err
        Right output -> output
    , toolDispatchFormatException = \name exception ->
        "Tool " <> name <> " crashed: " <> Text.pack (show exception)
    , toolDispatchOnException = \_name (_ :: SomeException) -> pure ()
    , toolDispatchOnOutput = \_call _output -> pure ()
    , toolDispatchFinalizeOutput = \_call output -> pure output
    }
