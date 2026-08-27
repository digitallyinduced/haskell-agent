-- | Provider-neutral agent loop: submit a user turn, dispatch tool calls,
-- feed results back, and repeat until the model answers in visible text or
-- hits a cap.
module Agent.Loop
    ( Backend(..)
    , BackendResult(..)
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
    , TurnCompletion(..)
    , TurnInput(..)
    , TurnOutput(..)
    , addTokenUsage
    , defaultLoopMaxTurns
    , defaultLoopMaxEmptyContinuations
    , defaultLoopDispatch
    , emptyTokenUsage
    , emptyTurnOutput
    , estimateTokensFromChars
    , generationTokensPerSecond
    , liveTokenRateMinMillis
    , liveTokensPerSecond
    , runLoop
    , tokensPerSecond
    , runLoopInputs
    , runLoopInputsDetailed
    ) where

import Agent.Loop.Internal
