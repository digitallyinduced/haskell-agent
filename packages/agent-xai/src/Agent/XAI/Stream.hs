-- | Typed decoding and terminal-response assembly for xAI Responses SSE.
module Agent.XAI.Stream
    ( SseDecoder
    , newSseDecoder
    , feedSseDecoder
    , finishSseDecoder
    , parseSseEvents
    , buildResponse
    ) where

import Agent.Error (ApiError(..), errorTypeFromText)
import Agent.Responses.Error (mkOpenAIError)
import Agent.Responses.SSE
    ( SseDecoder
    , feedSseDecoder
    , finishSseDecoder
    , newSseDecoder
    , parseSseEvents
    )
import Agent.Responses.StreamAssembly
    ( StreamAssemblyConfig(..)
    , buildStreamResponse
    , failedResponseMessage
    )
import Agent.Responses.Types
import Agent.XAI.Error (classifyStreamError)

-- | Merge streamed output-item events into the terminal completed response.
buildResponse :: [ResponseStreamEvent] -> Either ApiError Response
buildResponse = buildStreamResponse StreamAssemblyConfig
    { missingCompletionMessage =
        "No terminal response event found in xAI SSE stream"
    , classifyStreamError
    , classifyFailedResponse = failedResponseError
    }

failedResponseError :: Response -> ApiError
failedResponseError response = case response.error of
    Just responseError ->
        mkOpenAIError
            (errorTypeFromText responseError.code)
            responseError.message
            (Just responseError.code)
            Nothing
    Nothing -> ConnectionError (failedResponseMessage response)
