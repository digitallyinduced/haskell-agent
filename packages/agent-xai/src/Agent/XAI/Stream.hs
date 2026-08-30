-- | Typed decoding and terminal-response assembly for xAI Responses SSE.
module Agent.XAI.Stream
    ( SseDecoder
    , newSseDecoder
    , feedSseDecoder
    , finishSseDecoder
    , parseSseEvents
    , streamAssemblyConfig
    , buildResponse
    ) where

import Agent.Error (ApiError(..), ErrorType(..), errorTypeFromText)
import Agent.Responses.Error (mkOpenAIError)
import Agent.Responses.SSE
    ( SseDecoder
    , feedSseDecoder
    , finishSseDecoder
    , newSseDecoder
    , parseSseEvents
    )
import Agent.Responses.StreamAssembly
    ( ResponseFailure(..)
    , StreamAssemblyConfig(..)
    , buildStreamResponse
    , failedStreamResponseMessage
    )
import Agent.Responses.Types
import Agent.XAI.Error (classifyStreamError)
import Control.Applicative ((<|>))

-- | Merge streamed output-item events into the terminal completed response.
buildResponse :: [ResponseStreamEvent] -> Either ApiError Response
buildResponse = buildStreamResponse streamAssemblyConfig

streamAssemblyConfig :: StreamAssemblyConfig
streamAssemblyConfig = StreamAssemblyConfig
    { missingCompletionMessage =
        "No terminal response event found in xAI SSE stream"
    , classifyStreamError
    , classifyFailedResponse = failedResponseError
    , incompleteAsFailure = False
    }

failedResponseError :: ResponseFailure -> ApiError
failedResponseError response =
    case response.failureErrorType
            <|> response.failureErrorCode
            <|> response.failureErrorMessage of
        Nothing -> ConnectionError (failedStreamResponseMessage response)
        Just _ ->
            mkOpenAIError
                (maybe ApiErrorType errorTypeFromText
                    (response.failureErrorType <|> response.failureErrorCode))
                (failedStreamResponseMessage response)
                response.failureErrorCode
                Nothing
