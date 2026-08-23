-- | Typed decoding and terminal-response assembly for xAI Responses SSE.
module Agent.XAI.Stream
    ( buildResponse
    ) where

import Agent.Error (ApiError(..), ErrorType(..), errorTypeFromText)
import Agent.Responses.Error (mkOpenAIError)
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
buildResponse = buildStreamResponse StreamAssemblyConfig
    { missingCompletionMessage =
        "No terminal response event found in xAI SSE stream"
    , classifyStreamError
    , classifyFailedResponse = failedResponseError
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
