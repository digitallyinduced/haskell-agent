-- | Typed decoding and terminal-response assembly for OpenRouter Responses SSE.
module Agent.OpenRouter.Stream
    ( buildResponse
    ) where

import Agent.Error (ApiError(..))
import Agent.Responses.StreamAssembly
    ( StreamAssemblyConfig(..)
    , buildStreamResponse
    , failedStreamResponseMessage
    )
import Agent.Responses.Types
import Agent.OpenRouter.Error (classifyStreamError)

-- | Merge streamed output-item events into the terminal completed response.
buildResponse :: [ResponseStreamEvent] -> Either ApiError Response
buildResponse = buildStreamResponse StreamAssemblyConfig
    { missingCompletionMessage =
        "No terminal response event found in OpenRouter SSE stream"
    , classifyStreamError
    , classifyFailedResponse =
        ConnectionError . failedStreamResponseMessage
    }
