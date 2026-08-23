-- | Request normalization for the ChatGPT Codex transport.
module Agent.OpenAI.Request
    ( sanitizeCodexRequest
    ) where

import Agent.Responses.Types (ResponseCreateParams(..))

-- | Keep prompt-cache retention out of Codex requests.
--
-- Retention is provider-managed for this transport. Sending a retained value
-- explicitly is model-dependent and can make an otherwise valid request fail
-- after the provider routes a response chain to a different model.
sanitizeCodexRequest :: ResponseCreateParams -> ResponseCreateParams
sanitizeCodexRequest ResponseCreateParams{promptCacheRetention = _, ..} =
    ResponseCreateParams
        { promptCacheRetention = Nothing
        , ..
        }
