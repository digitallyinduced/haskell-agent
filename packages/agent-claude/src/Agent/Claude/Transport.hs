module Agent.Claude.Transport
    ( ClaudeCodeTransport(..)
    , anthropicGatewayBaseUrl
    ) where

import Data.Text (Text)
import qualified Data.Text as Text

-- | Where the local Claude Code subprocess sends model requests.
--
-- The gateway token is deliberately redacted from 'Show'. The subprocess and
-- all of its tools remain local in both modes.
data ClaudeCodeTransport
    = ClaudeCodeLocalSubscription
    | ClaudeCodeGateway
        { gatewayBaseUrl :: !Text
        , gatewayToken :: !Text
        }
    deriving (Eq)

instance Show ClaudeCodeTransport where
    show ClaudeCodeLocalSubscription = "ClaudeCodeLocalSubscription"
    show ClaudeCodeGateway{gatewayBaseUrl} =
        "ClaudeCodeGateway { gatewayBaseUrl = "
            <> show gatewayBaseUrl
            <> ", gatewayToken = <redacted> }"

-- | Claude Code appends @/v1/messages@ to this provider base URL.
anthropicGatewayBaseUrl :: Text -> Text
anthropicGatewayBaseUrl base =
    Text.dropWhileEnd (== '/') (Text.strip base) <> "/anthropic"
