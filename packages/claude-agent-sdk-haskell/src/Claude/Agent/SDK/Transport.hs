-- | Low-level transport interface, analogous to the public @Transport@
-- abstraction in Anthropic's Python SDK.
module Claude.Agent.SDK.Transport
    ( Transport(..)
    , TransportFactory
    , TransportMode(..)
    , TransportRequest(..)
    ) where

import Claude.Agent.SDK.Errors (ClaudeSDKError)
import Data.ByteString (ByteString)
import Data.Text (Text)
import System.Exit (ExitCode)

data TransportMode
    = TransportNew !Text
    | TransportResume !Text
    | TransportContinue
    deriving (Eq, Show)

-- | Parameters supplied whenever the client needs a new transport. A factory
-- may therefore support fresh sessions, resumed sessions, and process
-- restarts without exposing the SDK's internal subprocess state machine.
data TransportRequest = TransportRequest
    { transportMode :: !TransportMode
    , transportModel :: !(Maybe Text)
    , transportEffort :: !(Maybe Text)
    } deriving (Eq, Show)

-- | Create one disconnected transport for a client start or restart. The SDK
-- calls 'transportConnect' and owns closing the returned transport.
type TransportFactory = TransportRequest -> IO Transport

-- | Raw newline-delimited JSON transport. Custom transports can implement
-- remote Claude Code connections without changing the client/query layers.
data Transport = Transport
    { transportConnect :: IO (Either ClaudeSDKError ())
    , transportWrite :: ByteString -> IO (Either ClaudeSDKError ())
    , transportRead :: IO (Either ClaudeSDKError (Maybe ByteString))
    , transportClose :: IO ()
    , transportIsReady :: IO Bool
    , transportEndInput :: IO ()
    , transportProcessExit :: IO (Maybe ExitCode)
    , transportDiagnostic :: IO Text
    }
