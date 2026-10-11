-- | Aeson-valued compatibility view of the shared worker protocol.
-- Wire validation and encoding remain in "Agent.Tools.CodeMode.Protocol".
module Agent.Tools.CodeMode.Protocol.Value
    ( CodeModeToolMetadata(..), ProtocolMessage(..), ToolInvocation(..)
    , decodeProtocolMessage, encodeExecRequest, encodeExecRequestWithState
    , encodeExecRequestWithStateAndImageDetail, encodeToolFailure, encodeToolSuccess
    ) where

import Agent.Tools.CodeMode.Protocol
    ( CodeModeToolMetadata(..), encodeExecRequest, encodeExecRequestWithState
    , encodeExecRequestWithStateAndImageDetail, encodeToolFailure, encodeToolSuccess )
import qualified Agent.Tools.CodeMode.Protocol as Protocol
import Data.Aeson (Value, toJSON)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import Data.Text (Text)

data ToolInvocation = ToolInvocation
    { invocationId :: !Text
    , invocationName :: !Text
    , invocationArguments :: !Value
    } deriving (Eq, Show)

data ProtocolMessage
    = WorkerReady
    | WorkerToolInvocation !ToolInvocation
    | WorkerYielded { responseValue :: !Value }
    | WorkerNotification { notificationText :: !Text }
    | WorkerContent { contentValue :: !Value }
    | WorkerExecSucceeded
        { responseId :: !Text
        , responseValue :: !Value
        , responseStoredValueWrites :: !(Map Text Value)
        }
    | WorkerExecFailed
        { responseId :: !Text
        , responseError :: !Text
        , responseValue :: !Value
        , responseStoredValueWrites :: !(Map Text Value)
        }
    deriving (Eq, Show)

decodeProtocolMessage :: BS.ByteString -> Either Text ProtocolMessage
decodeProtocolMessage = fmap fromProtocol . Protocol.decodeProtocolMessage
  where
    fromProtocol = \case
        Protocol.WorkerReady -> WorkerReady
        Protocol.WorkerToolInvocation invocation -> WorkerToolInvocation ToolInvocation
            { invocationId = invocation.invocationId
            , invocationName = invocation.invocationName
            , invocationArguments = toJSON invocation.invocationArguments
            }
        Protocol.WorkerYielded value -> WorkerYielded (toJSON value)
        Protocol.WorkerNotification message -> WorkerNotification message
        Protocol.WorkerContent value -> WorkerContent (toJSON value)
        Protocol.WorkerExecSucceeded identifier value writes ->
            WorkerExecSucceeded identifier (toJSON value) (fmap toJSON writes)
        Protocol.WorkerExecFailed identifier message value writes ->
            WorkerExecFailed identifier message (toJSON value) (fmap toJSON writes)
