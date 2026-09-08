-- | Shell permission requests are untrusted input; execution authorization is
-- supplied separately by the host's fresh-confirmation dispatch boundary.
module Agent.Tools.ShellPermission
    ( ShellPermissionRequest(..)
    , shellPermissionFieldsDecoder
    , shellPermissionApproval
    , ShellExecutionAuthorization
    , shellExecutionAuthorization
    , defaultShellExecutionAuthorization
    , shellExecutionIsEscalated
    , consumeShellExecutionAuthorization
    ) where

import qualified Agent.Json.Decode as Json
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolInvocationAuthorization
    , consumeToolInvocationAuthorization
    , decodeToolArguments
    )
import Agent.Tools.Types (ApprovalRequirement(..))
import Data.Text (Text)
import qualified Data.Text as Text

data ShellPermissionRequest
    = UseDefaultSandbox
    | RequireEscalatedSandbox !Text
    deriving (Eq, Show)

shellPermissionFieldsDecoder :: Json.FieldsDecoder ShellPermissionRequest
shellPermissionFieldsDecoder = do
    permission <- Json.atKeyOptional "sandbox_permissions" Json.text
    justification <- Json.atKeyOptional "justification" Json.text
    case permission of
        Nothing -> pure UseDefaultSandbox
        Just "use_default" -> pure UseDefaultSandbox
        Just "require_escalated" ->
            case justification of
                Just reason | not (Text.null (Text.strip reason)) ->
                    pure (RequireEscalatedSandbox reason)
                _ -> fail "require_escalated requires a nonblank justification"
        Just _ -> fail "sandbox_permissions must be use_default or require_escalated"

shellPermissionApproval :: ToolCall -> IO ApprovalRequirement
shellPermissionApproval call =
    pure $ case decodeToolArguments (Json.object shellPermissionFieldsDecoder) call.arguments of
        -- Resource-claim classification is not an authorization boundary:
        -- apparently read-only commands can execute configured programs.
        Right UseDefaultSandbox -> ApprovalPromptRequired
        -- Malformed requests fail closed and are rejected again by the handler.
        _ -> FreshApprovalRequired

data ShellExecutionAuthorization
    = DefaultShellExecution
    | EscalatedShellExecution !ToolInvocationAuthorization

defaultShellExecutionAuthorization :: ShellExecutionAuthorization
defaultShellExecutionAuthorization = DefaultShellExecution

shellExecutionAuthorization
    :: Maybe ToolInvocationAuthorization
    -> ShellPermissionRequest
    -> Either Text ShellExecutionAuthorization
shellExecutionAuthorization _ UseDefaultSandbox = Right DefaultShellExecution
shellExecutionAuthorization (Just authorization) RequireEscalatedSandbox{} =
    Right (EscalatedShellExecution authorization)
shellExecutionAuthorization Nothing RequireEscalatedSandbox{} =
    Left "Sandbox escalation requires fresh user approval for this exact invocation."

shellExecutionIsEscalated :: ShellExecutionAuthorization -> Bool
shellExecutionIsEscalated DefaultShellExecution = False
shellExecutionIsEscalated EscalatedShellExecution{} = True

-- | A launch consumes the capability. Reuse or use after handler completion is
-- rejected. Default execution never consumes an escalation capability.
consumeShellExecutionAuthorization :: ShellExecutionAuthorization -> IO Bool
consumeShellExecutionAuthorization DefaultShellExecution = pure True
consumeShellExecutionAuthorization (EscalatedShellExecution authorization) =
    consumeToolInvocationAuthorization authorization
