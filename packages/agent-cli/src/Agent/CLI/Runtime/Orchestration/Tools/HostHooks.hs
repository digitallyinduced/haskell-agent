-- | Adapt host/native interactions for the tools without acquiring resources.
module Agent.CLI.Runtime.Orchestration.Tools.HostHooks
    ( ToolHostHooks(..)
    , buildToolHostHooks
    ) where

import Agent.CLI.Options (isOneShot)
import Agent.CLI.Plan (cliPlanHooks)
import Agent.CLI.PromptHooks
    ( fullscreenAwareImageHooks, fullscreenAwarePlanHooks, fullscreenAwareSecretHooks )
import Agent.CLI.Runtime.Orchestration.Tools.Model
import Agent.CLI.Runtime.Orchestration.Tools.Request
import Agent.CLI.Runtime.Orchestration.Types (NativeRunCapabilities(..), NativeRunHooks(..))
import Agent.CLI.Secret (promptSecretLine)
import Agent.CLI.Session.Attachments (putImagePreview)
import Agent.CLI.Session.Runtime.Types (StartupRuntime(..))
import Agent.CLI.SessionState (SessionState(..))
import Agent.CLI.Terminal (resolveColor)
import Agent.Tools.PlanMode (PlanModeHooks(..), PlanDecision(..))
import Agent.Tools.Secret (SecretPrompt(..), SecretPromptHooks(..))
import Agent.Tools.ShowImage (ImageDisplayHooks(..), ImageDisplayRequest(..))

data ToolHostHooks = ToolHostHooks
    { toolPlanHooks :: PlanModeHooks
    , toolSecretHooks :: Maybe SecretPromptHooks
    , toolImageHooks :: Maybe ImageDisplayHooks
    }

buildToolHostHooks
    :: AgentToolsRequest windowTitleResult
    -> ToolStartup
    -> ToolModelRuntime
    -> ToolHostHooks
buildToolHostHooks AgentToolsRequest
    { interrupt
    , stdinControl
    , stderrHandle
    , uiRuntimeRef
    , options
    , isTty
    , startup
    } ToolStartup
    { toolNativeCapabilities = nativeCapabilities
    } ToolModelRuntime
    { toolProvider = provider
    } =
    ToolHostHooks{..}
  where
    basePlanHooks
        | Just hooks <- startup.startupNativeHooks =
            hooks.nativePlanHooks
        | startup.startupBackground =
            PlanModeHooks
                { planConfirmEnter = \_ -> pure False
                , planDecideExit = \_ -> pure PlanCancel
                , planAskQuestion = \_ _ -> pure Nothing
                }
        | otherwise =
            cliPlanHooks
                provider interrupt stdinControl (resolveColor stderrHandle)
    toolPlanHooks = fullscreenAwarePlanHooks uiRuntimeRef basePlanHooks
    baseSecretHooks = SecretPromptHooks \request ->
        Right <$> promptSecretLine
            stdinControl
            request.secretPromptMessage
            request.secretPromptPurpose
    toolSecretHooks
        | not nativeCapabilities.nativeHostExtensions
            || isOneShot options || not isTty = Nothing
        | otherwise =
            Just (fullscreenAwareSecretHooks uiRuntimeRef baseSecretHooks)
    -- Outside the retained TUI, agent-displayed images print inline with the
    -- same graphics path as pasted attachments.
    baseImageHooks = ImageDisplayHooks \request -> do
        color <- resolveColor stderrHandle
        putImagePreview
            startup.startupSessionState.sessionPreviewId
            color
            [request.displayImage]
        pure (Right ())
    toolImageHooks
        | not nativeCapabilities.nativeHostExtensions || not isTty =
            Nothing
        | otherwise =
            Just (fullscreenAwareImageHooks uiRuntimeRef baseImageHooks)
