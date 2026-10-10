-- | Adapt host/native interactions for the tools without acquiring resources.
module Agent.CLI.Runtime.Orchestration.Tools.HostHooks
    ( ToolHostHooks(..)
    , buildToolHostHooks
    , terminalChartTool
    ) where

import Agent.CLI.CancelWatch (withStdinPaused)
import Agent.CLI.Notification (AttentionRequest(InputRequested), notifyAttention)
import Agent.CLI.Render (renderAssistantTextForHandle)
import Agent.CLI.TUI.App (emitUiEvent)
import Agent.TUI.Model (UiEvent(UiAssistantHistory))
import Agent.Loop (LoopEvent(TextDelta))
import Agent.CLI.Options (isOneShot)
import Data.IORef (readIORef)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Agent.CLI.ChartImage (terminalChartTool)
import Agent.CLI.ImagePreview (routeChartPresentation)
import Agent.CLI.Plan (cliPlanHooks)
import Agent.CLI.PromptHooks
    ( fullscreenAwareImageHooks, fullscreenAwarePlanHooks, fullscreenAwareSecretHooks )
import Agent.CLI.Runtime.Orchestration.Tools.Model
import Agent.CLI.Runtime.Orchestration.Tools.Request
import Agent.CLI.Runtime.Orchestration.Types (NativeRunCapabilities(..), NativeRunHooks(..))
import Agent.CLI.Secret (promptSecretLine)
import Agent.CLI.Session.Attachments (putImagePreview, putChartPreview)
import Agent.CLI.Session.Runtime.Types (StartupRuntime(..))
import Agent.CLI.SessionState (SessionState(..))
import Agent.CLI.Terminal (resolveColor)
import Agent.Tools.PlanMode (PlanModeHooks(..), PlanDecision(..))
import Agent.Tools.Secret (SecretPrompt(..), SecretPromptHooks(..))
import Agent.Tools.ShowImage (ImageDisplayHooks(..), ImageDisplayRequest(..))
import System.IO (stdout)

data ToolHostHooks = ToolHostHooks
    { toolPlanHooks :: PlanModeHooks
    , toolSecretHooks :: Maybe SecretPromptHooks
    , toolImageHooks :: Maybe ImageDisplayHooks
    , toolAsyncQuestionDelivery :: Maybe (Text -> IO (Either Text ()))
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
    toolAsyncQuestionDelivery
        | Just hooks <- startup.startupNativeHooks =
            Just \message -> do
                hooks.nativeOnLoopEvent (TextDelta ("\n\n" <> message <> "\n\n"))
                pure (Right ())
        | startup.startupBackground || isOneShot options || not isTty = Nothing
        | otherwise = Just \message -> do
            notifyAttention stderrHandle InputRequested
            readIORef uiRuntimeRef >>= \case
                Just runtime -> emitUiEvent runtime (UiAssistantHistory message)
                Nothing -> withStdinPaused stdinControl do
                    color <- resolveColor stderrHandle
                    rendered <- renderAssistantTextForHandle stderrHandle color message
                    Text.hPutStrLn stderrHandle ("\n" <> rendered)
            pure (Right ())
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
        if request.displayPath == "render_chart"
            then routeChartPresentation (isOneShot options) stdout stderrHandle
                request.displayCaption
                (putChartPreview
                    startup.startupSessionState.sessionPreviewId
                    color
                    [request.displayImage])
            else putImagePreview
                startup.startupSessionState.sessionPreviewId
                color
                [request.displayImage]
        pure (Right ())
    toolImageHooks
        | not nativeCapabilities.nativeHostExtensions || not isTty =
            Nothing
        | otherwise =
            Just (fullscreenAwareImageHooks uiRuntimeRef baseImageHooks)
