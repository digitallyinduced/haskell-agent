-- | Adapt plan-mode and secret prompts to the currently active fullscreen UI.
module Agent.CLI.PromptHooks
    ( fullscreenAwarePlanHooks
    , fullscreenAwareSecretHooks
    ) where

import Agent.CLI.Secret (sanitizeSecretPromptText)
import Agent.CLI.TUI.App
    ( FullscreenRuntime
    , requestFullscreenChoiceWithBody
    , requestFullscreenSecret
    , requestFullscreenText
    )
import Agent.Tools.PlanMode
    ( PlanDecision(..)
    , PlanModeHooks(..)
    )
import Agent.Tools.Secret
    ( SecretPrompt(..)
    , SecretPromptHooks(..)
    )
import Data.IORef (IORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

fullscreenAwarePlanHooks
    :: IORef (Maybe FullscreenRuntime)
    -> PlanModeHooks
    -> PlanModeHooks
fullscreenAwarePlanHooks runtimeRef hooks = PlanModeHooks
    { planConfirmEnter = \reason ->
        withCurrentFullscreen runtimeRef
            (hooks.planConfirmEnter reason)
            \runtime ->
                requestFullscreenChoiceWithBody
                    runtime
                    "Enter plan mode?"
                    reason
                    0
                    [ ("Enter plan mode", "Explore and design before implementing")
                    , ("Stay in normal mode", "Continue without entering plan mode")
                    ]
                    >>= pure . (== Just 0)
    , planDecideExit = \planBody ->
        withCurrentFullscreen runtimeRef
            (hooks.planDecideExit planBody)
            \runtime ->
                requestFullscreenChoiceWithBody
                    runtime
                    "Ready to implement this plan?"
                    planBody
                    0
                    [ ("Approve and implement", "Leave plan mode and start implementation")
                    , ("Request changes", "Send feedback and keep planning")
                    , ("Cancel plan", "Leave plan mode without implementing")
                    ]
                    >>= \case
                        Just 0 -> pure PlanApprove
                        Just 1 ->
                            requestFullscreenText
                                runtime
                                "Request changes"
                                "What should be changed in the plan?"
                                ""
                                >>= pure
                                    . PlanRequestChanges
                                    . normalizePlanNotes
                        _ -> pure PlanCancel
    , planAskQuestion = \question options ->
        withCurrentFullscreen runtimeRef
            (hooks.planAskQuestion question options)
            \runtime -> case options of
                [] ->
                    requestFullscreenText
                        runtime
                        "Planning question"
                        question
                        ""
                        >>= pure . nonBlank
                choices ->
                    requestFullscreenChoiceWithBody
                        runtime
                        "Planning question"
                        question
                        0
                        [(choice, "") | choice <- choices]
                        >>= pure . (>>= (`atMay` choices))
    }

fullscreenAwareSecretHooks
    :: IORef (Maybe FullscreenRuntime)
    -> SecretPromptHooks
    -> SecretPromptHooks
fullscreenAwareSecretHooks runtimeRef hooks =
    SecretPromptHooks \request ->
        withCurrentFullscreen runtimeRef
            (hooks.promptSecret request)
            \runtime ->
                Right <$> requestFullscreenSecret
                    runtime
                    "Secret requested by agent"
                    (secretRequestBody request)

secretRequestBody :: SecretPrompt -> Text
secretRequestBody request =
    Text.intercalate "\n\n" $
        filter (not . Text.null)
            [ maybe ""
                (\purpose ->
                    "Purpose: "
                        <> sanitizeSecretPromptText (Text.strip purpose))
                request.secretPromptPurpose
            , sanitizeSecretPromptText
                (Text.strip request.secretPromptMessage)
            , "Input is hidden and is not added to conversation history."
            ]

withCurrentFullscreen
    :: IORef (Maybe FullscreenRuntime)
    -> IO a
    -> (FullscreenRuntime -> IO a)
    -> IO a
withCurrentFullscreen runtimeRef fallback fullscreenAction = do
    runtime <- readIORef runtimeRef
    case runtime of
        Nothing -> fallback
        Just active -> fullscreenAction active

normalizePlanNotes :: Maybe Text -> Text
normalizePlanNotes =
    fromMaybe "(no notes)" . nonBlank

nonBlank :: Maybe Text -> Maybe Text
nonBlank =
    (>>= \text ->
        let stripped = Text.strip text
        in if Text.null stripped then Nothing else Just stripped)

atMay :: Int -> [a] -> Maybe a
atMay index values
    | index < 0 = Nothing
    | otherwise = case drop index values of
        value : _ -> Just value
        [] -> Nothing
