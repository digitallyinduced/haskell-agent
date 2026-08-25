-- | Startup authentication, credential onboarding, and progress reporting.
module Agent.CLI.Startup.Auth
    ( learnAboutUserOnboardingPrompt
    , loadStartupAuth
    , markStartupStage
    , recordStartupTiming
    , setStartupNotice
    , startupDie
    ) where

import Agent.CLI.Auth
    ( LoadedAuth
    , authErrorNeedsOnboarding
    , loadAuth
    )
import Agent.CLI.Login (connectProviderAccount)
import Agent.CLI.Models (ModelTarget(..))
import Agent.CLI.Provider.Switch (loadSelectedAccountAuth)
import Agent.CLI.ProviderTransition (ProviderTransition(..))
import Agent.CLI.Runtime.Types
    ( StartupCancelled(..)
    , StartupFailure(..)
    , StartupRuntime(..)
    )
import Agent.CLI.Terminal (resolveColor)
import Agent.CLI.TUI.App
    ( FullscreenRuntime
    , emitUiEvent
    , requestFullscreenOnboarding
    , withFullscreenSuspended
    )
import Agent.Provider
    ( Provider(..)
    )
import Agent.Store.Postgres.Skill (LearnedSkill(..))
import Agent.TUI.Model
    ( UiEvent(..)
    , progressNotice
    )
import Control.Exception.Safe (throwIO)
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    )
import Data.Maybe
    ( fromMaybe
    , isNothing
    )
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock
    ( NominalDiffTime
    , UTCTime
    , diffUTCTime
    , getCurrentTime
    )
import System.Exit (die)
import System.IO (stderr)

setStartupNotice :: Maybe FullscreenRuntime -> Text -> IO ()
setStartupNotice fullscreen message =
    case fullscreen of
        Nothing -> pure ()
        Just runtime ->
            emitUiEvent runtime
                (UiSetNotice (Just (progressNotice message)))

recordStartupTiming
    :: UTCTime
    -> IORef [(Text, NominalDiffTime)]
    -> Text
    -> IO ()
recordStartupTiming startedAt timingsRef label = do
    elapsed <- (`diffUTCTime` startedAt) <$> getCurrentTime
    atomicModifyIORef' timingsRef \timings ->
        (timings <> [(label, elapsed)], ())

markStartupStage :: StartupRuntime -> Text -> IO ()
markStartupStage startup label = do
    recordStartupTiming startup.startupStartedAt startup.startupTimings label
    setStartupNotice startup.startupFullscreen label

startupDie :: StartupRuntime -> String -> IO a
startupDie startup message =
    case startup.startupFullscreen of
        Nothing -> die message
        Just _ -> throwIO (StartupFailure message)

loadStartupAuth
    :: StartupRuntime
    -> Maybe ProviderTransition
    -> Maybe Provider
    -> IO (LoadedAuth, Bool)
loadStartupAuth startup transition requestedProvider =
    loadTransitionAuth transition requestedProvider >>= \case
        Right loaded -> pure (loaded, False)
        Left err
            | isNothing transition
            , authErrorNeedsOnboarding err
            , Just runtime <- startup.startupFullscreen ->
                runCredentialOnboarding startup runtime
                    >>= \(provider, learnAboutUser) ->
                        loadAuth (Just provider)
                            >>= either
                                (startupDie startup . Text.unpack)
                                (\loaded -> pure (loaded, learnAboutUser))
            | otherwise ->
                startupDie startup (Text.unpack err)

loadTransitionAuth
    :: Maybe ProviderTransition
    -> Maybe Provider
    -> IO (Either Text LoadedAuth)
loadTransitionAuth transition requestedProvider =
    case transition of
        Just active
            | Just selectionId <- active.transitionAccountSelectionId ->
                loadSelectedAccountAuth
                    active.transitionTarget.targetProvider
                    selectionId
                    (fromMaybe selectionId active.transitionAccountId)
        _ -> loadAuth requestedProvider

runCredentialOnboarding
    :: StartupRuntime
    -> FullscreenRuntime
    -> IO (Provider, Bool)
runCredentialOnboarding startup runtime = do
    markStartupStage startup "Choose how to connect…"
    loop
  where
    choices =
        [ ( OpenAIProvider
          , ("Sign in with ChatGPT", "Use an OpenAI subscription")
          )
        , ( XAIProvider
          , ("Sign in with Grok", "Use an xAI subscription")
          )
        , ( OpenRouterProvider
          , ("Add an OpenRouter API key", "Use API credits")
          )
        ]
    loop =
        requestFullscreenOnboarding
            runtime
            "Welcome to haskell-agent"
            "haskell-agent can access AI models with a subscription or API key."
            (map snd choices)
            >>= \case
                Nothing -> throwIO StartupCancelled
                Just index ->
                    case atMay index choices of
                        Nothing -> loop
                        Just (provider, _) -> do
                            connected <-
                                withFullscreenSuspended runtime $
                                    resolveColor stderr >>= \color ->
                                        connectProviderAccount color provider
                            case connected of
                                Nothing -> loop
                                Just _ -> do
                                    markStartupStage startup
                                        "Personalize your agent…"
                                    learnAboutUser <-
                                        requestFullscreenOnboarding
                                            runtime
                                            "Personalize your agent"
                                            "haskell-agent can learn your technical defaults from a confirmed public GitHub profile. You review the profile before anything is saved."
                                            [ ( "Learn from my GitHub"
                                              , "Inspect public repositories and propose a technical profile"
                                              )
                                            , ( "Skip for now"
                                              , "Run /learn-about-user whenever you want"
                                              )
                                            ]
                                    pure (provider, learnAboutUser == Just 0)

learnAboutUserOnboardingPrompt :: [LearnedSkill] -> Maybe Text
learnAboutUserOnboardingPrompt learnedSkills
    | any
        ((== "user-technical-profile") . (.learnedSkillSlug))
        learnedSkills =
            Nothing
    | otherwise =
        Just
            "$learn-about-user Learn my technical preferences from my public GitHub profile, show me the proposed profile, and ask before saving it."

atMay :: Int -> [a] -> Maybe a
atMay index values
    | index < 0 = Nothing
    | otherwise = case drop index values of
        value : _ -> Just value
        [] -> Nothing
