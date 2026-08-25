module Agent.CLI.Runtime.Orchestration.Restart
    ( RestartCallbacks(..)
    , runFullscreenRestartLoop
    ) where

import Agent.CLI.Options ( CliOptions )
import Agent.CLI.ProviderTransition
    ( ProviderTransition(..), TransitionCause(AutomaticFallback) )
import Agent.CLI.Runtime.Types ( PreparedAgent(..), RunResult(..) )
import Agent.CLI.Session.Runtime.Types ( StartupFailure(..) )
import Agent.CLI.TUI.App
    ( FullscreenRuntime
    , emitUiEvent
    , requestFullscreenChoiceWithBody
    , withFullscreenSuspended
    )
import Agent.TUI.Model ( UiEvent(UiSetNotice), progressNotice )
import Agent.Error ( ApiError )
import Control.Exception.Safe ( try )
import Data.Text ( Text )
import qualified Data.Text as Text

data RestartCallbacks = RestartCallbacks
    { restartPrepare
        :: CliOptions
        -> Maybe ProviderTransition
        -> IO PreparedAgent
    , restartFallback
        :: ProviderTransition
        -> ApiError
        -> IO (Maybe ProviderTransition)
    , restartFormatFailure :: ApiError -> IO Text
    , restartOptions :: CliOptions -> Text -> CliOptions
    , restartApplyTransition
        :: CliOptions -> ProviderTransition -> CliOptions
    , restartManageAccounts :: IO ()
    , restartChooseModel
        :: CliOptions
        -> Maybe ProviderTransition
        -> IO (Either Text (Maybe ProviderTransition))
    }

runFullscreenRestartLoop
    :: RestartCallbacks
    -> FullscreenRuntime
    -> CliOptions
    -> Maybe ProviderTransition
    -> IO RunResult
    -> IO RunResult
runFullscreenRestartLoop callbacks runtime =
    loop
  where
    loop options transition action =
        -- The notifier in 'runFullscreen' watches this whole tail-recursive
        -- chain, rather than stopping Brick after the first provider exits.
        try @_ @StartupFailure action >>= \case
            Left (StartupFailure message) ->
                recoverStartup options transition (Text.pack message)
            Right result -> case result of
                RunRestart sessionId -> do
                    let nextOptions = callbacks.restartOptions options sessionId
                    retryStartup nextOptions Nothing
                RunSwitchProvider next -> do
                    let nextOptions =
                            callbacks.restartApplyTransition options next
                    retryStartup nextOptions (Just next)
                RunProviderStartFailed apiError ->
                    case transition of
                        Just failed
                            | failed.transitionCause == AutomaticFallback ->
                                callbacks.restartFallback failed apiError
                                    >>= \case
                                    Just next -> do
                                        let nextOptions =
                                                callbacks.restartApplyTransition
                                                    options next
                                        retryStartup nextOptions (Just next)
                                    Nothing ->
                                        recoverProviderStart
                                            options transition apiError
                        _ ->
                            recoverProviderStart options transition apiError
                other -> pure other

    recoverProviderStart options transition apiError = do
        message <- callbacks.restartFormatFailure apiError
        recoverStartup options transition message

    retryStartup options transition = do
        emitUiEvent runtime $
            UiSetNotice (Just (progressNotice "Retrying startup…"))
        try @_ @StartupFailure
            (callbacks.restartPrepare options transition) >>= \case
                Left (StartupFailure message) ->
                    recoverStartup options transition (Text.pack message)
                Right prepared ->
                    loop options transition prepared.preparedRun

    recoverStartup options transition message = do
        emitUiEvent runtime (UiSetNotice Nothing)
        requestFullscreenChoiceWithBody
            runtime
            "Couldn’t start the agent"
            message
            0
            [ ( "Retry"
              , "Try loading credentials and account usage again"
              )
            , ( "Choose model"
              , "Pick a different model or provider"
              )
            , ( "Manage"
              , "Connect, refresh, enable, or remove provider accounts"
              )
            , ("Exit", "Close the agent")
            ] >>= \case
                Just 0 -> retryStartup options transition
                Just 1 ->
                    callbacks.restartChooseModel options transition >>= \case
                        Left err ->
                            recoverStartup options transition err
                        Right Nothing ->
                            recoverStartup options transition message
                        Right (Just next) ->
                            retryStartup
                                (callbacks.restartApplyTransition options next)
                                (Just next)
                Just 2 -> do
                    withFullscreenSuspended runtime
                        callbacks.restartManageAccounts
                    retryStartup options transition
                _ -> pure RunQuit
