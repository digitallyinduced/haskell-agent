module Agent.CLI.Login.Internal.Gateway
    ( GatewayLoginFlow(..)
    , connectFullscreenGateway
    , connectTerminalGateway
    , selectGatewayLoginFlow
    ) where

import Agent.CLI.Environment (lookupNonEmpty)
import Agent.CLI.Error (formatException)
import Agent.CLI.GatewayClient
    ( GatewayAuthorization(..)
    , GatewayCredential(..)
    , GatewayDeviceAuthorization(..)
    , GatewayPollResult(..)
    , connectGateway
    , connectGatewayBrowserWithCancel
    , defaultGatewayBaseUrl
    , pollGatewayAuthorization
    , saveGatewayCredential
    , startGatewayAuthorization
    )
import Agent.CLI.Input (readApprovalLine)
import Agent.CLI.Login.Internal.Browser (openBrowser)
import Agent.CLI.Login.Internal.Dashboard
    ( LoginNotice(..)
    , withLoginProgress
    )
import Agent.CLI.Login.Internal.Device
    ( DevicePollReadiness(..)
    , advanceGatewayPollSchedule
    , authorizationPendingNotice
    , deviceAuthorizationBody
    , devicePollReadiness
    , initialDevicePollSchedule
    , pollWaitNotice
    )
import Agent.CLI.Login.Internal.ProviderConnection
    ( printLoginMessage
    , printLoginResult
    )
import Agent.CLI.Style (roleMuted, rolePrompt)
import Agent.CLI.TUI.App
    ( FullscreenRuntime
    , requestFullscreenChoiceWithBody
    )
import Control.Concurrent.Async (Async, poll, race, wait, withAsync)
import Control.Concurrent.MVar
    ( MVar
    , newEmptyMVar
    , putMVar
    , takeMVar
    )
import Control.Exception.Safe (onException, tryAny)
import Control.Monad (unless)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (getCurrentTime)
import System.IO (hFlush, stderr)
import qualified System.Info as SystemInfo

-- | The interactive gateway authorization appropriate for the current host.
-- A macOS process reached through SSH is headless from the user's point of
-- view, so it uses the same device flow as a remote Linux server.
data GatewayLoginFlow
    = GatewayBrowserOAuth
    | GatewayDeviceFlow
    deriving (Eq, Show)

selectGatewayLoginFlow :: String -> Bool -> GatewayLoginFlow
selectGatewayLoginFlow operatingSystem remoteSession
    | operatingSystem == "darwin" && not remoteSession =
        GatewayBrowserOAuth
    | otherwise =
        GatewayDeviceFlow

currentGatewayLoginFlow :: IO GatewayLoginFlow
currentGatewayLoginFlow = do
    sshConnection <- lookupNonEmpty "SSH_CONNECTION"
    sshClient <- lookupNonEmpty "SSH_CLIENT"
    sshTty <- lookupNonEmpty "SSH_TTY"
    pure $
        selectGatewayLoginFlow
            SystemInfo.os
            (any isJust [sshConnection, sshClient, sshTty])

connectFullscreenGateway :: FullscreenRuntime -> IO (Maybe LoginNotice)
connectFullscreenGateway runtime = do
    currentGatewayLoginFlow >>= \case
        GatewayBrowserOAuth ->
            connectFullscreenGatewayBrowser runtime
        GatewayDeviceFlow ->
            connectFullscreenGatewayDevice runtime

connectFullscreenGatewayBrowser
    :: FullscreenRuntime
    -> IO (Maybe LoginNotice)
connectFullscreenGatewayBrowser runtime = do
    result <-
        withLoginProgress runtime "Waiting for platform gateway login…" $
            runFullscreenGatewayBrowser runtime
    pure $
        case result of
            Left err
                | gatewayBrowserAuthorizationCancelled err ->
                    Nothing
                | otherwise ->
                    Just $
                        LoginNotice False
                            ("Gateway connection failed: " <> err)
            Right () ->
                Just $
                    LoginNotice True
                        "Platform gateway connected. Restart the agent to apply the new route immediately."

runFullscreenGatewayBrowser
    :: FullscreenRuntime
    -> IO (Either Text ())
runFullscreenGatewayBrowser runtime = do
    presentation <- newEmptyMVar
    cancellation <- newEmptyMVar
    withAsync
        (connectGatewayBrowserWithCancel
            defaultGatewayBaseUrl
            "Haskell Agent CLI"
            (presentGatewayBrowserLogin presentation)
            (takeMVar cancellation))
        \authorization ->
            race
                (takeMVar presentation)
                (waitGatewayBrowserAuthorization authorization)
                >>= \case
                    Left details ->
                        awaitFullscreenGatewayBrowser
                            runtime
                            authorization
                            cancellation
                            details
                    Right result -> pure result

presentGatewayBrowserLogin
    :: MVar (Text, Bool)
    -> Text
    -> IO Bool
presentGatewayBrowserLogin presentation authorizationUrl = do
    opened <- openBrowser authorizationUrl
    putMVar presentation (authorizationUrl, opened)
    pure True

awaitFullscreenGatewayBrowser
    :: FullscreenRuntime
    -> Async (Either Text ())
    -> MVar ()
    -> (Text, Bool)
    -> IO (Either Text ())
awaitFullscreenGatewayBrowser
    runtime authorization cancellation (authorizationUrl, opened) =
        pollGatewayBrowserAuthorization authorization >>= \case
            Just result -> pure result
            Nothing -> do
                choice <-
                    requestFullscreenChoiceWithBody
                        runtime
                        "Log in to platform gateway"
                        (Text.intercalate "\n\n"
                            [ if opened
                                then
                                    "A browser window was opened automatically. Approve access there, then return here."
                                else
                                    "The browser could not be opened automatically. [Open the platform login page]("
                                        <> authorizationUrl
                                        <> "), then return here."
                            , "Select Check connection after approving. Cancel or Esc stops waiting for the local callback."
                            ])
                        0
                        [ ( "Check connection"
                          , "See whether browser authorization completed"
                          )
                        , ( "Cancel"
                          , "Stop without changing the saved gateway"
                          )
                        ]
                case choice of
                    Just 0 ->
                        awaitFullscreenGatewayBrowser
                            runtime
                            authorization
                            cancellation
                            (authorizationUrl, opened)
                    _ -> do
                        putMVar cancellation ()
                        waitGatewayBrowserAuthorization authorization

pollGatewayBrowserAuthorization
    :: Async (Either Text ())
    -> IO (Maybe (Either Text ()))
pollGatewayBrowserAuthorization authorization =
    poll authorization >>= \case
        Nothing -> pure Nothing
        Just (Left _) ->
            pure $
                Just $
                    Left
                        "Gateway browser authorization failed unexpectedly."
        Just (Right result) -> pure (Just result)

waitGatewayBrowserAuthorization
    :: Async (Either Text ())
    -> IO (Either Text ())
waitGatewayBrowserAuthorization authorization =
    wait authorization

connectFullscreenGatewayDevice
    :: FullscreenRuntime
    -> IO (Maybe LoginNotice)
connectFullscreenGatewayDevice runtime = do
    requestedAt <- getCurrentTime
    requested <-
        withLoginProgress runtime
            "Starting gateway device authorization…" $
                startGatewayAuthorization defaultGatewayBaseUrl
    case requested of
        Left err ->
            pure $ Just $
                LoginNotice False
                    ("Gateway connection failed: " <> err)
        Right authorization -> do
            let device =
                    authorization.authorizationDevice
                notice =
                    Just
                        "Open the verification link in a browser on your local computer."
            awaitAuthorization
                authorization
                (initialDevicePollSchedule
                    requestedAt
                    device.pollIntervalSeconds
                    device.expiresInSeconds)
                notice
  where
    awaitAuthorization authorization schedule notice = do
        let device = authorization.authorizationDevice
        choice <-
            requestFullscreenChoiceWithBody
                runtime
                "Connect gateway"
                (deviceAuthorizationBody
                    "gateway"
                    device.verificationUriComplete
                    device.userCode
                    notice)
                0
                [ ( "Check authorization"
                  , "Return after approving the one-time code in your browser"
                  )
                , ("Cancel", "Stop without changing the saved gateway")
                ]
        case choice of
            Just 0 -> do
                now <- getCurrentTime
                case devicePollReadiness now schedule of
                    DevicePollExpired ->
                        gatewayTimedOut
                    DevicePollWait seconds ->
                        awaitAuthorization
                            authorization
                            schedule
                            (Just (pollWaitNotice seconds))
                    DevicePollReady -> do
                        polled <-
                            withLoginProgress runtime
                                "Checking gateway authorization…" $
                                    pollGatewayAuthorization authorization
                        polledAt <- getCurrentTime
                        case polled of
                            Left err ->
                                pure $ Just $
                                    LoginNotice False
                                        ("Gateway connection failed: " <> err)
                            Right result ->
                                handlePoll
                                    authorization
                                    schedule
                                    polledAt
                                    result
            _ -> pure Nothing

    handlePoll authorization schedule polledAt = \case
        GatewayAuthorized accessToken websocketUrl
            | devicePollReadiness polledAt schedule
                == DevicePollExpired ->
                    gatewayTimedOut
            | otherwise -> do
                saved <-
                    saveGatewayCredential GatewayCredential
                        { gatewayBaseUrl =
                            authorization.authorizationBaseUrl
                        , gatewayWebSocketUrl = websocketUrl
                        , gatewayAccessToken = accessToken
                        }
                pure $ Just $ case saved of
                    Left err ->
                        LoginNotice False
                            ("Could not save gateway connection: " <> err)
                    Right () ->
                        LoginNotice True
                            "Gateway connected. Restart the agent to apply the new route immediately."
        GatewayAuthorizationPending serverInterval ->
            continuePending
                authorization
                polledAt
                (advanceGatewayPollSchedule
                    False serverInterval polledAt schedule)
                authorizationPendingNotice
        GatewaySlowDown serverInterval ->
            let nextSchedule =
                    advanceGatewayPollSchedule
                        True serverInterval polledAt schedule
                seconds = case devicePollReadiness polledAt nextSchedule of
                    DevicePollWait waitSeconds -> waitSeconds
                    _ -> 0
            in continuePending
                authorization
                polledAt
                nextSchedule
                ( "The gateway asked this client to slow down. "
                    <> "The next check is available in "
                    <> Text.pack (show seconds)
                    <> " seconds."
                )
        GatewayAccessDenied ->
            pure $ Just $
                LoginNotice False "Gateway authorization was denied."
        GatewayExpired ->
            gatewayTimedOut
        GatewayPollFailed code ->
            pure $ Just $
                LoginNotice False
                    ("Gateway authorization failed: " <> code)

    continuePending authorization polledAt nextSchedule notice
        | devicePollReadiness polledAt nextSchedule
            == DevicePollExpired =
                gatewayTimedOut
        | otherwise =
            awaitAuthorization
                authorization
                nextSchedule
                (Just notice)

    gatewayTimedOut =
        pure $ Just $
            LoginNotice False "Gateway device authorization timed out."

connectTerminalGateway :: Bool -> IO ()
connectTerminalGateway color = do
    currentGatewayLoginFlow >>= \case
        GatewayBrowserOAuth -> do
            result <-
                connectGatewayBrowserWithCancel
                    defaultGatewayBaseUrl
                    "Haskell Agent CLI"
                    (\authorizationUrl -> do
                        Text.hPutStrLn stderr $
                            roleMuted color "Open "
                                <> rolePrompt color authorizationUrl
                        opened <- openBrowser authorizationUrl
                        unless opened $
                            Text.hPutStrLn stderr $
                                roleMuted color
                                    "Could not launch a browser automatically; open the URL above."
                        Text.hPutStrLn stderr $
                            roleMuted color
                                "Waiting for platform gateway authorization…"
                        hFlush stderr
                        -- Printing the complete link is a valid presentation
                        -- fallback even when automatic browser launch fails.
                        pure True)
                    (waitForTerminalGatewayBrowserCancellation color)
            case result of
                Left err
                    | gatewayBrowserAuthorizationCancelled err ->
                        Text.hPutStrLn stderr $
                            roleMuted color
                                "Gateway authorization cancelled."
                _ ->
                    printLoginResult color $
                        fmap
                            (const
                                "Platform gateway connected. Restart the agent to apply the new route immediately.")
                            result
        GatewayDeviceFlow ->
            tryAny (connectGateway defaultGatewayBaseUrl) >>= \case
                Left exception ->
                    printLoginMessage color False
                        ("Gateway connection failed: "
                            <> formatException exception)
                Right () ->
                    pure ()

waitForTerminalGatewayBrowserCancellation :: Bool -> IO ()
waitForTerminalGatewayBrowserCancellation color =
    loop `onException` Text.hPutStrLn stderr ""
  where
    loop =
        readApprovalLine
            (roleMuted color
                "Press Esc or q to cancel gateway authorization. ")
            >>= \case
                Nothing -> pure ()
                Just answer
                    | Text.toLower answer `elem` ["q", "\ESC"] ->
                        pure ()
                    | otherwise -> loop

gatewayBrowserAuthorizationCancelled :: Text -> Bool
gatewayBrowserAuthorizationCancelled =
    (== "Gateway browser authorization was cancelled.")
