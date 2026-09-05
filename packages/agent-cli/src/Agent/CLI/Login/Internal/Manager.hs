module Agent.CLI.Login.Internal.Manager
    ( runFullscreenLoginManager
    , runLoginManager
    ) where

import Agent.CLI.Input (readApprovalLine)
import Agent.CLI.Error (formatException)
import Agent.CLI.Login.Internal.Accounts
    ( discoverLoginAccounts
    , disconnectLoginAccount
    , importLoginAccount
    , isGatewayLoginAccount
    , refreshLoginAccount
    , toggleLoginAccount
    )
import Agent.CLI.Login.Internal.Dashboard
    ( LoginAccountMenuAction(..)
    , LoginDashboardAction(..)
    , accountMenuTitle
    , confirmFullscreenDisconnect
    , loginAccountBody
    , loginAccountMenuEntries
    , loginDashboardBody
    , loginDashboardEntries
    , noticeFromResult
    , refreshAllNotice
    , refreshOneNotice
    , withLoginProgress
    )
import Agent.CLI.Login.Internal.Formatting
    ( formatLoginAccounts
    , renderLoginFrame
    )
import Agent.CLI.Login.Internal.Gateway
    ( connectFullscreenGateway
    , connectTerminalGateway
    )
import Agent.CLI.Login.Internal.ProviderConnection
    ( connectAccount
    , connectFullscreenAccount
    , printLoginResult
    )
import Agent.CLI.Login.Types
    ( LoginAccount(..)
    , LoginAction(..)
    , LoginState(..)
    , UsageState(..)
    , applyLoginKey
    , initialLoginState
    )
import Agent.CLI.Picker (runOverlayWithUpdates)
import Agent.CLI.TUI.App
    ( FullscreenRuntime
    , requestFullscreenChoiceWithBody
    )
import Control.Applicative ((<|>))
import Control.Concurrent.Async
    ( mapConcurrently
    , mapConcurrently_
    , withAsync
    )
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception.Safe (tryAny)
import Control.Monad (void)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO (hFlush, hIsTerminalDevice, stderr, stdin)

runLoginManager :: Bool -> IO ()
runLoginManager color = do
    accounts <- discoverLoginAccounts
    tty <- hIsTerminalDevice stdin
    if not tty
        then do
            refreshed <- mapConcurrently refreshLoginAccount accounts
            Text.hPutStrLn stderr (formatLoginAccounts color refreshed)
            hFlush stderr
        else loop [0 .. length accounts - 1] (initialLoginState accounts)
  where
    loop refreshIndices state = do
        updates <- newChan
        result <- withAsync
            (refreshSelectedAccounts updates refreshIndices state.loginAccounts)
            \_ ->
                runOverlayWithUpdates
                    (renderLoginFrame color)
                    applyLoginKey
                    (readChan updates)
                    applyRefreshedAccount
                    state
        case result of
            Nothing -> pure ()
            Just (LoginClose, _) -> pure ()
            Just (LoginRefresh index, state') ->
                loop [index] state'
            Just (LoginAdd, _) -> do
                void (connectAccount color)
                rediscover
            Just (LoginGateway, _) -> do
                connectTerminalGateway color
                rediscover
            Just (LoginToggle index, state') -> do
                toggleAt color index state'.loginAccounts
                rediscover
            Just (LoginDelete index, state') -> do
                deleteAt color index state'.loginAccounts
                rediscover
            Just (LoginImport index, state') -> do
                importAt color index state'.loginAccounts
                rediscover
      where
        rediscover = do
            accounts <- discoverLoginAccounts
            loop [0 .. length accounts - 1] (initialLoginState accounts)

-- | Native fullscreen credential manager used by @/login@. Every prompt is a
-- Brick overlay, so the application stays in the alternate screen and
-- provider tokens never pass through the ordinary line editor.
runFullscreenLoginManager :: FullscreenRuntime -> IO ()
runFullscreenLoginManager runtime = do
    accounts <- discoverLoginAccounts
    dashboardLoop Nothing accounts
  where
    dashboardLoop notice accounts = do
        let entries = loginDashboardEntries accounts
        choice <-
            requestFullscreenChoiceWithBody
                runtime
                "Provider accounts"
                (loginDashboardBody notice accounts)
                0
                (map snd entries)
        case choice >>= (`accountAt` entries) of
            Nothing -> pure ()
            Just (LoginDashboardConnect, _) -> do
                result <- connectFullscreenAccount runtime
                rediscovered <- discoverLoginAccounts
                dashboardLoop (result <|> notice) rediscovered
            Just (LoginDashboardGateway, _) -> do
                result <- connectFullscreenGateway runtime
                rediscovered <- discoverLoginAccounts
                dashboardLoop (result <|> notice) rediscovered
            Just (LoginDashboardRefreshAll, _) -> do
                refreshed <-
                    withLoginProgress runtime "Refreshing provider usage…" $
                        mapConcurrently
                            (\account ->
                                if isGatewayLoginAccount account
                                    then pure account
                                    else refreshLoginAccountSafely account)
                            accounts
                dashboardLoop
                    (Just (refreshAllNotice refreshed))
                    refreshed
            Just (LoginDashboardOpen index, _) ->
                accountLoop notice index accounts

    accountLoop notice index accounts =
        case accountAt index accounts of
            Nothing -> dashboardLoop notice accounts
            Just account -> do
                let entries = loginAccountMenuEntries account
                choice <-
                    requestFullscreenChoiceWithBody
                        runtime
                        (accountMenuTitle account)
                        (loginAccountBody notice account)
                        0
                        (map snd entries)
                case choice >>= (`accountAt` entries) of
                    Nothing ->
                        dashboardLoop notice accounts
                    Just (LoginAccountRefresh, _) -> do
                        refreshed <-
                            withLoginProgress runtime "Refreshing account usage…" $
                                refreshLoginAccountSafely account
                        accountLoop
                            (Just (refreshOneNotice refreshed))
                            index
                            (replaceAt index refreshed accounts)
                    Just (LoginAccountToggle, _) -> do
                        result <- toggleLoginAccount account
                        rediscoverAndLoop (Just (noticeFromResult result))
                    Just (LoginAccountImport, _) -> do
                        result <- importLoginAccount account
                        rediscoverAndLoop (Just (noticeFromResult result))
                    Just (LoginAccountDisconnect, _) -> do
                        confirmed <- confirmFullscreenDisconnect runtime account
                        if not confirmed
                            then accountLoop notice index accounts
                            else do
                                result <- disconnectLoginAccount account
                                rediscoverAndLoop
                                    (Just (noticeFromResult result))
                    Just (LoginAccountBack, _) ->
                        dashboardLoop notice accounts
      where
        rediscoverAndLoop nextNotice = do
            rediscovered <- discoverLoginAccounts
            dashboardLoop nextNotice rediscovered

refreshLoginAccountSafely :: LoginAccount -> IO LoginAccount
refreshLoginAccountSafely account =
    tryAny (refreshLoginAccount account) >>= \case
        Left err ->
            pure account
                { loginUsage =
                    UsageUnavailable
                        ("usage check failed: " <> formatException err)
                }
        Right refreshed -> pure refreshed

refreshSelectedAccounts
    :: Chan (Int, LoginAccount)
    -> [Int]
    -> [LoginAccount]
    -> IO ()
refreshSelectedAccounts updates indices accounts =
    mapConcurrently_ refreshOne indices
  where
    refreshOne index = case accountAt index accounts of
        Nothing -> pure ()
        Just account -> do
            refreshed <- refreshLoginAccountSafely account
            writeChan updates (index, refreshed)

applyRefreshedAccount
    :: (Int, LoginAccount)
    -> LoginState
    -> LoginState
applyRefreshedAccount (index, refreshed) state =
    state { loginAccounts = replaceAt index refreshed state.loginAccounts }

replaceAt :: Int -> account -> [account] -> [account]
replaceAt index replacement accounts =
    case splitAt index accounts of
        (before, _ : after) -> before <> (replacement : after)
        _ -> accounts

toggleAt :: Bool -> Int -> [LoginAccount] -> IO ()
toggleAt color index accounts =
    case accountAt index accounts of
        Nothing -> pure ()
        Just account ->
            toggleLoginAccount account >>= printLoginResult color

deleteAt :: Bool -> Int -> [LoginAccount] -> IO ()
deleteAt color index accounts =
    case accountAt index accounts of
        Nothing -> pure ()
        Just account -> case account.loginManagedId of
            Nothing
                | isGatewayLoginAccount account ->
                    confirmDisconnect account
            Nothing ->
                disconnectLoginAccount account >>= printLoginResult color
            Just _ -> confirmDisconnect account
  where
    confirmDisconnect account =
        readApprovalLine
            ("Disconnect " <> account.loginLabel <> "? [y/N] ")
            >>= \case
                Just answer
                    | Text.toLower (Text.strip answer) `elem` ["y", "yes"] ->
                        disconnectLoginAccount account
                            >>= printLoginResult color
                _ -> pure ()

importAt :: Bool -> Int -> [LoginAccount] -> IO ()
importAt color index accounts =
    case accountAt index accounts of
        Nothing -> pure ()
        Just account ->
            importLoginAccount account >>= printLoginResult color

accountAt :: Int -> [account] -> Maybe account
accountAt index accounts =
    case drop index accounts of
        account : _ -> Just account
        [] -> Nothing
