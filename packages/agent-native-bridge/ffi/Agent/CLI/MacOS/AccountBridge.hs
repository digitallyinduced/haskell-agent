{-# LANGUAGE ForeignFunctionInterface #-}

-- | Account-list and connection callbacks exposed to native hosts.
module Agent.CLI.MacOS.AccountBridge () where

import Agent.CLI.CredentialStore (deleteManagedCredential, setManagedCredentialEnabled)
import Agent.CLI.Login
    ( AccountBilling(..)
    , AccountUsage(..)
    , LoginAccount(..)
    , UsageState(..)
    , UsageWindow(..)
    , discoverLoginAccounts
    , loginAccountSelectionId
    , refreshLoginAccount
    )
import Agent.CLI.MacOS.AccountConnection
import Agent.CLI.MacOS.Marshalling
    ( anyNonEmptyNull, decodeInput, nonEmptyText, withText, withOptionalText )
import Agent.Provider (providerSlug)
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception.Safe (SomeException, tryAny)
import Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word8)
import Foreign (FunPtr, Ptr, nullFunPtr, nullPtr)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CLLong(..), CSize(..))

type AccountListCallback =
    Ptr () -> CInt -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> CString -> CSize -> CString -> CSize
    -> CInt -> CInt -> CString -> CSize -> IO ()

type AccountUsageWindowCallback =
    Ptr () -> CString -> CSize -> CString -> CSize
    -> CInt -> CLLong -> CLLong -> IO ()

type AccountResultCallback =
    Ptr () -> CInt -> CString -> CSize -> CString -> CSize -> IO ()

type AccountOAuthStartCallback =
    Ptr () -> CInt -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> CString -> CSize -> CInt -> CInt
    -> CString -> CSize -> IO ()

foreign import ccall "dynamic"
    invokeAccountListCallback
        :: FunPtr AccountListCallback -> AccountListCallback

foreign import ccall "dynamic"
    invokeAccountUsageWindowCallback
        :: FunPtr AccountUsageWindowCallback -> AccountUsageWindowCallback

foreign import ccall "dynamic"
    invokeAccountResultCallback
        :: FunPtr AccountResultCallback -> AccountResultCallback

foreign import ccall "dynamic"
    invokeAccountOAuthStartCallback
        :: FunPtr AccountOAuthStartCallback -> AccountOAuthStartCallback

foreign export ccall ha_accounts_list
    :: FunPtr AccountListCallback -> FunPtr AccountUsageWindowCallback
    -> Ptr () -> IO CInt

foreign export ccall ha_account_oauth_start
    :: Ptr Word8 -> CSize -> FunPtr AccountOAuthStartCallback -> Ptr () -> IO CInt

foreign export ccall ha_account_oauth_poll
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> CInt -> CInt
    -> FunPtr AccountResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_account_api_key_connect
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr AccountResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_account_set_enabled
    :: Ptr Word8 -> CSize -> CInt
    -> FunPtr AccountResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_account_delete
    :: Ptr Word8 -> CSize -> FunPtr AccountResultCallback -> Ptr () -> IO CInt

ha_accounts_list
    :: FunPtr AccountListCallback -> FunPtr AccountUsageWindowCallback
    -> Ptr () -> IO CInt
ha_accounts_list callback usageCallback context
    | callback == nullFunPtr || usageCallback == nullFunPtr = pure 1
    | otherwise = do
        _ <- forkIO do
            tryAny
                (discoverLoginAccounts >>= mapConcurrently refreshLoginAccount)
                >>= \case
                Left exception ->
                    withText (Text.pack (show exception)) $ \errorPtr errorLength ->
                        invokeAccountListCallback callback context (-1)
                            nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
                            nullPtr 0 nullPtr 0 nullPtr 0
                            0 0 errorPtr errorLength
                Right accounts -> do
                    forM_ accounts \account -> do
                        withAccountStrings account $
                            invokeAccountListCallback callback context 0
                        invokeAccountUsageWindows usageCallback context account
                    invokeAccountListCallback callback context 1
                        nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
                        nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
                        0 0 nullPtr 0
        pure 0

ha_account_oauth_start
    :: Ptr Word8 -> CSize -> FunPtr AccountOAuthStartCallback -> Ptr () -> IO CInt
ha_account_oauth_start providerBytes (CSize providerLength) callback context
    | callback == nullFunPtr = pure 1
    | providerBytes == nullPtr && providerLength > 0 = pure 2
    | otherwise = do
        provider <- decodeInput providerBytes providerLength
        _ <- forkIO do
            tryAny (startAccountOAuth AccountProviderRequest
                { accountProvider = provider }) >>= \case
                Left exception -> withText (Text.pack (show exception)) $ \errorPtr errorLength ->
                    invokeAccountOAuthStartCallback callback context
                        (-1) nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 0 0
                        errorPtr errorLength
                Right (Left err) -> withText err $ \errorPtr errorLength ->
                    invokeAccountOAuthStartCallback callback context
                        (-1) nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0 0 0
                        errorPtr errorLength
                Right (Right value) -> case parseChallenge value of
                    Nothing -> withText "invalid OAuth challenge"
                        (\errorPtr errorLength ->
                            invokeAccountOAuthStartCallback callback context
                                (-1) nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
                                0 0 errorPtr errorLength)
                    Just challenge ->
                        withChallengeStrings challenge
                            (invokeAccountOAuthStartCallback callback context 0)
        pure 0

ha_account_oauth_poll
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> CInt -> CInt
    -> FunPtr AccountResultCallback -> Ptr () -> IO CInt
ha_account_oauth_poll providerBytes (CSize providerLength) urlBytes (CSize urlLength)
    userBytes (CSize userLength) authIdBytes (CSize authIdLength)
    deviceBytes (CSize deviceLength) pollInterval expires callback context
    | callback == nullFunPtr = pure 1
    | anyNonEmptyNull
        [ (providerBytes, providerLength)
        , (urlBytes, urlLength)
        , (userBytes, userLength)
        , (authIdBytes, authIdLength)
        , (deviceBytes, deviceLength)
        ] = pure 2
    | otherwise = do
        provider <- decodeInput providerBytes providerLength
        url <- decodeInput urlBytes urlLength
        user <- decodeInput userBytes userLength
        authId <- decodeInput authIdBytes authIdLength
        device <- decodeInput deviceBytes deviceLength
        _ <- forkIO do
            tryAny (pollAccountOAuth AccountOAuthPollRequest
                { oauthPollProvider = provider
                , oauthPollVerificationUrl = nonEmptyText url
                , oauthPollUserCode = nonEmptyText user
                , oauthPollDeviceAuthId = nonEmptyText authId
                , oauthPollDeviceCode = nonEmptyText device
                , oauthPollIntervalSeconds = Just (fromIntegral pollInterval)
                , oauthPollExpiresInSeconds = Just (fromIntegral expires)
                }) >>= \case
                    Left exception -> invokeExceptionResult callback context exception
                    Right result -> invokeResult callback context result
        pure 0

ha_account_api_key_connect
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr AccountResultCallback -> Ptr () -> IO CInt
ha_account_api_key_connect providerBytes (CSize providerLength) keyBytes (CSize keyLength) callback context
    | callback == nullFunPtr = pure 1
    | anyNonEmptyNull
        [ (providerBytes, providerLength), (keyBytes, keyLength) ] = pure 2
    | otherwise = do
        provider <- decodeInput providerBytes providerLength
        key <- decodeInput keyBytes keyLength
        _ <- forkIO do
            tryAny (connectAccountAPIKey AccountAPIKeyRequest
                { accountAPIKeyProvider = provider, accountAPIKey = key }
                ) >>= \case
                    Left exception -> invokeExceptionResult callback context exception
                    Right result -> invokeResult callback context result
        pure 0

ha_account_set_enabled
    :: Ptr Word8 -> CSize -> CInt -> FunPtr AccountResultCallback -> Ptr () -> IO CInt
ha_account_set_enabled idBytes (CSize idLength) enabled callback context
    | callback == nullFunPtr = pure 1
    | anyNonEmptyNull [(idBytes, idLength)] = pure 2
    | otherwise = do
        managedId <- decodeInput idBytes idLength
        _ <- forkIO do
            tryAny (setManagedCredentialEnabled managedId (enabled /= 0))
                >>= \case
                    Left exception -> invokeExceptionResult callback context exception
                    Right result -> invokeStoreResult callback context result
        pure 0

ha_account_delete
    :: Ptr Word8 -> CSize -> FunPtr AccountResultCallback -> Ptr () -> IO CInt
ha_account_delete idBytes (CSize idLength) callback context
    | callback == nullFunPtr = pure 1
    | anyNonEmptyNull [(idBytes, idLength)] = pure 2
    | otherwise = do
        managedId <- decodeInput idBytes idLength
        _ <- forkIO do
            tryAny (deleteManagedCredential managedId) >>= \case
                Left exception -> invokeExceptionResult callback context exception
                Right result -> invokeStoreResult callback context result
        pure 0

withAccountStrings :: LoginAccount -> (CString -> CSize -> CString -> CSize
    -> CString -> CSize -> CString -> CSize -> CString -> CSize
    -> CString -> CSize -> CString -> CSize -> CString -> CSize
    -> CInt -> CInt -> CString -> CSize -> IO a) -> IO a
withAccountStrings account action =
    withText (providerSlug account.loginProvider) $ \provider providerLength ->
    withText (billingText account.loginBilling) $ \billing billingLength ->
    withText (loginAccountSelectionId account) $ \selection selectionLength ->
    withText account.loginAccountId $ \accountId accountIdLength ->
    withText account.loginLabel $ \label labelLength ->
    withText account.loginSource $ \source sourceLength ->
        withText
            (accountUsageSummary account)
            $ \detail detailLength ->
        withOptionalText account.loginManagedId $ \managedId managedLength ->
        action provider providerLength billing billingLength
            selection selectionLength accountId accountIdLength label labelLength
            detail detailLength managedId managedLength source sourceLength
            (if account.loginEnabled then 1 else 0)
            (if account.loginManagedId == Nothing then 0 else 1)
            nullPtr 0

billingText :: AccountBilling -> Text
billingText = \case
    SubscriptionBilling _ -> "subscription"
    ApiCreditsBilling -> "api"

accountUsageSummary :: LoginAccount -> Text
accountUsageSummary account =
    Text.intercalate " · " $
        billing <> case account.loginUsage of
            UsageNotChecked -> []
            UsageUnavailable _ -> ["usage unavailable"]
            UsageAvailable usage ->
                maybeToList usage.usagePlan
                    <> maybeToList
                        (("credits " <>) <$> usage.creditsRemaining)
                    <> maybeToList
                        (("used " <>) <$> usage.creditsUsed)
  where
    billing = case account.loginBilling of
        ApiCreditsBilling -> ["API credits"]
        SubscriptionBilling plan ->
            maybe ["subscription"] (\value -> ["subscription", value]) plan
    maybeToList = maybe [] pure

invokeAccountUsageWindows
    :: FunPtr AccountUsageWindowCallback -> Ptr () -> LoginAccount -> IO ()
invokeAccountUsageWindows callback context account =
    case account.loginUsage of
        UsageAvailable usage ->
            forM_ usage.usageWindows \window ->
                withText (loginAccountSelectionId account) $ \selection selectionLength ->
                withText window.windowName $ \name nameLength ->
                    invokeAccountUsageWindowCallback callback context
                        selection selectionLength name nameLength
                        (fromIntegral window.usedPercent)
                        (fromIntegral window.windowSeconds)
                        (round (utcTimeToPOSIXSeconds window.resetsAt))
        _ -> pure ()

parseChallenge :: Aeson.Value
    -> Maybe (Text, Text, Maybe Text, Maybe Text, Int, Int)
parseChallenge = Aeson.parseMaybe $ Aeson.withObject "challenge" \object ->
    (,,,,,)
        <$> object Aeson..: "verificationUrl"
        <*> object Aeson..: "userCode"
        <*> object Aeson..:? "deviceAuthId"
        <*> object Aeson..:? "deviceCode"
        <*> (object Aeson..:? "pollIntervalSeconds" Aeson..!= 5)
        <*> (object Aeson..:? "expiresInSeconds" Aeson..!= 0)

withChallengeStrings
    :: (Text, Text, Maybe Text, Maybe Text, Int, Int)
    -> (CString -> CSize -> CString -> CSize -> CString -> CSize -> CString -> CSize
        -> CInt -> CInt -> CString -> CSize -> IO a)
    -> IO a
withChallengeStrings (url, user, authId, device, interval, expires) action =
    withText url $ \urlPtr urlLength ->
    withText user $ \userPtr userLength ->
    withOptionalText authId $ \authPtr authLength ->
    withOptionalText device $ \devicePtr deviceLength ->
        action urlPtr urlLength userPtr userLength authPtr authLength
            devicePtr deviceLength (fromIntegral interval) (fromIntegral expires)
            nullPtr 0

invokeResult :: FunPtr AccountResultCallback -> Ptr ()
    -> Either Text Aeson.Value -> IO ()
invokeResult callback context result = case result of
    Left errorText -> withText errorText $ \errorPtr errorLength ->
        invokeAccountResultCallback callback context (-1)
            nullPtr 0 errorPtr errorLength
    Right value ->
        let status = Aeson.parseMaybe (Aeson.withObject "result"
                (\object -> object Aeson..: "status")) value
            accountId = Aeson.parseMaybe (Aeson.withObject "result"
                (\object -> object Aeson..:? "accountID")) value >>= id
        in case status of
            Just ("pending" :: Text) ->
                invokeAccountResultCallback callback context 1 nullPtr 0 nullPtr 0
            _ -> withOptionalText accountId $ \idPtr idLength ->
                invokeAccountResultCallback callback context 0 idPtr idLength nullPtr 0

invokeStoreResult :: FunPtr AccountResultCallback -> Ptr ()
    -> Either Text () -> IO ()
invokeStoreResult callback context result = case result of
    Left errorText -> withText errorText $ \errorPtr errorLength ->
        invokeAccountResultCallback callback context (-1)
            nullPtr 0 errorPtr errorLength
    Right () -> invokeAccountResultCallback callback context 0 nullPtr 0 nullPtr 0

invokeExceptionResult :: FunPtr AccountResultCallback -> Ptr ()
    -> SomeException -> IO ()
invokeExceptionResult callback context exception =
    withText (Text.pack (show exception)) $ \errorPtr errorLength ->
        invokeAccountResultCallback callback context (-1)
            nullPtr 0 errorPtr errorLength
