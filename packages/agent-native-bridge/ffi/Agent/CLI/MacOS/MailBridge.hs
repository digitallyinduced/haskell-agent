{-# LANGUAGE ForeignFunctionInterface #-}

-- | Mail account administration callbacks exposed to native hosts.
module Agent.CLI.MacOS.MailBridge
    ( mailABISynchronousValidationSmoke
    ) where

import Agent.CLI.Mail.Imap (connectMailImapAccount)
import Agent.CLI.Mail.OAuth
    ( MailOAuthChallenge(..)
    , MailOAuthPoll(..)
    , cancelMailOAuth
    , pollMailOAuth
    , startMailOAuth
    )
import Agent.CLI.Mail.Store
    ( MailAccount(..)
    , MailAccountDiscovery(..)
    , MailImapSettings(..)
    , MailProvider(..)
    , MailTLSMode(..)
    , deleteMailAccount
    , discoverMailSettings
    , loadMailAccounts
    , mailAccountStateSlug
    , mailProviderSlug
    , mailTLSModeSlug
    , parseMailProvider
    , setMailAccountEnabled
    , validateMailImapSettings
    )
import Agent.CLI.MacOS.Marshalling (decodeUtf8Input, withText)
import Control.Concurrent (forkIO)
import Control.Exception.Safe (tryAny)
import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8, Word64)
import Foreign (FunPtr, Ptr, nullFunPtr, nullPtr)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CSize(..))

-- All returned strings are callback-scoped UTF-8. Credential material is
-- never returned through this ABI.
type MailAccountListCallback =
    Ptr () -> CInt
    -> CString -> CSize
    -> CString -> CSize
    -> CString -> CSize
    -> CString -> CSize
    -> CInt
    -> CString -> CSize
    -> CString -> CSize
    -> IO ()

type MailOAuthStartCallback =
    Ptr () -> CInt
    -> CString -> CSize
    -> CString -> CSize
    -> CInt
    -> CString -> CSize
    -> IO ()

type MailResultCallback =
    Ptr () -> CInt
    -> CString -> CSize
    -> CString -> CSize
    -> IO ()

type MailDiscoveryCallback =
    Ptr () -> CInt
    -> CString -> CSize
    -> CString -> CSize
    -> CInt
    -> CString -> CSize
    -> CString -> CSize
    -> CString -> CSize
    -> IO ()

foreign import ccall "dynamic"
    invokeMailAccountListCallback
        :: FunPtr MailAccountListCallback -> MailAccountListCallback

foreign import ccall "dynamic"
    invokeMailOAuthStartCallback
        :: FunPtr MailOAuthStartCallback -> MailOAuthStartCallback

foreign import ccall "dynamic"
    invokeMailResultCallback
        :: FunPtr MailResultCallback -> MailResultCallback

foreign import ccall "dynamic"
    invokeMailDiscoveryCallback
        :: FunPtr MailDiscoveryCallback -> MailDiscoveryCallback

foreign export ccall ha_mail_accounts_list
    :: FunPtr MailAccountListCallback -> Ptr () -> IO CInt

foreign export ccall ha_mail_oauth_start
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr MailOAuthStartCallback -> Ptr () -> IO CInt

foreign export ccall ha_mail_oauth_poll
    :: Ptr Word8 -> CSize -> FunPtr MailResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_mail_oauth_cancel
    :: Ptr Word8 -> CSize -> FunPtr MailResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_mail_discover
    :: Ptr Word8 -> CSize -> FunPtr MailDiscoveryCallback -> Ptr () -> IO CInt

foreign export ccall ha_mail_imap_connect
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize -> CInt -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr MailResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_mail_account_set_enabled
    :: Ptr Word8 -> CSize -> CInt
    -> FunPtr MailResultCallback -> Ptr () -> IO CInt

foreign export ccall ha_mail_account_delete
    :: Ptr Word8 -> CSize -> FunPtr MailResultCallback -> Ptr () -> IO CInt

ha_mail_accounts_list
    :: FunPtr MailAccountListCallback -> Ptr () -> IO CInt
ha_mail_accounts_list callback context
    | callback == nullFunPtr = pure (-1)
    | otherwise = do
        _ <- forkIO do
            tryAny loadMailAccounts >>= \case
                Left _ ->
                    invokeMailAccountListError callback context
                        mailOperationFailed
                Right (Left err) ->
                    invokeMailAccountListError callback context err
                Right (Right accounts) -> do
                    forM_ accounts \account ->
                        withMailAccountStrings account $
                            invokeMailAccountListCallback callback context 0
                    invokeMailAccountListCallback callback context 1
                        nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
                        0 nullPtr 0 nullPtr 0
        pure 0

ha_mail_oauth_start
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr MailOAuthStartCallback -> Ptr () -> IO CInt
ha_mail_oauth_start
    providerBytes (CSize providerLength)
    clientIdBytes (CSize clientIdLength)
    callback context
    | callback == nullFunPtr
        || invalidRequiredMailInputs
            [ (providerBytes, providerLength)
            , (clientIdBytes, clientIdLength)
            ] = pure (-1)
    | otherwise =
        decodeMailInputs
            [ (providerBytes, providerLength)
            , (clientIdBytes, clientIdLength)
            ] >>= \case
                Right [providerText, clientId]
                    | Just provider <- parseMailProvider providerText
                    , provider /= ImapProvider -> do
                        _ <- forkIO do
                            tryAny (startMailOAuth provider clientId) >>= \case
                                Left _ ->
                                    invokeMailOAuthStartError callback context
                                        mailOperationFailed
                                Right (Left err) ->
                                    invokeMailOAuthStartError callback context err
                                Right (Right challenge) ->
                                    withMailOAuthChallenge challenge $
                                        invokeMailOAuthStartCallback
                                            callback context 0
                        pure 0
                _ -> pure (-1)

ha_mail_oauth_poll
    :: Ptr Word8 -> CSize -> FunPtr MailResultCallback -> Ptr () -> IO CInt
ha_mail_oauth_poll
    flowIdBytes (CSize flowIdLength) callback context
    | callback == nullFunPtr
        || invalidRequiredMailInputs [(flowIdBytes, flowIdLength)] =
            pure (-1)
    | otherwise =
        decodeMailInputs [(flowIdBytes, flowIdLength)] >>= \case
            Right [flowId] -> do
                _ <- forkIO do
                    tryAny (pollMailOAuth flowId) >>= \case
                        Left _ ->
                            invokeMailResultError callback context
                                mailOperationFailed
                        Right (Left err) ->
                            invokeMailResultError callback context err
                        Right (Right MailOAuthPending) ->
                            invokeMailResultCallback callback context 1
                                nullPtr 0 nullPtr 0
                        Right (Right (MailOAuthConnected accountId)) ->
                            withText accountId \accountPtr accountLength ->
                                invokeMailResultCallback callback context 0
                                    accountPtr accountLength nullPtr 0
                        Right (Right (MailOAuthFailed err)) ->
                            invokeMailResultError callback context err
                        Right (Right MailOAuthCancelled) ->
                            invokeMailResultError callback context
                                "Mail authorization was cancelled."
                pure 0
            _ -> pure (-1)

ha_mail_oauth_cancel
    :: Ptr Word8 -> CSize -> FunPtr MailResultCallback -> Ptr () -> IO CInt
ha_mail_oauth_cancel
    flowIdBytes (CSize flowIdLength) callback context
    | callback == nullFunPtr
        || invalidRequiredMailInputs [(flowIdBytes, flowIdLength)] =
            pure (-1)
    | otherwise =
        decodeMailInputs [(flowIdBytes, flowIdLength)] >>= \case
            Right [flowId] -> do
                _ <- forkIO do
                    tryAny (cancelMailOAuth flowId) >>= \case
                        Left _ ->
                            invokeMailResultError callback context
                                mailOperationFailed
                        Right result ->
                            invokeMailUnitResult callback context result
                pure 0
            _ -> pure (-1)

ha_mail_discover
    :: Ptr Word8 -> CSize -> FunPtr MailDiscoveryCallback -> Ptr () -> IO CInt
ha_mail_discover emailBytes (CSize emailLength) callback context
    | callback == nullFunPtr
        || invalidRequiredMailInputs [(emailBytes, emailLength)] =
            pure (-1)
    | otherwise =
        decodeMailInputs [(emailBytes, emailLength)] >>= \case
            Right [email]
                | Right discovery <- discoverMailSettings email -> do
                    _ <- forkIO $
                        withMailDiscovery discovery $
                            invokeMailDiscoveryCallback callback context 0
                    pure 0
            _ -> pure (-1)

ha_mail_imap_connect
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize -> CInt -> Ptr Word8 -> CSize
    -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> FunPtr MailResultCallback -> Ptr () -> IO CInt
ha_mail_imap_connect
    emailBytes (CSize emailLength)
    labelBytes (CSize labelLength)
    hostBytes (CSize hostLength)
    port
    tlsBytes (CSize tlsLength)
    usernameBytes (CSize usernameLength)
    passwordBytes (CSize passwordLength)
    callback context
    | callback == nullFunPtr
        || invalidRequiredMailInputs
            [ (emailBytes, emailLength)
            , (hostBytes, hostLength)
            , (tlsBytes, tlsLength)
            , (usernameBytes, usernameLength)
            , (passwordBytes, passwordLength)
            ]
        || invalidOptionalMailInputs [(labelBytes, labelLength)]
        || port < 1 || port > 65535 =
            pure (-1)
    | otherwise =
        decodeMailInputs
            [ (emailBytes, emailLength)
            , (labelBytes, labelLength)
            , (hostBytes, hostLength)
            , (tlsBytes, tlsLength)
            , (usernameBytes, usernameLength)
            , (passwordBytes, passwordLength)
            ] >>= \case
                Right [email, label, host, tlsText, username, password]
                    | Just tlsMode <- parseMailTLSMode tlsText
                    , let settings = MailImapSettings
                            { mailImapHost = host
                            , mailImapPort = fromIntegral port
                            , mailImapTLSMode = tlsMode
                            , mailImapUsername = username
                            }
                    , Right () <- validateMailImapSettings settings -> do
                        _ <- forkIO do
                            tryAny
                                (connectMailImapAccount
                                    email label settings password)
                                >>= \case
                                    Left _ ->
                                        invokeMailResultError callback context
                                            mailOperationFailed
                                    Right (Left err) ->
                                        invokeMailResultError callback context err
                                    Right (Right accountId) ->
                                        withText accountId
                                            \accountPtr accountLength ->
                                                invokeMailResultCallback
                                                    callback context 0
                                                    accountPtr accountLength
                                                    nullPtr 0
                        pure 0
                _ -> pure (-1)

ha_mail_account_set_enabled
    :: Ptr Word8 -> CSize -> CInt
    -> FunPtr MailResultCallback -> Ptr () -> IO CInt
ha_mail_account_set_enabled
    accountIdBytes (CSize accountIdLength) enabled callback context
    | callback == nullFunPtr
        || invalidRequiredMailInputs [(accountIdBytes, accountIdLength)]
        || enabled < 0 || enabled > 1 =
            pure (-1)
    | otherwise =
        decodeMailInputs [(accountIdBytes, accountIdLength)] >>= \case
            Right [accountId] -> do
                _ <- forkIO do
                    tryAny
                        (setMailAccountEnabled accountId (enabled == 1))
                        >>= \case
                            Left _ ->
                                invokeMailResultError callback context
                                    mailOperationFailed
                            Right result ->
                                invokeMailUnitResult callback context result
                pure 0
            _ -> pure (-1)

ha_mail_account_delete
    :: Ptr Word8 -> CSize -> FunPtr MailResultCallback -> Ptr () -> IO CInt
ha_mail_account_delete
    accountIdBytes (CSize accountIdLength) callback context
    | callback == nullFunPtr
        || invalidRequiredMailInputs [(accountIdBytes, accountIdLength)] =
            pure (-1)
    | otherwise =
        decodeMailInputs [(accountIdBytes, accountIdLength)] >>= \case
            Right [accountId] -> do
                _ <- forkIO do
                    tryAny (deleteMailAccount accountId) >>= \case
                        Left _ ->
                            invokeMailResultError callback context
                                mailOperationFailed
                        Right result ->
                            invokeMailUnitResult callback context result
                pure 0
            _ -> pure (-1)

-- | Purely synchronous ABI smoke used by the FFI test suite. Null callbacks
-- must reject without launching a worker or touching account storage.
mailABISynchronousValidationSmoke :: IO Bool
mailABISynchronousValidationSmoke = do
    statuses <- sequence
        [ ha_mail_accounts_list nullFunPtr nullPtr
        , ha_mail_oauth_start
            nullPtr 0 nullPtr 0 nullFunPtr nullPtr
        , ha_mail_oauth_poll nullPtr 0 nullFunPtr nullPtr
        , ha_mail_oauth_cancel nullPtr 0 nullFunPtr nullPtr
        , ha_mail_discover nullPtr 0 nullFunPtr nullPtr
        , ha_mail_imap_connect
            nullPtr 0 nullPtr 0 nullPtr 0 0 nullPtr 0
            nullPtr 0 nullPtr 0 nullFunPtr nullPtr
        , ha_mail_account_set_enabled
            nullPtr 0 0 nullFunPtr nullPtr
        , ha_mail_account_delete nullPtr 0 nullFunPtr nullPtr
        ]
    pure (all (== -1) statuses)

withMailAccountStrings
    :: MailAccount
    -> (CString -> CSize -> CString -> CSize
        -> CString -> CSize -> CString -> CSize
        -> CInt -> CString -> CSize -> CString -> CSize -> IO value)
    -> IO value
withMailAccountStrings account action =
    withText account.mailAccountId \accountId accountIdLength ->
    withText (mailProviderSlug account.mailAccountProvider)
        \provider providerLength ->
    withText account.mailAccountEmail \email emailLength ->
    withText account.mailAccountLabel \label labelLength ->
    withText (mailAccountStateSlug account.mailAccountState)
        \state stateLength ->
            action
                accountId accountIdLength
                provider providerLength
                email emailLength
                label labelLength
                (if account.mailAccountEnabled then 1 else 0)
                state stateLength
                nullPtr 0

invokeMailAccountListError
    :: FunPtr MailAccountListCallback -> Ptr () -> Text -> IO ()
invokeMailAccountListError callback context err =
    withText err \errorPtr errorLength ->
        invokeMailAccountListCallback callback context (-1)
            nullPtr 0 nullPtr 0 nullPtr 0 nullPtr 0
            0 nullPtr 0 errorPtr errorLength

withMailOAuthChallenge
    :: MailOAuthChallenge
    -> (CString -> CSize -> CString -> CSize
        -> CInt -> CString -> CSize -> IO value)
    -> IO value
withMailOAuthChallenge challenge action =
    withText challenge.mailOAuthAuthorizationUrl
        \authorizationUrl authorizationUrlLength ->
    withText challenge.mailOAuthFlowId \flowId flowIdLength ->
        action
            authorizationUrl authorizationUrlLength
            flowId flowIdLength
            (fromIntegral challenge.mailOAuthExpiresInSeconds)
            nullPtr 0

invokeMailOAuthStartError
    :: FunPtr MailOAuthStartCallback -> Ptr () -> Text -> IO ()
invokeMailOAuthStartError callback context err =
    withText err \errorPtr errorLength ->
        invokeMailOAuthStartCallback callback context (-1)
            nullPtr 0 nullPtr 0 0 errorPtr errorLength

invokeMailResultError
    :: FunPtr MailResultCallback -> Ptr () -> Text -> IO ()
invokeMailResultError callback context err =
    withText err \errorPtr errorLength ->
        invokeMailResultCallback callback context (-1)
            nullPtr 0 errorPtr errorLength

invokeMailUnitResult
    :: FunPtr MailResultCallback
    -> Ptr ()
    -> Either Text ()
    -> IO ()
invokeMailUnitResult callback context = \case
    Left err -> invokeMailResultError callback context err
    Right () ->
        invokeMailResultCallback callback context 0
            nullPtr 0 nullPtr 0

withMailDiscovery
    :: MailAccountDiscovery
    -> (CString -> CSize -> CString -> CSize
        -> CInt -> CString -> CSize -> CString -> CSize
        -> CString -> CSize -> IO value)
    -> IO value
withMailDiscovery discovery action =
    case discovery of
        MailOAuthDiscovery provider ->
            withText (mailProviderSlug provider) \providerPtr providerLength ->
                action
                    providerPtr providerLength
                    nullPtr 0
                    0
                    nullPtr 0
                    nullPtr 0
                    nullPtr 0
        MailImapDiscovery settings ->
            withText "imap" \providerPtr providerLength ->
            withText settings.mailImapHost \host hostLength ->
            withText (mailTLSModeSlug settings.mailImapTLSMode)
                \tlsMode tlsModeLength ->
            withText settings.mailImapUsername \username usernameLength ->
                action
                    providerPtr providerLength
                    host hostLength
                    (fromIntegral settings.mailImapPort)
                    tlsMode tlsModeLength
                    username usernameLength
                    nullPtr 0

parseMailTLSMode :: Text -> Maybe MailTLSMode
parseMailTLSMode value =
    case Text.toCaseFold (Text.strip value) of
        "tls" -> Just MailImplicitTLS
        "starttls" -> Just MailStartTLS
        _ -> Nothing

decodeMailInputs
    :: [(Ptr Word8, Word64)]
    -> IO (Either () [Text])
decodeMailInputs = fmap sequence . traverse decodeOne
  where
    decodeOne (pointer, length)
        | pointer == nullPtr && length == 0 = pure (Right "")
        | otherwise = decodeUtf8Input pointer length

invalidRequiredMailInputs :: [(Ptr Word8, Word64)] -> Bool
invalidRequiredMailInputs =
    any \(pointer, length) ->
        pointer == nullPtr
            || length == 0
            || length > maximumMailABIInputBytes

invalidOptionalMailInputs :: [(Ptr Word8, Word64)] -> Bool
invalidOptionalMailInputs =
    any \(pointer, length) ->
        (pointer == nullPtr && length > 0)
            || length > maximumMailABIInputBytes

maximumMailABIInputBytes :: Word64
maximumMailABIInputBytes = 64 * 1024

mailOperationFailed :: Text
mailOperationFailed = "The local mail account operation failed."
