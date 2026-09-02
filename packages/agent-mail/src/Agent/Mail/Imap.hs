-- | Secure custom-IMAP account connection and validation.
--
-- The connector never permits plaintext authentication: implicit TLS is
-- established before reading the greeting, while STARTTLS is negotiated and
-- verified before LOGIN is sent.  Passwords are accepted as values only and
-- are never included in an error or returned to native callers.
module Agent.Mail.Imap
    ( verifyMailImapCredentials
    , withMailImapConnection
    ) where

import Agent.Mail.Types
    ( MailImapSettings(..)
    , MailTLSMode(..)
    , validateMailImapSettings
    )
import Control.Exception.Safe (SomeException, bracket, finally, tryAny)
import Control.Monad (unless, void)
import Data.Char (isControl)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError
import qualified Network.Connection as Connection
import Network.TLS (defaultSupported)
import System.Timeout (timeout)

verifyMailImapCredentials
    :: MailImapSettings
    -> Text
    -> IO (Either Text ())
verifyMailImapCredentials settings password =
    fmap (fmap (const ())) $
        withMailImapConnection settings password \connection -> do
            -- Probe only INBOX so an account with thousands of folders cannot
            -- exhaust the bounded response reader during setup.
            sendLine connection "a003 LIST \"\" \"INBOX\""
            consumeUntilTagged connection "a003"

-- | Open an authenticated, TLS-protected IMAP connection for one bounded
-- operation.  The caller never receives a plaintext socket and cannot opt out
-- of certificate validation or the connection timeout.
withMailImapConnection
    :: MailImapSettings
    -> Text
    -> (Connection.Connection -> IO value)
    -> IO (Either Text value)
withMailImapConnection settings password action =
    case (validateMailImapSettings settings, validatePassword password) of
        (Left err, _) -> pure (Left err)
        (_, Left err) -> pure (Left err)
        (Right (), Right ()) -> do
            outcome <- timeout imapConnectTimeoutMicros $
                tryAny (withConnection settings \context connection ->
                    (do
                        authenticate context settings password connection
                        action connection
                    ) `finally` logoutQuietly connection)
            pure case outcome of
                Nothing -> Left "The IMAP server did not respond in time."
                Just (Left exception) -> Left (sanitizeImapException exception)
                Just (Right value) -> Right value

withConnection
    :: MailImapSettings
    -> (Connection.ConnectionContext -> Connection.Connection -> IO value)
    -> IO value
withConnection settings action =
    bracket acquire (Connection.connectionClose . snd) \(context, connection) ->
        action context connection
  where
    acquire = do
        context <- Connection.initConnectionContext
        connection <- Connection.connectTo context Connection.ConnectionParams
            { Connection.connectionHostname = Text.unpack settings.mailImapHost
            , Connection.connectionPort = fromIntegral settings.mailImapPort
            , Connection.connectionUseSecure =
                case settings.mailImapTLSMode of
                    MailImplicitTLS -> Just tlsSettings
                    MailStartTLS -> Nothing
            , Connection.connectionUseSocks = Nothing
            }
        pure (context, connection)

    tlsSettings = Connection.TLSSettingsSimple
        { Connection.settingDisableCertificateValidation = False
        , Connection.settingDisableSession = False
        , Connection.settingUseServerName = True
        , Connection.settingClientSupported = defaultSupported
        }

authenticate
    :: Connection.ConnectionContext
    -> MailImapSettings
    -> Text
    -> Connection.Connection
    -> IO ()
authenticate context settings password connection = do
    expectUntaggedOk =<< readLine connection
    case settings.mailImapTLSMode of
        MailImplicitTLS -> pure ()
        MailStartTLS -> do
            sendLine connection "a001 STARTTLS"
            consumeUntilTagged connection "a001"
            Connection.connectionSetSecure context connection tlsSettings
    sendLine connection $
        "a002 LOGIN "
            <> quoteImap settings.mailImapUsername
            <> " "
            <> quoteImap password
    consumeUntilTagged connection "a002"
  where
    tlsSettings = Connection.TLSSettingsSimple
        { Connection.settingDisableCertificateValidation = False
        , Connection.settingDisableSession = False
        , Connection.settingUseServerName = True
        , Connection.settingClientSupported = defaultSupported
        }

readLine :: Connection.Connection -> IO Text
readLine =
    fmap (TextEncoding.decodeUtf8With TextEncodingError.lenientDecode)
        . Connection.connectionGetLine maximumImapLineBytes

sendLine :: Connection.Connection -> Text -> IO ()
sendLine connection line =
    Connection.connectionPut connection
        (TextEncoding.encodeUtf8 line <> "\r\n")

consumeUntilTagged :: Connection.Connection -> Text -> IO ()
consumeUntilTagged connection tag = go maximumProbeLines
  where
    go remaining
        | remaining <= 0 =
            fail "IMAP response exceeded the safe validation limit."
        | otherwise = do
            line <- readLine connection
            if (tag <> " ") `Text.isPrefixOf` Text.toCaseFold line
                then expectTaggedOk tag line
                else go (remaining - 1)

logoutQuietly :: Connection.Connection -> IO ()
logoutQuietly connection =
    void (tryAny (sendLine connection "a999 LOGOUT") :: IO (Either SomeException ()))

expectUntaggedOk :: Text -> IO ()
expectUntaggedOk line =
    unless ("* ok" `Text.isPrefixOf` Text.toCaseFold line) $
        fail "IMAP server did not return a valid greeting."

expectTaggedOk :: Text -> Text -> IO ()
expectTaggedOk tag line =
    unless ((Text.toCaseFold tag <> " ok") `Text.isPrefixOf`
            Text.toCaseFold line) $
        fail "IMAP authentication or mailbox validation failed."

quoteImap :: Text -> Text
quoteImap value =
    "\"" <> Text.concatMap escape value <> "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape character = Text.singleton character

validatePassword :: Text -> Either Text ()
validatePassword password
    | Text.null password = Left "An IMAP password is required."
    | Text.length password > maximumCredentialChars =
        Left "The IMAP password is too long."
    | Text.any isControl password =
        Left "The IMAP password contains unsupported control characters."
    | otherwise = Right ()

sanitizeImapException :: SomeException -> Text
sanitizeImapException exception
    | "certificate" `Text.isInfixOf` folded =
        "The IMAP server TLS certificate could not be verified."
    | "authentication" `Text.isInfixOf` folded
        || "login" `Text.isInfixOf` folded =
        "IMAP authentication failed. Check the username and app password."
    | otherwise =
        "Could not securely validate the IMAP account."
  where
    folded = Text.toCaseFold (Text.pack (show exception))

maximumCredentialChars, maximumImapLineBytes, maximumProbeLines :: Int
maximumCredentialChars = 4096
maximumImapLineBytes = 64 * 1024
maximumProbeLines = 500

imapConnectTimeoutMicros :: Int
imapConnectTimeoutMicros = 20 * 1000 * 1000
