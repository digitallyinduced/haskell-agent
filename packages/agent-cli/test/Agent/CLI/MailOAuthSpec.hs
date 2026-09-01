module Agent.CLI.MailOAuthSpec (spec) where

import Agent.CLI.Mail.OAuth
import Agent.CLI.Mail.Store (MailProvider(GmailProvider))
import Control.Concurrent (threadDelay)
import Control.Exception (bracket, finally)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Network.HTTP.Types.URI (urlDecode)
import Network.Socket
    ( AddrInfo(..)
    , Socket
    , SocketType(Stream)
    , close
    , connect
    , defaultHints
    , defaultProtocol
    , getAddrInfo
    , socket
    )
import qualified Network.Socket.ByteString as Socket
import Network.URI (URIAuth(..), parseURI, uriAuthority)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "mail OAuth loopback callback" do
    it "ignores a preconnection and accepts a fragmented matching callback" do
        started <- startMailOAuth
            GmailProvider
            "test-client.apps.googleusercontent.com"
        challenge <- case started of
            Left err -> expectationFailure (Text.unpack err) >> fail "OAuth did not start"
            Right value -> pure value
        let cleanup = do
                _ <- cancelMailOAuth challenge.mailOAuthFlowId
                pure ()
        (do
            state <- requireQueryValue
                "state"
                challenge.mailOAuthAuthorizationUrl
            redirectUri <- requireQueryValue
                "redirect_uri"
                challenge.mailOAuthAuthorizationUrl
            port <- requireLoopbackPort redirectUri

            withLoopbackClient port \client -> do
                Socket.sendAll client
                    "GET /favicon.ico HTTP/1.1\r\nHost: localhost\r\n\r\n"
                response <- timeout 1_000_000 (Socket.recv client 4096)
                response `shouldSatisfy` maybe False
                    ("HTTP/1.1 200 OK" `BS.isPrefixOf`)

            withLoopbackClient port \client -> do
                Socket.sendAll client
                    ("GET /mail/callback?state="
                        <> TextEncoding.encodeUtf8 state)
                threadDelay 20_000
                Socket.sendAll client
                    "&error=access_denied HTTP/1.1\r\nHost: localhost\r\n\r\n"
                response <- timeout 1_000_000 (Socket.recv client 4096)
                response `shouldSatisfy` maybe False
                    ("HTTP/1.1 200 OK" `BS.isPrefixOf`)

            result <- waitForOAuthResult 100 challenge.mailOAuthFlowId
            result `shouldSatisfy` \case
                Right (MailOAuthFailed message) ->
                    "access_denied" `Text.isInfixOf` message
                _ -> False
         ) `finally` cleanup

waitForOAuthResult :: Int -> Text -> IO (Either Text MailOAuthPoll)
waitForOAuthResult remaining flowId
    | remaining <= 0 = pure (Left "OAuth callback test timed out")
    | otherwise =
        pollMailOAuth flowId >>= \case
            Right MailOAuthPending -> do
                threadDelay 10_000
                waitForOAuthResult (remaining - 1) flowId
            result -> pure result

withLoopbackClient :: Int -> (Socket -> IO value) -> IO value
withLoopbackClient port action = do
    addresses <- getAddrInfo
        (Just defaultHints { addrSocketType = Stream })
        (Just "127.0.0.1")
        (Just (show port))
    case addresses of
        [] -> fail "No IPv4 loopback address was available"
        address : _ ->
            bracket
                (socket (addrFamily address) Stream defaultProtocol)
                close
                (\client -> connect client (addrAddress address) >> action client)

requireQueryValue :: Text -> Text -> IO Text
requireQueryValue key url =
    case lookup key (queryParameters url) of
        Just value -> pure value
        Nothing -> expectationFailure
            ("OAuth URL omitted " <> Text.unpack key) >> fail "invalid OAuth URL"

queryParameters :: Text -> [(Text, Text)]
queryParameters url =
    [ (decode key, decode (Text.drop 1 rawValue))
    | parameter <- Text.splitOn "&" . Text.drop 1 . snd $
        Text.breakOn "?" url
    , let (key, rawValue) = Text.breakOn "=" parameter
    , not (Text.null rawValue)
    ]
  where
    decode =
        TextEncoding.decodeUtf8
            . urlDecode True
            . TextEncoding.encodeUtf8

requireLoopbackPort :: Text -> IO Int
requireLoopbackPort redirectUri =
    case parseURI (Text.unpack redirectUri)
        >>= uriAuthority
        >>= readPort . uriPort of
            Nothing ->
                expectationFailure "OAuth redirect URI had no loopback port"
                    >> fail "invalid OAuth redirect URI"
            Just port -> pure port
  where
    readPort (':' : digits) =
        case reads digits of
            [(port, "")] | port >= 1 && port <= 65535 -> Just port
            _ -> Nothing
    readPort _ = Nothing
