{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Agent.CLI.McpOAuth
    ( loginMcp
    , logoutMcp
    ) where

import Agent.CLI.McpOAuthStore (mcpOAuthStorePath, saveMcpOAuth)
import qualified Agent.MCP.OAuth as OAuth
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.URI (urlEncode)
import Network.Socket
    ( AddrInfo(..), Socket, SockAddr(..), SocketType(Stream), accept, bind, close
    , defaultHints, defaultProtocol, getAddrInfo, getSocketName, listen
    , setSocketOption, socket, SocketOption(ReuseAddr)
    )
import qualified Network.Socket.ByteString as Socket
import qualified System.Directory.OsPath as Dir
import qualified System.Entropy
import System.Exit (ExitCode(..))
import System.Process (rawSystem)
import System.Timeout (timeout)
import Control.Exception.Safe (bracket, tryAny)
import Control.Monad (when)

loginMcp :: Text -> IO ()
loginMcp serverUrl = do
    manager <- newTlsManager
    resource <- OAuth.discoverProtectedResource manager serverUrl >>= either failText pure
    authIssuer <- case resource.authorizationServers of
        issuer : _ -> pure issuer
        [] -> failText "MCP OAuth metadata did not advertise an authorization server"
    metadata <- OAuth.discoverAuthorizationServer manager authIssuer >>= either failText pure
    bracket (openCallbackSocket) close $ \listener -> do
        port <- callbackPort listener
        let redirect = "http://127.0.0.1:" <> Text.pack (show port) <> "/callback"
        registration <- case metadata.registrationEndpoint of
            Nothing -> pure (OAuth.ClientRegistration "mcp-client" Nothing)
            Just endpoint ->
                OAuth.registerClient manager endpoint [redirect] metadata.scopesSupportedByServer
                    >>= either failText pure
        verifier <- randomUrlBytes 32
        state <- randomUrlBytes 24
        let challenge = Base64.encodeUnpadded (BA.convert (hash (Encoding.encodeUtf8 verifier) :: Digest SHA256))
            scope = Text.unwords metadata.scopesSupportedByServer
            resourceUrl = fromMaybe serverUrl resource.resource
            authUrl = metadata.authorizationEndpoint
                <> "?response_type=code&client_id=" <> encode registration.clientId
                <> "&redirect_uri=" <> encode redirect
                <> "&code_challenge=" <> Encoding.decodeUtf8 challenge
                <> "&code_challenge_method=S256&state=" <> encode state
                <> (if Text.null scope then "" else "&scope=" <> encode scope)
                <> "&resource=" <> encode resourceUrl
        putStrLn ("Opening browser for MCP authorization: " <> Text.unpack authUrl)
        _ <- openBrowser authUrl
        callback <- timeout (5 * 60 * 1000000) (receiveCallback listener)
            >>= maybe (failText "Timed out waiting for MCP OAuth callback") pure
        when (callback.state /= Just state) (failText "MCP OAuth callback state mismatch")
        code <- maybe (failText "MCP OAuth callback did not contain an authorization code") pure callback.code
        OAuth.exchangeAuthorizationCode manager metadata.tokenEndpoint registration.clientId code redirect verifier (Just resourceUrl)
            >>= \case
                OAuth.OAuthTokenFailure err -> failText err
                OAuth.OAuthTokenSuccess tokens -> do
                    now <- round <$> getPOSIXTime
                    let tokenFile = OAuth.OAuthTokenFile
                            registration.clientId metadata.tokenEndpoint tokens.accessToken
                            (fromMaybe "" tokens.refreshToken)
                            (fmap (\seconds -> now + seconds) tokens.expiresIn)
                    when (Text.null tokenFile.tokenRefreshToken) $
                        putStrLn "Warning: MCP provider returned no refresh token; reauthorization may be required."
                    saveMcpOAuth serverUrl tokenFile >>= either failText (const (putStrLn "MCP authorization saved."))

logoutMcp :: Text -> IO ()
logoutMcp server = do
    home <- Dir.getHomeDirectory
    let path = mcpOAuthStorePath home server
    exists <- Dir.doesFileExist path
    when exists (Dir.removeFile path)

data Callback = Callback { code :: Maybe Text, state :: Maybe Text }

openCallbackSocket :: IO Socket
openCallbackSocket = do
    addr : _ <- getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just "127.0.0.1") (Just "0")
    sock <- socket (addrFamily addr) Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    listen sock 1
    pure sock

callbackPort :: Socket -> IO Int
callbackPort sock = do
    SockAddrInet port _ <- getSocketName sock
    pure (fromIntegral port)

receiveCallback :: Socket -> IO Callback
receiveCallback listener = bracket (fst <$> accept listener) close $ \sock -> do
    bytes <- Socket.recv sock 8192
    let requestLine = case BS8.lines bytes of line : _ -> line; _ -> ""
        target = case BS8.words requestLine of _method : path : _ -> path; _ -> "/"
        query = snd (BS.break (== 63) target)
        params = parseQuery (BS.drop 1 query)
        response = "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=utf-8\r\nContent-Length: "
            <> Encoding.encodeUtf8 (Text.pack (show (LBS.length OAuth.oauthCallbackSuccessPage)))
            <> "\r\nConnection: close\r\n\r\n" <> LBS.toStrict OAuth.oauthCallbackSuccessPage
    Socket.sendAll sock response
    pure Callback { code = lookup "code" params, state = lookup "state" params }

parseQuery :: BS.ByteString -> [(Text, Text)]
parseQuery raw = map parsePair (filter (not . BS.null) (BS.split 38 raw))
  where
    parsePair item =
        let (key, value) = BS.break (== 61) item
        in (decode key, decode (BS.drop 1 value))
    decode = Encoding.decodeUtf8 . urlDecode
    urlDecode = BS.concat . go
    go input = case BS.uncons input of
        Nothing -> []
        Just (37, a) -> case BS.uncons a of
            Just (x, b) -> case BS.uncons b of
                Just (y, rest) -> maybe [BS.singleton 37] (\v -> [BS.singleton v]) (hex x y) <> go rest
                _ -> [BS.singleton 37] <> go a
            _ -> [BS.singleton 37] <> go a
        Just (43, rest) -> BS.singleton 32 : go rest
        Just (x, rest) -> BS.singleton x : go rest
    hex x y = do
        a <- digit x
        b <- digit y
        pure (a * 16 + b)
    digit c
        | c >= 48 && c <= 57 = Just (c - 48)
        | c >= 65 && c <= 70 = Just (c - 55)
        | c >= 97 && c <= 102 = Just (c - 87)
        | otherwise = Nothing

randomUrlBytes :: Int -> IO Text
randomUrlBytes n = do
    bytes <- System.Entropy.getEntropy n
    pure (Encoding.decodeUtf8 (Base64.encodeUnpadded bytes))

encode :: Text -> Text
encode = Encoding.decodeUtf8 . urlEncode True . Encoding.encodeUtf8

openBrowser :: Text -> IO Bool
openBrowser url = do
    result <- tryAny (rawSystem "open" [Text.unpack url])
    case result of
        Right ExitSuccess -> pure True
        _ -> do
            result' <- tryAny (rawSystem "xdg-open" [Text.unpack url])
            pure (case result' of Right ExitSuccess -> True; _ -> False)

failText :: Text -> IO a
failText = ioError . userError . Text.unpack
