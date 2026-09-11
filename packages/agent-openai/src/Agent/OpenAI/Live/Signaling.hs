-- | Codex subscription signaling. Media belongs to the scoped WebRTC peer;
-- this module never opens a microphone or falls back to API-key billing.
module Agent.OpenAI.Live.Signaling
    ( withCodexLiveCall
    , withGatewayLiveCall
    , withGatewayLiveCallPreparing
    , gatewayLiveEndpoint
    , decodeGatewayLiveAnswer
    , liveCallRequest
    , parseLiveCallIdentifier
    , validateLiveSdp
    , readLiveSdp
    , liveFailureReason
    ) where

import Agent.Error (ApiError(..))
import Agent.OpenAI.Http (postCodexJson)
import Agent.OpenAI.Live
import Agent.Provider
    ( BillingMode(..), Credential(..), Provider(OpenAIProvider), TokenProvider
    , ProviderAttemptFailure(..), ReplaySafety(..)
    , runWithTokenProviderAttempt, tokenProviderBillingMode
    )
import Control.Exception.Safe (tryAny)
import Control.Concurrent.Async (withAsync, link, wait)
import Data.Aeson (Value(..), object, (.=), decodeStrict', encode)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isHexDigit)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.Http.Client (getHeader, getStatusCode, setHeader)
import qualified System.IO.Streams as Streams
import System.Timeout (timeout)
import qualified Wuss
import qualified Network.URI as URI
import qualified Network.WebSockets as WS
import Text.Read (readMaybe)

-- | A single authenticated gateway connection owns call creation and the
-- sideband. The gateway retains the upstream credential for this lifetime.
withGatewayLiveCall
    :: Text -> Text -> LiveConfig -> Text
    -> (Text -> (IO () -> IO LiveInput -> (LiveEvent -> IO ()) -> IO ()) -> IO ())
    -> IO (Either ApiError ())
withGatewayLiveCall baseUrl bearer config offer use =
    withGatewayLiveCallPreparing baseUrl bearer config (pure offer) use

-- | Offer gathering and the gateway handshake are independent. Both belong
-- to this scope; failure or cancellation joins the offer worker before return.
withGatewayLiveCallPreparing
    :: Text -> Text -> LiveConfig -> IO Text
    -> (Text -> (IO () -> IO LiveInput -> (LiveEvent -> IO ()) -> IO ()) -> IO ())
    -> IO (Either ApiError ())
withGatewayLiveCallPreparing baseUrl bearer config prepare use =
    case gatewayLiveEndpoint baseUrl of
        Left problem -> pure (Left (ConnectionError problem))
        Right (secure, host, port, path) -> do
            outcome <- tryAny $ withAsync prepare \offerWorker -> do
                link offerWorker
                (if secure then Wuss.runSecureClientWith host (fromIntegral port)
                    else WS.runClientWith host port)
                    path liveConnectionOptions
                    [("Authorization", "Bearer " <> Text.encodeUtf8 bearer)] \connection -> do
                        response <- timeout 30_000_000 do
                            offer <- wait offerWorker
                            request <- either (fail . Text.unpack) pure (liveCallRequest config offer)
                            WS.sendTextData connection (encode request)
                            WS.receiveData connection
                        case response of
                            Nothing -> pure (Left (ConnectionError "Gateway voice setup timed out; it was not retried."))
                            Just bytes -> case decodeGatewayLiveAnswer bytes of
                                Left problem -> pure (Left (ConnectionError problem))
                                Right answer -> do
                                    use answer (\ready next receive -> ready >> runLiveSidebandConnection connection next receive)
                                    pure (Right ())
            pure $ case outcome of
                Left _ -> Left (ConnectionError "Gateway voice connection failed; no direct-account fallback was attempted.")
                Right result -> result

gatewayLiveEndpoint :: Text -> Either Text (Bool, String, Int, String)
gatewayLiveEndpoint baseUrl = do
    uri <- maybe invalid Right (URI.parseURI (Text.unpack baseUrl))
    authority <- maybe invalid Right uri.uriAuthority
    secure <- case uri.uriScheme of
        "https:" -> Right True
        "http:" | authority.uriRegName `elem` ["localhost", "127.0.0.1", "[::1]"] -> Right False
        _ -> invalid
    port <- case authority.uriPort of
        "" -> Right (if secure then 443 else 80)
        ':' : digits | Just value <- readMaybe digits, value > 0, value <= 65535 -> Right value
        _ -> invalid
    if null authority.uriRegName || not (null authority.uriUserInfo)
        || not (null uri.uriQuery) || not (null uri.uriFragment)
        then invalid
        else Right (secure, authority.uriRegName, port,
            reverse (dropWhile (== '/') (reverse uri.uriPath)) <> "/v1/voice")
  where
    invalid = Left "Invalid gateway voice endpoint; HTTPS is required except on loopback."

decodeGatewayLiveAnswer :: BS.ByteString -> Either Text Text
decodeGatewayLiveAnswer bytes
    | BS.length bytes > 131_072 = Left "Gateway voice answer exceeded its size limit."
    | otherwise = case decodeStrict' bytes of
        Just (Object response)
            | Just (String "voice.answer") <- KeyMap.lookup "type" response
            , Just (String answer) <- KeyMap.lookup "sdp" response -> validateLiveSdp (Text.encodeUtf8 answer)
        _ -> Left ("Gateway voice setup rejected: " <> liveFailureReason bytes)

-- | Callback receives the SDP answer and a sideband runner bound to this exact
-- call and credential. It must own/join media and sideband workers. No network
-- exception or response body is exposed: either may contain credentials/SDP.
-- Creation is attempted once. Even authentication failures are ReplayUnknown;
-- an ambiguous POST must never create a second inference session automatically.
withCodexLiveCall
    :: TokenProvider -> LiveConfig -> Text
    -> (Text -> (IO () -> IO LiveInput -> (LiveEvent -> IO ()) -> IO ()) -> IO ())
    -> IO (Either ApiError ())
withCodexLiveCall provider config offer use
    | tokenProviderBillingMode provider /= SubscriptionBilled =
        pure (Left (CredentialError "Voice requires a local ChatGPT sign-in, not an OpenAI API key."))
    | otherwise = case liveCallRequest config offer of
        Left problem -> pure (Left (ConnectionError problem))
        Right body -> runWithTokenProviderAttempt provider \credential ->
            fmap (first (ProviderAttemptFailure ReplayUnknown)) $
            if credential.provider /= OpenAIProvider
                then pure (Left (CredentialError "Voice requires a local ChatGPT sign-in."))
                else do
                    outcome <- tryAny do
                        created <- timeout 20_000_000 $
                            postCodexJson "https://chatgpt.com/backend-api/codex?intent=quicksilver&architecture=avas"
                                "realtime/calls"
                                credential.accessToken credential.accountId
                                (\request -> request >> setHeader "Accept" "application/sdp"
                                    >> setHeader "openai-alpha" "quicksilver=v2"
                                    >> setHeader "originator" "haskell-agent") body
                                \response stream ->
                                    if getStatusCode response < 200 || getStatusCode response >= 300
                                        then do
                                            diagnostic <- readLiveFailure stream
                                            pure (Left (ConnectionError
                                                ("Codex voice call creation returned HTTP " <> Text.pack (show (getStatusCode response)) <> ": " <> diagnostic)))
                                        else case getHeader response "Location" >>= either (const Nothing) Just . Text.decodeUtf8' of
                                            Nothing -> pure (Left (ConnectionError "Codex voice response is missing its call identifier."))
                                            Just location -> case parseLiveCallIdentifier location of
                                                Left problem -> pure (Left (ConnectionError problem))
                                                Right identifier -> fmap (fmap (\sdp -> (identifier, sdp))) (readLiveSdp stream)
                        case created of
                            Nothing -> pure (Left (ConnectionError "Codex voice call creation timed out; it was not retried."))
                            Just (Left problem) -> pure (Left problem)
                            Just (Right (identifier, answer)) -> do
                                let headers =
                                        [("Authorization", "Bearer " <> Text.encodeUtf8 credential.accessToken)
                                        ,("openai-alpha", "quicksilver=v2")
                                        ,("originator", "haskell-agent")]
                                        <> [("chatgpt-account-id", Text.encodeUtf8 credential.accountId) | not (Text.null credential.accountId)]
                                    sideband ready next receive = Wuss.runSecureClientWith "api.openai.com" 443
                                        ("/v1/live/" <> Text.unpack identifier) liveConnectionOptions headers
                                        (\connection -> ready >> runLiveSidebandConnection connection next receive)
                                use answer sideband
                                pure (Right ())
                    pure $ case outcome of
                        Left _ -> Left (ConnectionError "Codex voice signaling failed; the call was not retried.")
                        Right result -> result

-- Read only a bounded error document and expose fixed classifications, never
-- arbitrary server messages (which may contain request data or credentials).
readLiveFailure :: Streams.InputStream BS.ByteString -> IO Text
readLiveFailure = collect 0 []
  where
    collect size chunks stream = Streams.read stream >>= \case
        Nothing -> pure (liveFailureReason (BS.concat (reverse chunks)))
        Just chunk
            | size + BS.length chunk > 16_384 -> pure "error response exceeded diagnostic limit"
            | otherwise -> collect (size + BS.length chunk) (chunk : chunks) stream

liveFailureReason :: BS.ByteString -> Text
liveFailureReason bytes = case decodeStrict' bytes of
    Just (Object root) -> classify (codes root <> case KeyMap.lookup "error" root of
        Just (Object nested) -> codes nested
        _ -> [])
    _ -> "server returned no structured error code"
  where
    codes value = [code | key <- ["code", "type"], Just (String code) <- [KeyMap.lookup key value]]
    classify values
        | any (`elem` values) ["insufficient_quota", "usage_limit_reached", "quota_exceeded"] = "account usage quota exhausted"
        | any (`elem` values) ["rate_limit_exceeded", "rate_limit_error", "too_many_requests"] = "request rate limit reached"
        | any (`elem` values) ["model_not_found", "model_not_available", "permission_denied", "access_denied"] = "model or voice access denied"
        | any (`elem` values) ["invalid_api_key", "authentication_error", "token_expired"] = "authentication rejected"
        | otherwise = "server returned an unrecognized error code"

liveCallRequest :: LiveConfig -> Text -> Either Text Value
liveCallRequest config offer = do
    _ <- validateLiveSdp (Text.encodeUtf8 offer)
    case sessionUpdate config.instructions config.voice config.initialHistory of
        Object envelope -> case KeyMap.lookup "session" envelope of
            Just (Object session) -> Right (object
                ["sdp" .= offer, "session" .= Object (KeyMap.insert "model" (String liveModel) (KeyMap.delete "id" session))])
            _ -> Left "Invalid voice session configuration."
        _ -> Left "Invalid voice session configuration."

-- Only the identifier is accepted, never the Location's host. Consequently a
-- server-supplied Location cannot redirect the bearer token to another origin.
parseLiveCallIdentifier :: Text -> Either Text Text
parseLiveCallIdentifier location
    | Text.length location > 4096 = invalid
    | otherwise = case reverse (Text.splitOn "/" (Text.takeWhile (\c -> c /= '?' && c /= '#') location)) of
        identifier : _ | valid identifier -> Right identifier
        _ -> invalid
  where
    invalid = Left "Invalid Codex voice call identifier."
    valid identifier = Text.length identifier <= 128 &&
        (("rtc_" `Text.isPrefixOf` identifier && Text.length identifier > 4
            && Text.all (\c -> isAsciiLower c || isAsciiUpper c || isDigit c || c == '_' || c == '-') identifier)
        || (map Text.length (Text.splitOn "-" identifier) == [8,4,4,4,12]
            && Text.all (\c -> isHexDigit c || c == '-') identifier))

validateLiveSdp :: BS.ByteString -> Either Text Text
validateLiveSdp bytes
    | BS.length bytes > 65_536 || BS.elem 0 bytes = invalid
    | otherwise = case Text.decodeUtf8' bytes of
        Right value | "v=0" `Text.isPrefixOf` value && any (Text.isPrefixOf "m=audio ") (Text.lines value) -> Right value
        _ -> invalid
  where invalid = Left "Invalid or oversized voice SDP."

readLiveSdp :: Streams.InputStream BS.ByteString -> IO (Either ApiError Text)
readLiveSdp = collect 0 []
  where
    collect size chunks stream = Streams.read stream >>= \case
        Nothing -> pure (first ConnectionError (validateLiveSdp (BS.concat (reverse chunks))))
        Just chunk
            | size + BS.length chunk > 65_536 -> pure (Left (ConnectionError "Voice SDP exceeds its size limit."))
            | otherwise -> collect (size + BS.length chunk) (chunk : chunks) stream
