-- | Tests for the xAI OAuth flows against an in-process mock of auth.x.ai.
module Agent.XAI.AuthSpec (spec) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.XAI.Auth
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as Base64Url
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Test.Hspec

spec :: Spec
spec = do
    describe "requestDeviceAuthorization" do
        it "posts the configured client id and scopes, and prefers the complete verification URL" do
            recorded <- newIORef []
            deviceCode <- withMockAuth recorded deviceCodeApp \options ->
                requestDeviceAuthorization options >>= expectRight
            deviceCode.deviceCode `shouldBe` "device-1"
            deviceCode.userCode `shouldBe` "ABCD-1234"
            deviceCode.verificationUrl `shouldBe` "https://accounts.example/activate?code=ABCD-1234"
            deviceCode.pollIntervalSeconds `shouldBe` 5

            [(path, form)] <- readIORef recorded
            path `shouldBe` "/oauth2/device/code"
            lookup "client_id" form `shouldBe` Just testClientId
            lookup "referrer" form `shouldBe` Just "grok-build"
            (Text.words <$> lookup "scope" form) `shouldBe` Just
                [ "openid", "profile", "email", "offline_access"
                , "grok-cli:access", "api:access"
                , "conversations:read", "conversations:write"
                , "workspaces:read", "workspaces:write"
                ]

        it "declares the team principal when minting a team token" do
            recorded <- newIORef []
            let teamOptions options = options
                    { principalType = Just "Team"
                    , principalId = Just "team-abc"
                    }
            _ <- withMockAuth recorded deviceCodeApp \options ->
                requestDeviceAuthorization (teamOptions options) >>= expectRight
            [(_, form)] <- readIORef recorded
            lookup "principal_type" form `shouldBe` Just "Team"
            lookup "principal_id" form `shouldBe` Just "team-abc"

            -- and again on the token exchange, not only on a later refresh
            tokenRecorded <- newIORef []
            _ <- withMockAuth tokenRecorded refreshApp \options ->
                pollDeviceAuthorization (teamOptions options) sampleDeviceCode >>= expectRight
            [(path, tokenForm)] <- readIORef tokenRecorded
            path `shouldBe` "/oauth2/token"
            lookup "principal_type" tokenForm `shouldBe` Just "Team"
            lookup "principal_id" tokenForm `shouldBe` Just "team-abc"

        it "reports a disabled device flow distinctly" do
            recorded <- newIORef []
            result <- withMockAuth recorded (respondStatus HTTP.status404 "{}") \options ->
                requestDeviceAuthorization options
            case result of
                Left message -> Text.unpack message `shouldContain` "disabled"
                Right _ -> expectationFailure "expected the 404 to surface as an error"

    describe "pollDeviceAuthorization" do
        it "treats authorization_pending as not-yet and then returns tokens" do
            recorded <- newIORef []
            calls <- newIORef (0 :: Int)
            let app path form
                    | path == "/oauth2/token" = do
                        call <- atomicModifyIORef' calls \n -> (n + 1, n + 1)
                        lookup "grant_type" form
                            `shouldBe` Just "urn:ietf:params:oauth:grant-type:device_code"
                        lookup "device_code" form `shouldBe` Just "device-1"
                        if call == 1
                            then pure (jsonResponse HTTP.status400 (Aeson.object
                                [ "error" Aeson..= ("authorization_pending" :: Text) ]))
                            else pure (jsonResponse HTTP.status200 (Aeson.object
                                [ "access_token" Aeson..= ("access-1" :: Text)
                                , "refresh_token" Aeson..= ("refresh-1" :: Text)
                                , "expires_in" Aeson..= (3600 :: Int)
                                ]))
                    | otherwise = pure (respondStatusValue HTTP.status404 "not found")
            withMockAuth recorded app \options -> do
                pending1 <- pollDeviceAuthorization options sampleDeviceCode >>= expectRight
                pending1 `shouldBe` Nothing
                completed <- pollDeviceAuthorization options sampleDeviceCode >>= expectRight
                case completed of
                    Nothing -> expectationFailure "expected tokens on the second poll"
                    Just tokens -> do
                        tokens.accessToken `shouldBe` "access-1"
                        tokens.refreshToken `shouldBe` Just "refresh-1"
                        tokens.expiresInSeconds `shouldBe` Just 3600

        it "surfaces a denial as an error" do
            recorded <- newIORef []
            result <- withMockAuth recorded
                (respondStatus HTTP.status400 "{\"error\":\"access_denied\"}")
                \options -> pollDeviceAuthorization options sampleDeviceCode
            case result of
                Left message -> Text.unpack message `shouldContain` "denied"
                Right _ -> expectationFailure "expected access_denied to fail"

    describe "refreshAccessToken" do
        it "rotates tokens and tolerates a non-rotating refresh token" do
            recorded <- newIORef []
            tokens <- withMockAuth recorded refreshApp \options ->
                refreshAccessToken options "refresh-old" >>= expectRight
            tokens.accessToken `shouldBe` "access-new"
            tokens.refreshToken `shouldBe` Nothing

            [(path, form)] <- readIORef recorded
            path `shouldBe` "/oauth2/token"
            lookup "grant_type" form `shouldBe` Just "refresh_token"
            lookup "refresh_token" form `shouldBe` Just "refresh-old"
            lookup "client_id" form `shouldBe` Just testClientId

        it "maps invalid_grant to a terminal authentication error" do
            recorded <- newIORef []
            result <- withMockAuth recorded
                (respondStatus HTTP.status400 "{\"error\":\"invalid_grant\"}")
                \options -> refreshAccessToken options "refresh-old"
            case result of
                Left (ProviderError AuthenticationError message _) ->
                    Text.unpack message `shouldContain` "invalid_grant"
                other -> expectationFailure ("expected AuthenticationError, got " <> show other)

        it "keeps server errors retryable" do
            recorded <- newIORef []
            result <- withMockAuth recorded
                (respondStatus HTTP.status503 "unavailable")
                \options -> refreshAccessToken options "refresh-old"
            case result of
                Left (HttpError 503 _) -> pure ()
                other -> expectationFailure ("expected HttpError 503, got " <> show other)

    describe "accountIdFromAccessToken" do
        it "prefers sub and falls back to the principal claim" do
            accountIdFromAccessToken (unsignedJwt (Aeson.object
                [ "sub" Aeson..= ("user-123" :: Text)
                , "principal_id" Aeson..= ("principal-9" :: Text)
                ])) `shouldBe` Just "user-123"
            accountIdFromAccessToken (unsignedJwt (Aeson.object
                [ "principal_id" Aeson..= ("principal-9" :: Text) ]))
                `shouldBe` Just "principal-9"
            accountIdFromAccessToken "not-a-jwt" `shouldBe` Nothing

    describe "emailFromToken" do
        it "extracts and trims the standard email claim" do
            emailFromToken (unsignedJwt (Aeson.object
                [ "email" Aeson..= (" person@example.com " :: Text) ]))
                `shouldBe` Just "person@example.com"
            emailFromToken (unsignedJwt (Aeson.object []))
                `shouldBe` Nothing

--------------------------------------------------------------------------------
-- Mock auth server
--------------------------------------------------------------------------------

type FormFields = [(Text, Text)]

withMockAuth
    :: IORef [(Text, FormFields)]
    -> (Text -> FormFields -> IO Wai.Response)
    -> (OAuthOptions -> IO a)
    -> IO a
withMockAuth recorded handler action =
    Warp.testWithApplication (pure app) \port ->
        action (defaultOAuthOptions testClientId)
            { issuer = "http://127.0.0.1:" <> show port }
  where
    app waiRequest respond = do
        requestBody <- Wai.strictRequestBody waiRequest
        let path = "/" <> Text.intercalate "/" (Wai.pathInfo waiRequest)
            form =
                [ (Text.decodeUtf8 name, Text.decodeUtf8 value)
                | (name, Just value) <- HTTP.parseQuery (LBS.toStrict requestBody)
                ]
        atomicModifyIORef' recorded \requests -> (requests <> [(path, form)], ())
        respond =<< handler path form

deviceCodeApp :: Text -> FormFields -> IO Wai.Response
deviceCodeApp path _form
    | path == "/oauth2/device/code" = pure $ jsonResponse HTTP.status200 $ Aeson.object
        [ "device_code" Aeson..= ("device-1" :: Text)
        , "user_code" Aeson..= ("ABCD-1234" :: Text)
        , "verification_uri" Aeson..= ("https://accounts.example/activate" :: Text)
        , "verification_uri_complete"
            Aeson..= ("https://accounts.example/activate?code=ABCD-1234" :: Text)
        , "expires_in" Aeson..= (600 :: Int)
        ]
    | otherwise = pure (respondStatusValue HTTP.status404 "not found")

refreshApp :: Text -> FormFields -> IO Wai.Response
refreshApp path _form
    | path == "/oauth2/token" = pure $ jsonResponse HTTP.status200 $ Aeson.object
        [ "access_token" Aeson..= ("access-new" :: Text) ]
    | otherwise = pure (respondStatusValue HTTP.status404 "not found")

respondStatus :: HTTP.Status -> LBS.ByteString -> Text -> FormFields -> IO Wai.Response
respondStatus status responseBody _path _form =
    pure (Wai.responseLBS status [("Content-Type", "application/json")] responseBody)

respondStatusValue :: HTTP.Status -> LBS.ByteString -> Wai.Response
respondStatusValue status responseBody =
    Wai.responseLBS status [("Content-Type", "application/json")] responseBody

jsonResponse :: HTTP.Status -> Aeson.Value -> Wai.Response
jsonResponse status value =
    Wai.responseLBS status [("Content-Type", "application/json")] (Aeson.encode value)

sampleDeviceCode :: DeviceAuthorization
sampleDeviceCode = DeviceAuthorization
    { deviceCode = "device-1"
    , userCode = "ABCD-1234"
    , verificationUrl = "https://accounts.example/activate"
    , pollIntervalSeconds = 1
    , expiresInSeconds = Just 600
    }

testClientId :: Text
testClientId = "client-under-test"

unsignedJwt :: Aeson.Value -> Text
unsignedJwt claims =
    Text.intercalate "."
        [ encodeSegment "{\"alg\":\"none\"}"
        , encodeSegment (LBS.toStrict (Aeson.encode claims))
        , ""
        ]
  where
    encodeSegment :: BS.ByteString -> Text
    encodeSegment = Text.decodeUtf8 . Base64Url.encodeUnpadded

expectRight :: Show e => Either e a -> IO a
expectRight = \case
    Left err -> expectationFailure ("expected Right, got Left " <> show err) >> fail "unreachable"
    Right value -> pure value
