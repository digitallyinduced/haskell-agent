module Agent.MCP.OAuthSpec (spec) where

import Agent.MCP.OAuth
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Either (isLeft, isRight)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "Agent.MCP.OAuth" do
    describe "parseWwwAuthenticate" do
        it "parses quoted parameters" do
            parseWwwAuthenticate
                "Bearer resource_metadata=\"https://mcp.example.com/.well-known/oauth-protected-resource\", scope=\"files:read files:write\""
                `shouldBe` Just WwwAuthenticateChallenge
                    { challengeResourceMetadata = Just "https://mcp.example.com/.well-known/oauth-protected-resource"
                    , challengeScope = Just "files:read files:write"
                    , challengeError = Nothing
                    , challengeErrorDescription = Nothing
                    }

        it "parses unquoted parameters and case-insensitive names" do
            parseWwwAuthenticate "bearer Resource_Metadata=https://x.example/prm, SCOPE=files:read"
                `shouldBe` Just WwwAuthenticateChallenge
                    { challengeResourceMetadata = Just "https://x.example/prm"
                    , challengeScope = Just "files:read"
                    , challengeError = Nothing
                    , challengeErrorDescription = Nothing
                    }

        it "parses insufficient_scope challenges with mixed quoting and escapes" do
            parseWwwAuthenticate
                "Bearer error=insufficient_scope, scope=\"files:write\", resource_metadata=\"https://mcp.example.com/.well-known/oauth-protected-resource\", error_description=\"File \\\"write\\\" permission required\""
                `shouldBe` Just WwwAuthenticateChallenge
                    { challengeResourceMetadata = Just "https://mcp.example.com/.well-known/oauth-protected-resource"
                    , challengeScope = Just "files:write"
                    , challengeError = Just "insufficient_scope"
                    , challengeErrorDescription = Just "File \"write\" permission required"
                    }

        it "skips non-Bearer challenges and finds Bearer among several" do
            parseWwwAuthenticate "Basic realm=\"mcp\"" `shouldBe` Nothing
            parseWwwAuthenticate "Digest realm=\"x\", nonce=\"abc\", Bearer scope=\"a b\""
                `shouldBe` Just WwwAuthenticateChallenge
                    { challengeResourceMetadata = Nothing
                    , challengeScope = Just "a b"
                    , challengeError = Nothing
                    , challengeErrorDescription = Nothing
                    }

        it "accepts a bare Bearer scheme and ignores unknown parameters" do
            parseWwwAuthenticate "Bearer" `shouldBe` Just (WwwAuthenticateChallenge Nothing Nothing Nothing Nothing)
            parseWwwAuthenticate "Bearer realm=\"mcp\", scope=\"tools\""
                `shouldBe` Just (WwwAuthenticateChallenge Nothing (Just "tools") Nothing Nothing)
            parseWwwAuthenticate "" `shouldBe` Nothing

        it "splits scopes on whitespace" do
            challengeScopes (WwwAuthenticateChallenge Nothing (Just "  files:read   files:write ") Nothing Nothing)
                `shouldBe` ["files:read", "files:write"]
            challengeScopes (WwwAuthenticateChallenge Nothing Nothing Nothing Nothing) `shouldBe` []

    describe "canonicalResourceUri" do
        it "strips fragments, queries, and trailing slashes while keeping port and path" do
            canonicalResourceUri "https://mcp.example.com/mcp/" `shouldBe` "https://mcp.example.com/mcp"
            canonicalResourceUri "https://mcp.example.com:8443/server/mcp#frag" `shouldBe` "https://mcp.example.com:8443/server/mcp"
            canonicalResourceUri "https://mcp.example.com/mcp?x=1" `shouldBe` "https://mcp.example.com/mcp"
            canonicalResourceUri "https://mcp.example.com/" `shouldBe` "https://mcp.example.com"

        it "lowercases scheme and host only" do
            canonicalResourceUri "HTTPS://MCP.Example.COM/Path/To" `shouldBe` "https://mcp.example.com/Path/To"

        it "reports the origin" do
            resourceOrigin "https://mcp.example.com:8443/server/mcp" `shouldBe` Just "https://mcp.example.com:8443"
            resourceOrigin "not a url" `shouldBe` Nothing
            resourceOrigin "1https://mcp.example.com/server" `shouldBe` Nothing
            resourceOrigin "https://?query" `shouldBe` Nothing
            resourceOrigin "https://user@mcp.example.com/server" `shouldBe` Nothing
            resourceOrigin "https://mcp.example.com:/server" `shouldBe` Nothing
            resourceOrigin "https://mcp.example.com:0/server" `shouldBe` Nothing
            resourceOrigin "https://mcp.example.com:65536/server" `shouldBe` Nothing

        it "extracts loopback redirect ports" do
            loopbackRedirectPort "http://127.0.0.1:43127/callback" `shouldBe` Just 43127
            loopbackRedirectPort "http://localhost:8080/callback" `shouldBe` Just 8080
            loopbackRedirectPort "http://[::1]:43127/callback" `shouldBe` Just 43127
            loopbackRedirectPort "https://example.com:443/callback" `shouldBe` Nothing
            loopbackRedirectPort "http://127.0.0.1/callback" `shouldBe` Nothing
            loopbackRedirectPort "http://user@127.0.0.1:43127/callback" `shouldBe` Nothing

    describe "protectedResourceMetadataUrls" do
        it "tries the path-aware well-known URI before the root" do
            protectedResourceMetadataUrls "https://example.com/public/mcp"
                `shouldBe`
                    [ "https://example.com/.well-known/oauth-protected-resource/public/mcp"
                    , "https://example.com/.well-known/oauth-protected-resource"
                    ]

        it "only tries the root for endpoints without a path" do
            protectedResourceMetadataUrls "https://example.com"
                `shouldBe` ["https://example.com/.well-known/oauth-protected-resource"]
            protectedResourceMetadataUrls "https://example.com:8443/"
                `shouldBe` ["https://example.com:8443/.well-known/oauth-protected-resource"]

        it "returns nothing for relative URLs" do
            protectedResourceMetadataUrls "example.com/mcp" `shouldBe` []

    describe "validateResourceMetadata" do
        let metadata resource = ProtectedResourceMetadata
                { resource = resource
                , authorizationServers = ["https://auth.example.com"]
                , scopesSupported = []
                }
        it "accepts documents without a resource or with the canonical resource" do
            validateResourceMetadata "https://mcp.example.com/mcp" (metadata Nothing) `shouldSatisfy` isRight
            validateResourceMetadata "https://mcp.example.com/mcp/" (metadata (Just "https://mcp.example.com/mcp"))
                `shouldSatisfy` isRight
            validateResourceMetadata "https://mcp.example.com/mcp" (metadata (Just "https://MCP.example.com/mcp/"))
                `shouldSatisfy` isRight

        it "accepts a root document describing the endpoint origin" do
            validateResourceMetadata "https://mcp.example.com/mcp" (metadata (Just "https://mcp.example.com/"))
                `shouldSatisfy` isRight

        it "rejects documents for other hosts or paths" do
            validateResourceMetadata "https://mcp.example.com/mcp" (metadata (Just "https://attacker.example/mcp"))
                `shouldSatisfy` isLeft
            validateResourceMetadata "https://mcp.example.com/mcp" (metadata (Just "https://mcp.example.com/other"))
                `shouldSatisfy` isLeft
            validateResourceMetadata "https://mcp.example.com:8443/mcp" (metadata (Just "https://mcp.example.com/mcp"))
                `shouldSatisfy` isLeft

    describe "authorizationServerMetadataUrls" do
        it "orders candidates for issuers with a path component" do
            authorizationServerMetadataUrls "https://auth.example.com/tenant1"
                `shouldBe`
                    [ "https://auth.example.com/.well-known/oauth-authorization-server/tenant1"
                    , "https://auth.example.com/.well-known/openid-configuration/tenant1"
                    , "https://auth.example.com/tenant1/.well-known/openid-configuration"
                    ]

        it "orders candidates for issuers without a path component" do
            authorizationServerMetadataUrls "https://auth.example.com"
                `shouldBe`
                    [ "https://auth.example.com/.well-known/oauth-authorization-server"
                    , "https://auth.example.com/.well-known/openid-configuration"
                    ]
            authorizationServerMetadataUrls "https://auth.example.com/"
                `shouldBe`
                    [ "https://auth.example.com/.well-known/oauth-authorization-server"
                    , "https://auth.example.com/.well-known/openid-configuration"
                    ]

    describe "validateIssuer" do
        it "requires the issuer to be byte-identical" do
            validateIssuer "https://auth.example.com" (asMetadata (Just "https://auth.example.com"))
                `shouldSatisfy` isRight
            validateIssuer "https://auth.example.com" (asMetadata (Just "https://auth.example.com/"))
                `shouldSatisfy` isLeft
            validateIssuer "https://attacker.example" (asMetadata (Just "https://honest.example"))
                `shouldSatisfy` isLeft
            validateIssuer "https://auth.example.com" (asMetadata Nothing) `shouldSatisfy` isLeft

    describe "checkPkceSupport" do
        it "refuses when code_challenge_methods_supported is absent" do
            checkPkceSupport (asMetadata (Just "https://auth.example.com")) { codeChallengeMethodsSupported = Nothing }
                `shouldSatisfy` isLeft

        it "refuses when S256 is not advertised" do
            checkPkceSupport (asMetadata (Just "https://auth.example.com")) { codeChallengeMethodsSupported = Just ["plain"] }
                `shouldSatisfy` isLeft

        it "accepts S256" do
            checkPkceSupport (asMetadata (Just "https://auth.example.com")) { codeChallengeMethodsSupported = Just ["plain", "S256"] }
                `shouldBe` Right ()

    describe "AuthorizationServerMetadata decoding" do
        it "parses the extended fields" do
            let json = "{\"issuer\":\"https://auth.example.com\",\"authorization_endpoint\":\"https://auth.example.com/authorize\",\"token_endpoint\":\"https://auth.example.com/token\",\"registration_endpoint\":\"https://auth.example.com/register\",\"code_challenge_methods_supported\":[\"S256\"],\"client_id_metadata_document_supported\":true,\"authorization_response_iss_parameter_supported\":true,\"scopes_supported\":[\"files:read\",\"offline_access\"],\"grant_types_supported\":[\"authorization_code\",\"refresh_token\"]}"
            Aeson.eitherDecode json `shouldBe` Right AuthorizationServerMetadata
                { issuer = Just "https://auth.example.com"
                , authorizationEndpoint = "https://auth.example.com/authorize"
                , tokenEndpoint = "https://auth.example.com/token"
                , registrationEndpoint = Just "https://auth.example.com/register"
                , codeChallengeMethodsSupported = Just ["S256"]
                , scopesSupportedByServer = ["files:read", "offline_access"]
                , clientIdMetadataDocumentSupported = True
                , authorizationResponseIssParameterSupported = True
                , grantTypesSupported = ["authorization_code", "refresh_token"]
                }

        it "defaults the optional flags" do
            let json = "{\"authorization_endpoint\":\"https://a/authorize\",\"token_endpoint\":\"https://a/token\"}"
            Aeson.eitherDecode json `shouldBe` Right (asMetadata Nothing)
                { authorizationEndpoint = "https://a/authorize"
                , tokenEndpoint = "https://a/token"
                , codeChallengeMethodsSupported = Nothing
                }

    describe "selectClientRegistration" do
        let pre = PreRegisteredClient "configured-client" (Just "secret")
            options = RegistrationOptions
                { registrationPreRegistered = Nothing
                , registrationClientIdMetadataUrl = Nothing
                , registrationStored = Nothing
                , registrationRedirectUri = "http://127.0.0.1:43127/callback"
                }
            metadata = (asMetadata (Just "https://auth.example.com"))
                { registrationEndpoint = Just "https://auth.example.com/register"
                , clientIdMetadataDocumentSupported = True
                }

        it "prefers pre-registered credentials" do
            selectClientRegistration
                options { registrationPreRegistered = Just pre, registrationClientIdMetadataUrl = Just "https://app.example/client.json" }
                metadata
                `shouldBe` Right (UsePreRegisteredClient pre)

        it "uses the client ID metadata document when the server supports it" do
            selectClientRegistration options { registrationClientIdMetadataUrl = Just "https://app.example/client.json" } metadata
                `shouldBe` Right (UseClientIdMetadataDocument "https://app.example/client.json")

        it "falls back to dynamic registration when metadata documents are unsupported" do
            selectClientRegistration
                options { registrationClientIdMetadataUrl = Just "https://app.example/client.json" }
                metadata { clientIdMetadataDocumentSupported = False }
                `shouldBe` Right (UseDynamicRegistration "https://auth.example.com/register")

        it "rejects metadata document URLs that are not https with a path" do
            selectClientRegistration options { registrationClientIdMetadataUrl = Just "http://app.example/client.json" } metadata
                `shouldSatisfy` isLeft
            selectClientRegistration options { registrationClientIdMetadataUrl = Just "https://app.example" } metadata
                `shouldSatisfy` isLeft
            validateClientIdMetadataUrl "https://app.example/client.json" `shouldBe` Right ()

        it "uses dynamic registration without configuration" do
            selectClientRegistration options metadata { clientIdMetadataDocumentSupported = False }
                `shouldBe` Right (UseDynamicRegistration "https://auth.example.com/register")

        it "reuses a stored dynamic registration only for the same issuer and redirect URI" do
            let stored = StoredClient
                    { storedIssuer = "https://auth.example.com"
                    , storedClientId = "dyn-client"
                    , storedSource = ClientIdDynamicRegistration
                    , storedRedirectUri = Just "http://127.0.0.1:43127/callback"
                    }
                plain = metadata { clientIdMetadataDocumentSupported = False }
            selectClientRegistration options { registrationStored = Just stored } plain
                `shouldBe` Right (ReuseDynamicRegistration "dyn-client")
            selectClientRegistration options { registrationStored = Just stored { storedIssuer = "https://other.example" } } plain
                `shouldBe` Right (UseDynamicRegistration "https://auth.example.com/register")
            selectClientRegistration options { registrationStored = Just stored { storedRedirectUri = Just "http://127.0.0.1:1/callback" } } plain
                `shouldBe` Right (UseDynamicRegistration "https://auth.example.com/register")
            selectClientRegistration options { registrationStored = Just stored { storedSource = ClientIdPreRegistered } } plain
                `shouldBe` Right (UseDynamicRegistration "https://auth.example.com/register")

        it "fails with an actionable error when no mechanism is available" do
            case selectClientRegistration options (asMetadata (Just "https://auth.example.com")) of
                Left err -> do
                    err `shouldSatisfy` Text.isInfixOf "oauth.clientId"
                    err `shouldSatisfy` Text.isInfixOf "oauth.clientIdMetadataUrl"
                Right plan -> expectationFailure ("unexpected plan " <> show plan)

        it "never selects a bogus fallback client id" do
            selectClientRegistration options (asMetadata (Just "https://auth.example.com")) `shouldSatisfy` isLeft

    describe "clientRegistrationPayload" do
        it "describes a native public client" do
            let payload = clientRegistrationPayload ClientRegistrationRequest
                    { registrationClientName = "Haskell Agent"
                    , registrationRedirectUris = ["http://127.0.0.1:43127/callback"]
                    , registrationScopes = ["files:read", "offline_access"]
                    }
            field "application_type" payload `shouldBe` Just (Aeson.String "native")
            field "client_name" payload `shouldBe` Just (Aeson.String "Haskell Agent")
            field "redirect_uris" payload `shouldBe` Just (Aeson.toJSON ["http://127.0.0.1:43127/callback" :: Text.Text])
            field "grant_types" payload `shouldBe` Just (Aeson.toJSON ["authorization_code", "refresh_token" :: Text.Text])
            field "response_types" payload `shouldBe` Just (Aeson.toJSON ["code" :: Text.Text])
            field "token_endpoint_auth_method" payload `shouldBe` Just (Aeson.String "none")
            field "scope" payload `shouldBe` Just (Aeson.String "files:read offline_access")

        it "omits scope when no scopes are requested" do
            field "scope" (clientRegistrationPayload (ClientRegistrationRequest "x" [] [])) `shouldBe` Nothing

    describe "scope selection" do
        it "prefers the challenge, then resource metadata, then configuration" do
            selectScopes (ScopeSources ["c"] ["m"] ["k"]) `shouldBe` ["c"]
            selectScopes (ScopeSources [] ["m1", "m2"] ["k"]) `shouldBe` ["m1", "m2"]
            selectScopes (ScopeSources [] [] ["k"]) `shouldBe` ["k"]
            selectScopes (ScopeSources [] [] []) `shouldBe` []

        it "unions scopes preserving order and dropping duplicates" do
            unionScopes ["a", "b"] ["b", "c", "a", ""] `shouldBe` ["a", "b", "c"]

        it "unions previously granted and additional scopes for step-up" do
            planScopes ScopePlan
                { scopeSources = ScopeSources ["files:write"] ["files:read"] []
                , scopePreviouslyGranted = ["files:read", "profile"]
                , scopeAdditional = ["admin"]
                , scopeAuthorizationServerSupported = []
                }
                `shouldBe` ["files:write", "files:read", "profile", "admin"]

        it "adds offline_access only when the authorization server advertises it" do
            planScopes (ScopePlan (ScopeSources ["files:read"] [] []) [] [] ["files:read", "offline_access"])
                `shouldBe` ["files:read", "offline_access"]
            planScopes (ScopePlan (ScopeSources ["files:read"] [] []) ["offline_access"] [] ["files:read"])
                `shouldBe` ["files:read"]
            planScopes (ScopePlan (ScopeSources [] [] []) [] [] []) `shouldBe` []

    describe "validateAuthorizationResponseIssuer" do
        let recorded = "https://auth.example.com"
        it "advertised and present: compares by simple string comparison" do
            validateAuthorizationResponseIssuer True recorded (Just recorded) `shouldBe` Right ()
            validateAuthorizationResponseIssuer True recorded (Just "https://attacker.example") `shouldSatisfy` isLeft

        it "advertised and absent: rejects the response" do
            validateAuthorizationResponseIssuer True recorded Nothing `shouldSatisfy` isLeft

        it "not advertised and present: still compares" do
            validateAuthorizationResponseIssuer False recorded (Just recorded) `shouldBe` Right ()
            validateAuthorizationResponseIssuer False recorded (Just "https://attacker.example") `shouldSatisfy` isLeft

        it "not advertised and absent: proceeds" do
            validateAuthorizationResponseIssuer False recorded Nothing `shouldBe` Right ()

        it "applies no normalization" do
            validateAuthorizationResponseIssuer False recorded (Just "https://auth.example.com/") `shouldSatisfy` isLeft
            validateAuthorizationResponseIssuer False recorded (Just "HTTPS://auth.example.com") `shouldSatisfy` isLeft
            validateAuthorizationResponseIssuer False recorded (Just "https://auth.example.com:443") `shouldSatisfy` isLeft

    describe "token records" do
        it "loads records written before the extra fields existed" do
            let legacy = "{\"client_id\":\"c\",\"token_endpoint\":\"https://a/token\",\"access_token\":\"at\",\"refresh_token\":\"rt\",\"expires_at\":null}"
            decodeOAuthTokenRecord legacy
                `shouldBe` Right (OAuthTokenFile "c" "https://a/token" "at" "rt" Nothing, emptyOAuthTokenFileExtra)

        it "round-trips the extra fields and omits absent keys" do
            let file = OAuthTokenFile "c" "https://a/token" "at" "rt" (Just 42)
                extra = emptyOAuthTokenFileExtra
                    { extraIssuer = Just "https://auth.example.com"
                    , extraScope = Just "files:read offline_access"
                    , extraResource = Just "https://mcp.example.com/mcp"
                    , extraClientIdSource = Just ClientIdDynamicRegistration
                    , extraRedirectUri = Just "http://127.0.0.1:43127/callback"
                    }
                encoded = encodeOAuthTokenRecord file extra
            decodeOAuthTokenRecord encoded `shouldBe` Right (file, extra)
            Aeson.eitherDecode encoded `shouldBe` Right file
            let keys = maybe [] KeyMap.keys (Aeson.decode encoded :: Maybe (KeyMap.KeyMap Aeson.Value))
            keys `shouldSatisfy` notElem "client_secret"
            keys `shouldSatisfy` notElem "client_id_metadata_url"
            keys `shouldSatisfy` elem "client_id_source"

        it "keeps client secrets out of Show output" do
            let extra = emptyOAuthTokenFileExtra { extraClientSecret = Just "top-secret" }
            show extra `shouldSatisfy` notElem' "top-secret"
            show (PreRegisteredClient "id" (Just "top-secret")) `shouldSatisfy` notElem' "top-secret"
            show (OAuthTokens "tok-secret-value" (Just "refresh-secret-value") Nothing Nothing)
                `shouldSatisfy` (\shown -> notElem' "tok-secret-value" shown && notElem' "refresh-secret-value" shown)
            show (OAuthTokenFile "c" "https://a/token" "tok-secret-value" "refresh-secret-value" Nothing)
                `shouldSatisfy` notElem' "secret-value"

        it "maps client id sources to stable text" do
            mapM_ (\source -> parseClientIdSource (clientIdSourceText source) `shouldBe` Just source)
                [ClientIdPreRegistered, ClientIdMetadataDocument, ClientIdDynamicRegistration]
            parseClientIdSource "unknown" `shouldBe` Nothing

asMetadata :: Maybe Text.Text -> AuthorizationServerMetadata
asMetadata issuer = AuthorizationServerMetadata
    { issuer = issuer
    , authorizationEndpoint = "https://auth.example.com/authorize"
    , tokenEndpoint = "https://auth.example.com/token"
    , registrationEndpoint = Nothing
    , codeChallengeMethodsSupported = Just ["S256"]
    , scopesSupportedByServer = []
    , clientIdMetadataDocumentSupported = False
    , authorizationResponseIssParameterSupported = False
    , grantTypesSupported = []
    }

field :: Aeson.Key -> Aeson.Value -> Maybe Aeson.Value
field key = \case
    Aeson.Object object -> KeyMap.lookup key object
    _ -> Nothing

notElem' :: String -> String -> Bool
notElem' needle haystack = not (Text.pack needle `Text.isInfixOf` Text.pack haystack)
