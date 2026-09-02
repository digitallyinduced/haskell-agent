module Agent.Server.ApplicationSpec (spec) where

import Agent.Server.Application
    ( ApplicationConfig(..)
    , newApplication
    )
import Agent.Server.Auth
import Agent.Server.Backend (Backend(..))
import Agent.Server.Supervisor
import Agent.Server.Types
import Control.Concurrent.MVar
    ( newEmptyMVar
    , putMVar
    , takeMVar
    )
import Control.Exception.Safe (bracket)
import Data.Aeson (object, (.=))
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Set qualified as Set
import Data.Text (Text)
import Network.HTTP.Types
    ( Header
    , Method
    , methodGet
    , methodPatch
    , methodPost
    , status200
    , status202
    , status400
    , status403
    , status409
    , status413
    , status415
    , status422
    )
import Network.Wai
    ( Application
    , defaultRequest
    , pathInfo
    , requestHeaders
    , requestMethod
    )
import Network.Wai.Test
    ( SRequest(..)
    , SResponse(..)
    , runSession
    , srequest
    )
import Test.Hspec

spec :: Spec
spec = describe "agent-server WAI application" do
    it "rejects requests without the strict loopback Host" do
        withApplication immediateRunner \application -> do
            response <- perform application
                methodGet
                ["healthz"]
                []
                ""
            response.simpleStatus `shouldBe` status403
            LBS8.unpack response.simpleBody
                `shouldContain` "\"requestId\":\"request-"

    it "keeps allowed CORS headers on authentication failures" do
        let auth = AuthConfig
                { authMode =
                    LoopbackHostAuth
                        (Set.fromList ["127.0.0.1:4096"])
                , authCorsOrigins =
                    Set.fromList ["https://client.example"]
                }
        withApplicationAuth auth immediateRunner \application -> do
            response <- perform application
                methodGet
                ["healthz"]
                [ ("Host", "attacker.example")
                , ("Origin", "https://client.example")
                ]
                ""
            response.simpleStatus `shouldBe` status403
            lookup
                "Access-Control-Allow-Origin"
                response.simpleHeaders
                `shouldBe` Just "https://client.example"

    it "serves health and attaches a request id" do
        withApplication immediateRunner \application -> do
            response <- perform application
                methodGet
                ["healthz"]
                validHeaders
                ""
            response.simpleStatus `shouldBe` status200
            lookup "X-Request-ID" response.simpleHeaders
                `shouldSatisfy` (/= Nothing)

    it "rejects an oversized body before JSON decoding" do
        withApplication immediateRunner \application -> do
            response <- perform application
                methodPost
                ["v1", "sessions"]
                validHeaders
                (LBS8.replicate 65 'x')
            response.simpleStatus `shouldBe` status413

    it "rejects media types that only prefix-match application/json" do
        withApplication immediateRunner \application -> do
            response <- perform application
                methodPost
                ["v1", "sessions"]
                [ ("Host", "127.0.0.1:4096")
                , ("Content-Type", "application/json-patch")
                ]
                "{}"
            response.simpleStatus `shouldBe` status415

    it "rejects request fields outside the published schema" do
        withApplication immediateRunner \application -> do
            response <- perform application
                methodPost
                ["v1", "sessions"]
                validHeaders
                "{\"unexpected\":true}"
            response.simpleStatus `shouldBe` status400

    it "rejects a non-atomic multi-field session patch" do
        withApplication immediateRunner \application -> do
            response <- perform application
                methodPatch
                ["v1", "sessions", "session-a"]
                validHeaders
                "{\"title\":\"new\",\"archived\":true}"
            response.simpleStatus `shouldBe` status422

    it "returns 409 for mutation while a session turn is active" do
        release <- newEmptyMVar
        started <- newEmptyMVar
        let runner _ _ = putMVar started () >> takeMVar release
                >> pure (Right ())
        withApplication runner \application -> do
            created <- perform application
                methodPost
                ["v1", "sessions", "session-a", "turns"]
                validHeaders
                "{\"input\":\"hello\"}"
            created.simpleStatus `shouldBe` status202
            takeMVar started
            patched <- perform application
                methodPatch
                ["v1", "sessions", "session-a"]
                validHeaders
                "{\"title\":\"new title\"}"
            patched.simpleStatus `shouldBe` status409
            putMVar release ()

withApplication
    :: TurnRunner
    -> (Application -> IO value)
    -> IO value
withApplication = withApplicationAuth auth
  where
    auth = AuthConfig
        { authMode =
            LoopbackHostAuth
                (Set.fromList ["127.0.0.1:4096"])
        , authCorsOrigins = Set.empty
        }

withApplicationAuth
    :: AuthConfig
    -> TurnRunner
    -> (Application -> IO value)
    -> IO value
withApplicationAuth auth runner action =
    bracket
        (newSupervisor supervisorConfig runner)
        closeSupervisor \supervisor -> do
            application <- newApplication
                ApplicationConfig
                    { applicationMaximumRequestBytes = 64
                    , applicationOpenApiDocument = "{}"
                    }
                auth
                fakeBackend
                supervisor
            action application
  where
    supervisorConfig = SupervisorConfig
        { supervisorMaxConcurrentTurns = 1
        , supervisorMaxQueuedTurns = 10
        , supervisorEventReplayLimit = 10
        }

fakeBackend :: Backend
fakeBackend = Backend
    { backendAdmitBoundary = \action ->
        Right <$> action (GatewayBoundary Nothing)
    , backendContinueBoundary = \_ action ->
        Right <$> action
    , backendTurnBoundaryGuard = \_ action ->
        Right <$> action
    , backendCheckReady = pure (Right ())
    , backendListModels =
        \_ -> pure (Right (object ["models" .= ([] :: [Int])]))
    , backendListSessions =
        \_ _ _ _ ->
            pure (Right (object ["sessions" .= ([] :: [Int])]))
    , backendCreateSession =
        \_ _ -> pure (Right sessionValue)
    , backendGetSession =
        \_ _ -> pure (Right sessionValue)
    , backendPatchSession =
        \_ _ _ -> pure (Right sessionValue)
    , backendDeleteSession =
        \_ _ -> pure (Right ())
    , backendSessionHistory =
        \_ _ _ _ ->
            pure (Right (object ["turns" .= ([] :: [Int])]))
    , backendForkSession =
        \_ _ _ -> pure (Right sessionValue)
    , backendRunTurn = immediateRunner
    }
  where
    sessionValue = object
        [ "id" .= ("session-a" :: String)
        ]

immediateRunner :: TurnRunner
immediateRunner _ _ = pure (Right ())

perform
    :: Application
    -> Method
    -> [Text]
    -> [Header]
    -> LBS8.ByteString
    -> IO SResponse
perform application method path headers body =
    runSession
        (srequest
            (SRequest
                defaultRequest
                    { requestMethod = method
                    , pathInfo = path
                    , requestHeaders = headers
                    }
                body))
        application

validHeaders :: [Header]
validHeaders =
    [ ("Host", "127.0.0.1:4096")
    , ("Content-Type", "application/json")
    ]
