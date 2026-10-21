module Agent.Server.LocalServiceSpec (spec) where

import Agent.Server.Application
import Agent.Server.ApplicationSpec (fakeBackend, immediateRunner)
import Agent.Server.Config
import Agent.Server.LocalService
import Agent.Server.Supervisor
import Agent.Server.Types
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Exception.Safe (bracket)
import Control.Monad (void)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.ByteString.Char8 qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Network.HTTP.Types
import Network.HTTP.Types.Header (hHost)
import Network.Wai
import Network.Wai.Test hiding (request)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "local service HTTP lifecycle" do
    it "finishes the stop response without waiting for Warp shutdown" do
        withService immediateRunner \application headers -> do
            result <- timeout 250000 (request application "POST" ["v1", "service", "stop"] headers (object []))
            case result of
                Just response -> response.simpleStatus `shouldBe` status202
                Nothing -> expectationFailure "stop response waited for its own HTTP connection"

    it "requires the private bearer as well as the loopback host" do
        withService immediateRunner \application headers -> do
            missing <- request application "GET" ["v1", "service"] (filter ((/= hAuthorization) . fst) headers) Null
            missing.simpleStatus `shouldBe` status401
            foreignHost <- request application "GET" ["v1", "service"] ((hHost, "foreign.example") : filter ((/= hHost) . fst) headers) Null
            foreignHost.simpleStatus `shouldBe` status403
            valid <- request application "GET" ["v1", "service"] headers Null
            valid.simpleStatus `shouldBe` status200

    it "disconnects immediately and cancels only that client's active session" do
        firstStarted <- newEmptyMVar
        secondStarted <- newEmptyMVar
        firstCancelled <- newEmptyMVar
        secondCancelled <- newEmptyMVar
        let runner control turn = do
                let (started, cancelled) = if turn.turnSpecSessionId == "first-session"
                        then (firstStarted, firstCancelled) else (secondStarted, secondCancelled)
                control.turnControlRegisterCancel (void (tryPutMVar cancelled ()))
                putMVar started ()
                takeMVar cancelled
                pure (Left "Turn cancelled.")
        withService runner \application headers -> do
            first <- register application headers
            second <- register application headers
            let firstHeaders = ("X-Agent-Client-Id", ByteString.pack (Text.unpack first)) : headers
                secondHeaders = ("X-Agent-Client-Id", ByteString.pack (Text.unpack second)) : headers
            attach application headers first "first-session"
            attach application headers second "second-session"
            conflicting <- request application "POST" ["v1", "clients", second, "sessions", "first-session"] headers (object [])
            conflicting.simpleStatus `shouldBe` status409
            one <- request application "POST" ["v1", "sessions", "first-session", "turns"] firstHeaders (object ["input" .= ("first" :: Text)])
            two <- request application "POST" ["v1", "sessions", "second-session", "turns"] secondHeaders (object ["input" .= ("second" :: Text)])
            one.simpleStatus `shouldBe` status202
            two.simpleStatus `shouldBe` status202
            timeout 1_000_000 (takeMVar firstStarted >> takeMVar secondStarted) `shouldReturn` Just ()
            timeout 250_000 (request application "DELETE" ["v1", "clients", first] headers Null)
                >>= \case
                    Just closed -> closed.simpleStatus `shouldBe` status202
                    Nothing -> expectationFailure "disconnect waited for session cleanup"
            timeout 1_000_000 (readMVar firstCancelled) `shouldReturn` Just ()
            isEmptyMVar secondCancelled `shouldReturn` True
            late <- request application "POST" ["v1", "clients", first, "heartbeat"] headers (object [])
            late.simpleStatus `shouldBe` status409
            alive <- request application "POST" ["v1", "clients", second, "heartbeat"] headers (object [])
            alive.simpleStatus `shouldBe` status200
            stopping <- request application "POST" ["v1", "service", "stop"] headers (object [])
            stopping.simpleStatus `shouldBe` status409

withService :: TurnRunner -> (Application -> [Header] -> IO a) -> IO a
withService runner use = withSystemTempDirectory "local-service" \directory ->
    bracket (lookupEnv "HASKELL_AGENT_RUNTIME_DIR" <* setEnv "HASKELL_AGENT_RUNTIME_DIR" directory)
        (maybe (unsetEnv "HASKELL_AGENT_RUNTIME_DIR") (setEnv "HASKELL_AGENT_RUNTIME_DIR")) \_ ->
        withLocalService defaultServerConfig \config service -> do
            resolved <- resolveServerConfig config >>= either (fail . Text.unpack) pure
            token <- ByteString.readFile =<< maybe (fail "missing token file") pure config.serverTokenFile
            let headers = [(hHost, ByteString.pack ("127.0.0.1:" <> show config.serverPort)), (hAuthorization, "Bearer " <> token)]
                supervisorConfig = SupervisorConfig 2 2 10 10 10 10 64
            bracket (newSupervisor supervisorConfig runner) closeSupervisor \supervisor -> do
                app <- newApplication ApplicationConfig
                    {applicationLocalService = True, applicationMaximumRequestBytes = 65536, applicationOpenApiDocument = "{}"}
                    resolved.resolvedAuth fakeBackend supervisor
                withLocalApplication service resolved fakeBackend supervisor app (\wrapped -> use wrapped headers)

request :: Application -> Method -> [Text] -> [Header] -> Value -> IO SResponse
request application method path headers body = runSession (srequest (SRequest
    defaultRequest{requestMethod = method, pathInfo = path, requestHeaders = (hContentType, "application/json") : headers}
    (if method `elem` ["POST", "PATCH"] then encode body else ""))) application

register :: Application -> [Header] -> IO Text
register application headers = do
    response <- request application "POST" ["v1", "clients"] headers (object [])
    response.simpleStatus `shouldBe` status201
    either fail pure (eitherDecode response.simpleBody >>= parseEither (withObject "client" (.: "id")))

attach :: Application -> [Header] -> Text -> Text -> IO ()
attach application headers client session = do
    response <- request application "POST" ["v1", "clients", client, "sessions", session] headers (object [])
    response.simpleStatus `shouldBe` status200
