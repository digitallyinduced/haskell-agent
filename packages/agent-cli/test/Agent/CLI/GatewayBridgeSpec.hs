module Agent.CLI.GatewayBridgeSpec (spec) where

import Agent.CLI.GatewayBridge
import Agent.CLI.ManagedTurn
import Agent.CLI.Permission (PermissionChoice(..))
import Agent.Loop (LoopEvent(..), defaultLoopDispatch)
import Agent.Json.Decode qualified as Hermes
import Agent.OsPath (unsafeToFilePath)
import Agent.Json (rawJsonBytes)
import Agent.ToolDispatch
    ( ToolArgumentStreamEvent(..)
    , ToolCallResult(..)
    , dispatchToolCall
    , functionToolCall
    )
import Agent.Tools.Types (AppTool(..), appToolHandlers)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync, wait)
import Control.Exception.Safe (bracket)
import Data.Aeson (Value(..))
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (listToMaybe)
import System.Directory
    ( createDirectoryIfMissing
    , getTemporaryDirectory
    , listDirectory
    , removePathForcibly
    )
import System.FilePath ((</>), takeExtension)
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.GatewayBridge" do
    it "round-trips a channel tool request through private files" $
        withBridgeRequest \request -> do
            let call = functionToolCall
                    "call-1"
                    "send_telegram_document"
                    "{\"path\":\"/tmp/report.pdf\",\"caption\":\"Report\"}"
            withAsync
                (dispatchToolCall
                    defaultLoopDispatch
                    (appToolHandlers (managedGatewayTools request))
                    call)
                \running -> do
                    bridgeRequest <- waitForBridgeRequest request
                    bridgeRequest.bridgeRequestKind `shouldBe` "send_document"
                    writeManagedBridgeResponse request ManagedBridgeResponse
                        { bridgeResponseVersion = 1
                        , bridgeResponseId = bridgeRequest.bridgeRequestId
                        , bridgeResponseOk = True
                        , bridgeResponseResult = Just (String "sent")
                        , bridgeResponseError = Nothing
                        }
                    result <- wait running
                    result.output `shouldBe` "sent"

    it "advertises Telegram allowlist tools on the managed gateway bridge" $
        withBridgeRequest \request -> do
            let names = map (.appToolName) (managedGatewayTools request)
            names `shouldContain` ["allow_telegram_user"]
            names `shouldContain` ["deny_telegram_user"]
            names `shouldContain` ["list_telegram_users"]

    it "uses the bridge for mutating-tool approval choices" $
        withBridgeRequest \request -> do
            let call = functionToolCall
                    "approval-1"
                    "apply_patch"
                    "*** Begin Patch"
            withAsync (requestManagedApproval request call) \running -> do
                bridgeRequest <- waitForBridgeRequest request
                bridgeRequest.bridgeRequestKind `shouldBe` "approval"
                writeManagedBridgeResponse request ManagedBridgeResponse
                    { bridgeResponseVersion = 1
                    , bridgeResponseId = bridgeRequest.bridgeRequestId
                    , bridgeResponseOk = True
                    , bridgeResponseResult = Just (String "allow_once")
                    , bridgeResponseError = Nothing
                    }
                wait running `shouldReturn` Just PermissionAllowOnce

    it "uses the bridge for filesystem root access choices" $
        withBridgeRequest \request -> do
            let root = "/tmp/agent-root"
            withAsync
                (requestManagedRootAccess request (unsafeEncodeUtf root))
                \running -> do
                    bridgeRequest <- waitForBridgeRequest request
                    bridgeRequest.bridgeRequestKind
                        `shouldBe` "filesystem_access"
                    rawJsonBytes bridgeRequest.bridgeRequestPayload
                        `shouldBe` "{\"path\":\"/tmp/agent-root\"}"
                    writeManagedBridgeResponse request ManagedBridgeResponse
                        { bridgeResponseVersion = 1
                        , bridgeResponseId = bridgeRequest.bridgeRequestId
                        , bridgeResponseOk = True
                        , bridgeResponseResult = Just (String "allow")
                        , bridgeResponseError = Nothing
                        }
                    wait running `shouldReturn` True

    it "publishes accumulated reasoning summaries and response text" $
        withBridgeRequest \request -> do
            publish <- newManagedLoopEventPublisher request
            publish (ReasoningDelta "Checking ")
            publish (ReasoningDelta "the files")
            publish (TextDelta "Found ")
            publish (TextDelta "the issue.")
            activity <- readManagedActivity request
            activity.managedActivityKind `shouldBe` "writing"
            activity.managedActivityMessage `shouldBe` "Writing reply…"
            activity.managedActivityReasoning `shouldBe` "Checking the files"
            activity.managedActivityResponse `shouldBe` "Found the issue."

    it "preserves accumulated reasoning across streamed tool arguments" $
        withBridgeRequest \request -> do
            publish <- newManagedLoopEventPublisher request
            publish (ReasoningDelta "Checking files")
            publish
                (ToolArgumentEvent
                    (ToolArgumentsDelta [] "{\"path\":"))
            activity <- readManagedActivity request
            activity.managedActivityKind `shouldBe` "thinking"
            activity.managedActivityMessage `shouldBe` "Thinking…"
            activity.managedActivityReasoning `shouldBe` "Checking files"

readManagedActivity :: ManagedTurnRequest -> IO ManagedActivity
readManagedActivity request = do
    bytes <- LBS.readFile
        (unsafeToFilePath (managedBridgeActivityPath request))
    case Hermes.decodeEither managedActivityDecoder (LBS.toStrict bytes) of
        Left err -> expectationFailure (show err) >> fail (show err)
        Right value -> pure value

withBridgeRequest :: (ManagedTurnRequest -> IO a) -> IO a
withBridgeRequest action =
    withTempDir "gateway-bridge-spec-" \dir -> do
        let request =
                managedTurnRequestWithGateway
                    dir
                    ManagedTurnContext
                        { managedGateway = "telegram"
                        , managedChatId = 123
                        , managedMessageThreadId = Nothing
                        , managedReplyToMessageId = Just 77
                        , managedUserId = 456
                        }
                    (managedTurnRequestFromText "hello")
        createDirectoryIfMissing True
            (unsafeToFilePath (managedBridgeRequestsDirectory request))
        createDirectoryIfMissing True
            (unsafeToFilePath (managedBridgeResponsesDirectory request))
        action request

waitForBridgeRequest :: ManagedTurnRequest -> IO ManagedBridgeRequest
waitForBridgeRequest request = go (100 :: Int)
  where
    directory =
        unsafeToFilePath (managedBridgeRequestsDirectory request)
    go 0 = expectationFailure "timed out waiting for bridge request" >> fail "timeout"
    go attempts = do
        files <- listDirectory directory
        case listToMaybe (filter ((== ".json") . takeExtension) files) of
            Nothing -> threadDelay 20_000 >> go (attempts - 1)
            Just name -> do
                bytes <- LBS.readFile (directory </> name)
                case Hermes.decodeEither managedBridgeRequestDecoder
                        (LBS.toStrict bytes) of
                    Left _ -> threadDelay 20_000 >> go (attempts - 1)
                    Right value -> pure value

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir prefix action = do
    tmp <- getTemporaryDirectory
    bracket
        (mkdtemp (tmp </> prefix))
        removePathForcibly
        action
