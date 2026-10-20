{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, OverloadedRecordDot, BlockArguments, NumericUnderscores, LambdaCase #-}
module Agent.OpenAI.TurnSpec (spec) where

import Prelude
import Agent.Error (ApiError (..), ErrorType (..))
import Agent.Provider (BillingMode (..), Credential (..), Provider (..))
import qualified Agent.Provider as Provider
import Agent.Responses.Types
import qualified Agent.Responses.Types as Responses (ResponseCreateParams(..))
import Agent.OpenAI.Turn
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Exception
import Control.Monad (forever, void)
import Data.IORef
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Network.Socket as Socket
import qualified Network.WebSockets as WS
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "model turns through the WebSocket transport" do
    it "recovers from capacity without an agent-specific retry policy" do
        requests <- newIORef []
        result <- withModelServer requests (== 1) \provider ->
            withConnection provider \connection _ ->
                sendTurn connection request (Just "previous-response")
        fmap (.responseId) result `shouldBe` Right "completed-response"
        submitted <- readIORef requests
        length submitted `shouldBe` 2
        -- Raw wire inspection is generic protocol plumbing, not a domain payload.
        map (Aeson.parseMaybe (Aeson.withObject "request" (Aeson..: "previous_response_id"))) submitted
            `shouldBe` replicate 2 (Just ("previous-response" :: Text.Text))

    it "uses four native submissions total instead of multiplying retry budgets" do
        requests <- newIORef []
        result <- timeout 8_000_000 $ withModelServer requests (const True) \provider ->
            withConnection provider \connection _ ->
                sendTurn connection request Nothing
        result `shouldSatisfy` \case
            Just (Left (ProviderError OverloadedError _ (Just 1))) -> True
            _ -> False
        length <$> readIORef requests `shouldReturn` 4

    it "replays successful history when a rejected turn requires a replacement socket" do
        requests <- newIORef []
        result <- withModelServer requests (== 2) \provider ->
            withConnection provider \connection _ -> do
                first <- sendTurn connection request Nothing
                case first of
                    Left err -> pure (Left err)
                    Right response -> sendTurn connection
                        request { Responses.input = Just (ResponseInputItems [messageItem RoleUser [inputText "next input"]]) }
                        (Just response.responseId)
        fmap (.responseId) result `shouldBe` Right "completed-response"
        submitted <- readIORef requests
        length submitted `shouldBe` 3
        let input = Aeson.parseMaybe (Aeson.withObject "request" (Aeson..: "input")) (last submitted)
        input `shouldBe` Just (Aeson.toJSON
            [messageItem RoleUser [inputText "continue"], messageItem RoleUser [inputText "next input"]])
        Aeson.parseMaybe (Aeson.withObject "request" (Aeson..:? "previous_response_id")) (last submitted)
            `shouldBe` Just (Nothing :: Maybe Text.Text)

request :: ResponseCreateParams
request = defaultResponseCreateParams
    { model = Just "gpt-test"
    , input = Just (ResponseInputItems [messageItem RoleUser [inputText "continue"]])
    }

withModelServer :: IORef [Aeson.Value] -> (Int -> Bool) -> (Provider.TokenProvider -> IO result) -> IO result
withModelServer requests shouldFail action =
    Exception.bracket openListener Socket.close \listener -> do
        port <- Socket.socketPort listener
        let serve = forever do
                (socket, _) <- Socket.accept listener
                void (Exception.try (Exception.finally (do
                    pending <- WS.makePendingConnection socket WS.defaultConnectionOptions
                    connection <- WS.acceptRequest pending
                    forever do
                        bytes <- WS.receiveData connection :: IO LBS.ByteString
                        value <- either fail pure (Aeson.eitherDecode bytes)
                        modifyIORef' requests (<> [value])
                        count <- length <$> readIORef requests
                        WS.sendTextData connection $ Aeson.encode $
                            if shouldFail count then overloaded else completed
                    ) (Socket.close socket)) :: IO (Either WS.ConnectionException ()))
            credential = Credential
                { accessToken = "test-token"
                , accountId = "ws://127.0.0.1:" <> Text.pack (show port) <> "/v1/responses"
                , leaseId = Nothing
                , provider = OpenAIProvider
                }
            provider = Provider.tokenProvider SubscriptionBilled (\_ -> pure (Right credential))
        Async.withAsync serve (\_ -> action provider)

openListener :: IO Socket.Socket
openListener = do
    listener <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
    Socket.setSocketOption listener Socket.ReuseAddr 1
    Socket.bind listener (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
    Socket.listen listener 1
    pure listener

overloaded :: Aeson.Value
overloaded = Aeson.object
    [ "type" Aeson..= ("error" :: Text.Text)
    , "headers" Aeson..= Aeson.object ["retry-after" Aeson..= ("1" :: Text.Text)]
    , "error" Aeson..= Aeson.object
        [ "type" Aeson..= ("overloaded_error" :: Text.Text)
        , "message" Aeson..= ("Provider at capacity" :: Text.Text)
        , "resets_in_seconds" Aeson..= (1 :: Int)
        ]
    ]

completed :: Aeson.Value
completed = Aeson.object
    [ "type" Aeson..= ("response.completed" :: Text.Text)
    , "response" Aeson..= Aeson.object
        [ "id" Aeson..= ("completed-response" :: Text.Text)
        , "model" Aeson..= ("gpt-test" :: Text.Text)
        , "status" Aeson..= ("completed" :: Text.Text)
        , "output" Aeson..= ([] :: [Aeson.Value])
        ]
    ]

withConnection provider action = Provider.runWithTokenProvider provider \credential ->
    withTurnConnection Nothing credential action

messageItem role parts = MessageItem ResponseMessage
    { messageId = Nothing, role, content = MessageContentParts parts
    , status = Nothing, phase = Nothing, passthrough = Nothing }

inputText value = InputTextPart value Nothing
