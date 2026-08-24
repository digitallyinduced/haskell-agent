module Agent.Responses.LoopBackendSpec (spec) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Loop
    ( Backend(..)
    , FileAttachment(..)
    , ImageAttachment(..)
    , TurnInput(..)
    )
import Agent.Provider
    ( Credential(..)
    , BillingMode(..)
    , FailedCredential(..)
    , Provider(..)
    , tokenProvider
    )
import Agent.Responses.LoopBackend (tokenProviderStatelessResponsesBackend)
import Agent.Responses.LoopBackend (turnInputsToItems)
import Agent.Responses.Types
    ( MessageContent(..)
    , ResponseContentPart(..)
    , ResponseItem(..)
    , ResponseMessage(..)
    , ResponseRole(..)
    , defaultResponseCreateParams
    )
import Data.IORef
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "tokenProviderStatelessResponsesBackend" do
    it "encodes file attachments as input_file parts" do
        let image = ImageAttachment "image/png" "png-bytes"
            file = FileAttachment (Just "notes.txt") "text/plain" "file-bytes"
        case turnInputsToItems
                [UserMultimodalFiles "see this" [image] [file]] of
            [MessageItem message] -> do
                message.role `shouldBe` RoleUser
                case message.content of
                    MessageContentParts parts ->
                        parts `shouldSatisfy` \ps ->
                            any isInputFile ps && any isInputImage ps
                    _ -> expectationFailure "expected multimodal message parts"
            other -> expectationFailure ("unexpected items: " <> show other)

    it "reacquires a credential after an authentication rejection" do
        attempts <- newIORef []
        let first = credential "first"
            second = credential "second"
            provider = tokenProvider SubscriptionBilled \failed ->
                pure $ Right $ case failed of
                Nothing -> first
                Just FailedCredential{} -> second
            send current _params _onEvent = do
                modifyIORef' attempts (<> [current])
                pure $ Left $
                    if current == first
                        then ProviderError AuthenticationError "rejected" Nothing
                        else ConnectionError "stopped after failover"
            backend =
                tokenProviderStatelessResponsesBackend provider send
                    (pure defaultResponseCreateParams)

        result <- backend.submitTurn [] Nothing [UserMessage "hello"]
            (const (pure ()))

        result `shouldBe` Left (ConnectionError "stopped after failover")
        readIORef attempts `shouldReturn` [first, second]

credential :: String -> Credential
credential label = Credential
    { accessToken = "token-" <> Text.pack label
    , accountId = Text.pack label
    , leaseId = Nothing
    , provider = OpenRouterProvider
    }

isInputFile :: ResponseContentPart -> Bool
isInputFile = \case
    InputFilePart{} -> True
    _ -> False

isInputImage :: ResponseContentPart -> Bool
isInputImage = \case
    InputImagePart{} -> True
    _ -> False
