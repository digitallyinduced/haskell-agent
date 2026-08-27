module Agent.Responses.ResponseMergeSpec (spec) where

import Agent.Json (emptyExtensions)
import qualified Agent.Json.Decoder as Decoder
import Agent.Responses.ResponseMerge
import Agent.Responses.Types
import Data.ByteString (ByteString)
import Data.List (nub)
import qualified Data.Text as Text
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
    ( Arbitrary(..)
    , chooseInt
    , elements
    , vectorOf
    , (===)
    )

spec :: Spec
spec = describe "typed response merging" do
    it "fills an empty completed output from streamed done items" do
        (mergeCompletedResponseOutput [toolCall] (response [])).output
            `shouldBe` [toolCall]

    it "keeps terminal output and appends missing streamed items" do
        (mergeCompletedResponseOutput [toolCall] (response [assistantMessage])).output
            `shouldBe` [assistantMessage, toolCall]

    it "deduplicates streamed items by typed item kind and identity" do
        (mergeCompletedResponseOutput [toolCall] (response [toolCall])).output
            `shouldBe` [toolCall]

    it "retains lifecycle base fields and gives the terminal response precedence" do
        let base = (response [])
                { responseId = "base"
                , model = "base-model"
                , user = Just "base-user"
                }
            terminal = (response [])
                { responseId = "terminal"
                , model = ""
                , usage = Just usageValue
                }
        let expected :: Response
            expected = terminal
                { model = "base-model"
                , user = Just "base-user"
                }
        mergeResponseFragments [base, terminal]
            `shouldBe` Just expected

    it "normalizes response.done to completed" do
        let done :: Response
            done = (response []) { status = ResponseInProgress }
        (mergeDoneResponse Nothing [] done).status
            `shouldBe` ResponseCompleted

    it "preserves an explicit failed status on response.done" do
        done <- decodeFragment
            ( "{\"status\":\"failed\",\"error\":"
                <> "{\"message\":\"boom\"}}"
            )
        (mergeDoneResponse Nothing [] done).status
            `shouldBe` ResponseFailed

    it "lets an explicit null clear an earlier lifecycle field" do
        base <- decodeFragment
            ( "{\"user\":\"earlier\",\"usage\":"
                <> "{\"input_tokens\":1,\"output_tokens\":2,"
                <> "\"total_tokens\":3}}"
            )
        terminal <- decodeFragment
            "{\"user\":null,\"usage\":null}"
        let merged = mergeResponseFragments [base, terminal]
        (.user) <$> merged `shouldBe` Just Nothing
        (.usage) <$> merged `shouldBe` Just Nothing

    it "does not let an omitted fragment object replace an earlier value" do
        base <- decodeFragment
            "{\"object\":\"custom_response\"}"
        terminal <- decodeFragment
            "{\"status\":\"completed\"}"
        (.object) <$> mergeResponseFragments [base, terminal]
            `shouldBe` Just "custom_response"

    modifyMaxSuccess (const 300) $
        prop "overlaying typed lifecycle responses is associative" $
            \(ResponseFragments (first, second, third)) ->
                let mergedFirst =
                        mergeResponseFragments [first, second, third]
                    mergedPair = do
                        pair <- mergeResponseFragments [first, second]
                        mergeResponseFragments [pair, third]
                in mergedFirst === mergedPair

    modifyMaxSuccess (const 300) $
        prop "later lifecycle fields win while unrelated fields survive" $
            \(LifecycleValues (firstUser, secondId)) ->
                let first = (response [])
                        { responseId = "first"
                        , user = Just (Text.pack (show firstUser))
                        }
                    second = (response [])
                        { responseId = Text.pack (show secondId)
                        , user = Nothing
                        }
                    expected :: Response
                    expected = second { user = first.user }
                in mergeResponseFragments [first, second]
                    === Just expected

    modifyMaxSuccess (const 300) $
        prop "merging identifiable streamed items is idempotent" $
            \(GeneratedItems items) ->
                let once = mergeCompletedResponseOutput items (response [])
                    twice = mergeCompletedResponseOutput items once
                in twice === once

response :: [ResponseItem] -> Response
response items = Response
    { responseId = "resp_1"
    , createdAt = 0
    , error = Nothing
    , incompleteDetails = Nothing
    , instructions = Nothing
    , metadata = Nothing
    , model = "gpt-test"
    , object = "response"
    , output = items
    , parallelToolCalls = Nothing
    , temperature = Nothing
    , toolChoice = Nothing
    , tools = Nothing
    , topP = Nothing
    , background = Nothing
    , completedAt = Nothing
    , conversation = Nothing
    , maxOutputTokens = Nothing
    , maxToolCalls = Nothing
    , moderation = Nothing
    , previousResponseId = Nothing
    , prompt = Nothing
    , promptCacheKey = Nothing
    , promptCacheOptions = Nothing
    , promptCacheRetention = Nothing
    , reasoning = Nothing
    , safetyIdentifier = Nothing
    , serviceTier = Nothing
    , status = ResponseCompleted
    , text = Nothing
    , topLogprobs = Nothing
    , truncation = Nothing
    , usage = Nothing
    , user = Nothing
    , extraFields = emptyExtensions
    }

decodeFragment :: ByteString -> IO Response
decodeFragment bytes =
    case Decoder.decode responseFragmentDecoder bytes of
        Left err ->
            expectationFailure
                (Text.unpack (Decoder.renderDecodeError err))
                >> pure (response [])
        Right value -> pure value

usageValue :: ResponseUsage
usageValue = ResponseUsage
    { inputTokens = 1
    , inputTokensDetails = Nothing
    , outputTokens = 2
    , outputTokensDetails = Nothing
    , totalTokens = 3
    , extraFields = emptyExtensions
    }

assistantMessage :: ResponseItem
assistantMessage =
    MessageItem ResponseMessage
        { messageId = Just "msg_1"
        , role = RoleAssistant
        , content = MessageContentParts
            [OutputTextPart "thinking..." Nothing Nothing emptyExtensions]
        , status = Nothing
        , phase = Nothing
        , passthrough = Nothing
        , extraFields = emptyExtensions
        }

newtype ResponseFragments =
    ResponseFragments (Response, Response, Response)

instance Show ResponseFragments where
    show (ResponseFragments values) = show values

instance Arbitrary ResponseFragments where
    arbitrary = ResponseFragments <$> ((,,)
        <$> generatedResponse
        <*> generatedResponse
        <*> generatedResponse)
    shrink _ = []

generatedResponse = do
    identifier <- chooseInt (1, 1000)
    modelChoice <- elements ["", "model-a", "model-b"]
    userChoice <- elements [Nothing, Just "user-a", Just "user-b"]
    outputChoice <- elements [[], [toolCall], [assistantMessage]]
    pure (response outputChoice)
        { responseId = "response-" <> Text.pack (show identifier)
        , model = modelChoice
        , user = userChoice
        }

newtype GeneratedItems = GeneratedItems [ResponseItem]
    deriving (Show)

instance Arbitrary GeneratedItems where
    arbitrary = do
        identifiers <- vectorOf 6 (chooseInt (0, 20))
        pure $ GeneratedItems
            [ FunctionCallItem FunctionCall
                { itemId = Just
                    ("streamed-" <> Text.pack (show identifier))
                , callId = "call-" <> Text.pack (show identifier)
                , name = "generated"
                , namespace = Nothing
                , arguments = "{}"
                , encryptedFunctionArgs = Nothing
                , status = Nothing
                , extraFields = emptyExtensions
                }
            | identifier <- nub identifiers
            ]
    shrink _ = []

newtype LifecycleValues = LifecycleValues (Int, Int)
    deriving (Show)

instance Arbitrary LifecycleValues where
    arbitrary =
        LifecycleValues <$> ((,) <$> chooseInt (1, 1000) <*> chooseInt (1, 1000))
    shrink _ = []

toolCall :: ResponseItem
toolCall =
    FunctionCallItem FunctionCall
        { itemId = Just "fc_1"
        , callId = "call_1"
        , name = "echo_text"
        , namespace = Nothing
        , arguments = "{\"text\":\"ok\"}"
        , encryptedFunctionArgs = Nothing
        , status = Nothing
        , extraFields = emptyExtensions
        }
