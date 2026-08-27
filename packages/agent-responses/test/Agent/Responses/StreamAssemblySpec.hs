module Agent.Responses.StreamAssemblySpec (spec) where

import Agent.Error (ApiError(..))
import Agent.Json (emptyExtensions)
import Agent.Responses.StreamAssembly
import Agent.Responses.Types
import Control.Applicative ((<|>))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
    ( Arbitrary(..)
    , chooseInt
    , counterexample
    , elements
    , listOf
    , (===)
    )

spec :: Spec
spec = describe "typed stream assembly" do
    it "merges output_item.done items into terminal output without duplicates" do
        result <- expectRight $ buildStreamResponse config
            [ outputDone 0 toolCall
            , completed (response [toolCall])
            ]
        result.output `shouldBe` [toolCall]

    it "uses lifecycle response bases and normalizes response.done" do
        result <- expectRight $ buildStreamResponse config
            [ created ((response []) { user = Just "retained" })
            , done ((response []) { user = Nothing, status = ResponseInProgress })
            ]
        result.user `shouldBe` Just "retained"
        result.status `shouldBe` ResponseCompleted

    it "accumulates function-call argument deltas" do
        result <- expectRight $ buildStreamResponse config
            [ outputAdded 0 toolCall
            , ResponseFunctionCallArgumentsDeltaEvent
                (Just "{\"value\":") (Just "fc_1") (Just 0)
                Nothing emptyExtensions
            , ResponseFunctionCallArgumentsDoneEvent
                (Just "{\"value\":1}") (Just "echo")
                (Just "fc_1") (Just 0) Nothing emptyExtensions
            , completed (response [])
            ]
        [arguments | FunctionCallItem FunctionCall { arguments } <- result.output]
            `shouldBe` ["{\"value\":1}"]

    it "accumulates custom-tool input deltas by call id" do
        result <- expectRight $ buildStreamResponse config
            [ outputAdded 0 customCall
            , ResponseCustomToolInputDeltaEvent
                (Just "*** Begin") Nothing (Just "custom-call")
                Nothing Nothing emptyExtensions
            , ResponseCustomToolInputDeltaEvent
                (Just " Patch") Nothing (Just "custom-call")
                Nothing Nothing emptyExtensions
            , completed (response [])
            ]
        [input | CustomToolCallItem CustomToolCall { input } <- result.output]
            `shouldBe` ["*** Begin Patch"]

    it "uses dedicated reasoning summary delta events" do
        result <- expectRight $ buildStreamResponse config
            [ outputAdded 0 reasoningItem
            , ResponseReasoningSummaryTextDeltaEvent
                (Just "Checked ") (Just "reason_1") (Just 0) (Just 0)
                Nothing emptyExtensions
            , ResponseReasoningSummaryTextDoneEvent
                (Just "reason_1") (Just 0) (Just 0)
                (Just "Checked repository") Nothing emptyExtensions
            , completed (response [])
            ]
        [text
            | ReasoningItemValue ReasoningItem
                { summary = [ReasoningSummaryPart { text }] } <- result.output
            ] `shouldBe` [Just "Checked repository"]

    it "assembles dedicated output-text deltas without an output item frame" do
        result <- expectRight $ buildStreamResponse config
            [ created (response [])
            , ResponseOutputTextDeltaEvent
                (Just "hello ") (Just "msg_1") (Just 0) (Just 0)
                Nothing Nothing emptyExtensions
            , ResponseOutputTextDoneEvent
                (Just "hello world") (Just "msg_1") (Just 0) (Just 0)
                Nothing emptyExtensions
            , completed (response [])
            ]
        [text
            | MessageItem ResponseMessage
                { content = MessageContentParts [OutputTextPart { text }] }
                <- result.output
            ] `shouldBe` ["hello world"]

    it "keeps done precedence over terminal after a late added overlay" do
        let streamedDone = renameCall "done" toolCall
            lateAdded = renameCall "late-added" toolCall
            terminal = renameCall "terminal" toolCall
        result <- expectRight $ buildStreamResponse config
            [ outputDone 0 streamedDone
            , outputAdded 0 lateAdded
            , completed (response [terminal])
            ]
        [name | FunctionCallItem FunctionCall { name } <- result.output]
            `shouldBe` ["late-added"]

    it "classifies terminal failures from typed response fields" do
        buildStreamResponse config
            [ ResponseFailedEvent
                ((response [])
                    { status = ResponseFailed
                    , error = Just ResponseError
                        { code = "overloaded"
                        , message = "try later"
                        , extraFields = emptyExtensions
                        }
                    })
                Nothing emptyExtensions
            ]
            `shouldBe` Left (ConnectionError "failed: try later")

    it "finishes collected state as incomplete after transport loss" do
        let state = applyStreamEvent
                (applyStreamEvent emptyStreamAssemblyState
                    (created (response [])))
                (outputDone 0 toolCall)
        result <- expectRight (finishAssembledIncomplete Nothing state)
        result.status `shouldBe` ResponseIncomplete
        result.output `shouldBe` [toolCall]

    modifyMaxSuccess (const 500) $
        prop "matches an independent model for adversarial indexed events" $
            \(AdversarialOperations operations) ->
                let events =
                        created (response [])
                            : map operationEvent operations
                            <> [completed (response [])]
                    expected =
                        map fst . Map.elems $
                            foldl applyExpected Map.empty operations
                in case buildStreamResponse config events of
                    Left err ->
                        counterexample
                            ( "unexpected assembly failure: " <> show err
                            <> "\noperations: " <> show operations
                            )
                            False
                    Right result ->
                        counterexample
                            ("operations: " <> show operations)
                            (result.output === expected)

    modifyMaxSuccess (const 300) $
        prop "keeps done precedence when a late added item is merged" $
            \doneFragment lateFragment terminalFragment ->
                let doneItem = fragmentItem doneFragment
                    lateItem = fragmentItem lateFragment
                    terminalItem = fragmentItem terminalFragment
                    events =
                        [ outputDone 0 doneItem
                        , outputAdded 0 lateItem
                        , completed (response [terminalItem])
                        ]
                in case buildStreamResponse config events of
                    Left err ->
                        counterexample
                            ("unexpected assembly failure: " <> show err)
                            False
                    Right result ->
                        result.output === [lateItem]

    modifyMaxSuccess (const 300) $
        prop "ignores all events after the first terminal lifecycle event" $
            \(LifecycleTrace (before, terminal, after)) ->
                let events =
                        map (created . responseWithId) before
                        <> [completed (responseWithId terminal)]
                        <> map (completed . responseWithId) after
                in case buildStreamResponse config events of
                    Left err ->
                        counterexample
                            ("unexpected assembly failure: " <> show err)
                            False
                    Right result ->
                        result.responseId === identifierText terminal

config :: StreamAssemblyConfig
config = StreamAssemblyConfig
    { missingCompletionMessage = "missing completion"
    , classifyStreamError =
        \streamError -> ConnectionError ("stream: " <> streamError.message)
    , classifyFailedResponse =
        \failure ->
            ConnectionError ("failed: " <> failedStreamResponseMessage failure)
    , incompleteAsFailure = True
    }

created :: Response -> ResponseStreamEvent
created value = ResponseCreatedEvent value Nothing emptyExtensions

completed :: Response -> ResponseStreamEvent
completed value = ResponseCompletedEvent value Nothing emptyExtensions

done :: Response -> ResponseStreamEvent
done value = ResponseDoneEvent value Nothing emptyExtensions

outputAdded :: Int -> ResponseItem -> ResponseStreamEvent
outputAdded index value =
    ResponseOutputItemAddedEvent value (Just index) Nothing emptyExtensions

outputDone :: Int -> ResponseItem -> ResponseStreamEvent
outputDone index value =
    ResponseOutputItemDoneEvent value (Just index) Nothing emptyExtensions

response :: [ResponseItem] -> Response
response items = Response
    { responseId = "resp_1"
    , createdAt = 1
    , error = Nothing
    , incompleteDetails = Nothing
    , instructions = Nothing
    , metadata = Nothing
    , model = "test-model"
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

toolCall :: ResponseItem
toolCall =
    FunctionCallItem FunctionCall
        { itemId = Just "fc_1"
        , callId = "call_1"
        , name = "echo"
        , namespace = Nothing
        , arguments = ""
        , encryptedFunctionArgs = Nothing
        , status = Nothing
        , extraFields = emptyExtensions
        }

customCall :: ResponseItem
customCall =
    CustomToolCallItem CustomToolCall
        { itemId = Just "custom_1"
        , callId = "custom-call"
        , name = "apply_patch"
        , namespace = Nothing
        , input = ""
        , status = Nothing
        , extraFields = emptyExtensions
        }

reasoningItem :: ResponseItem
reasoningItem =
    ReasoningItemValue ReasoningItem
        { itemId = Just "reason_1"
        , summary = []
        , content = Nothing
        , encryptedContent = Nothing
        , status = Nothing
        , extraFields = emptyExtensions
        }

renameCall :: Text -> ResponseItem -> ResponseItem
renameCall value (FunctionCallItem call) =
    FunctionCallItem call { name = value }
renameCall _ item = item

expectRight :: (Show error) => Either error value -> IO value
expectRight = \case
    Right value -> pure value
    Left err -> do
        expectationFailure ("expected Right, got " <> show err)
        error "expectRight: unreachable"

data CallFragment = CallFragment
    { fragmentItemId :: !(Maybe Text)
    , fragmentCallId :: !Text
    , fragmentName :: !Text
    , fragmentArguments :: !Text
    }
    deriving (Eq, Show)

instance Arbitrary CallFragment where
    arbitrary = do
        identifier <- chooseInt (0, 20)
        includeItemId <- elements [False, True]
        marker <- chooseInt (-1000, 1000)
        pure CallFragment
            { fragmentItemId =
                if includeItemId
                    then Just ("item-" <> identifierText identifier)
                    else Nothing
            , fragmentCallId = "call-" <> identifierText identifier
            , fragmentName = "name-" <> identifierText marker
            , fragmentArguments = "{\"marker\":"
                <> identifierText marker <> "}"
            }
    shrink _ = []

fragmentItem :: CallFragment -> ResponseItem
fragmentItem fragment =
    FunctionCallItem FunctionCall
        { itemId = fragment.fragmentItemId
        , callId = fragment.fragmentCallId
        , name = fragment.fragmentName
        , namespace = Nothing
        , arguments = fragment.fragmentArguments
        , encryptedFunctionArgs = Nothing
        , status = Nothing
        , extraFields = emptyExtensions
        }

data IndexedOperation = IndexedOperation
    { operationIndex :: !(Maybe Int)
    , operationDone :: !Bool
    , operationFragment :: !CallFragment
    }
    deriving (Eq, Show)

instance Arbitrary IndexedOperation where
    arbitrary = IndexedOperation
        <$> elements [Nothing, Just 0, Just 1, Just 2, Just 4]
        <*> elements [False, True]
        <*> arbitrary
    shrink _ = []

newtype AdversarialOperations =
    AdversarialOperations [IndexedOperation]
    deriving (Show)

instance Arbitrary AdversarialOperations where
    arbitrary = AdversarialOperations <$> listOf arbitrary
    shrink _ = []

operationEvent :: IndexedOperation -> ResponseStreamEvent
operationEvent operation =
    let item = fragmentItem operation.operationFragment
    in if operation.operationDone
        then ResponseOutputItemDoneEvent
            item operation.operationIndex Nothing emptyExtensions
        else ResponseOutputItemAddedEvent
            item operation.operationIndex Nothing emptyExtensions

type StreamModel = Map.Map Int (ResponseItem, Bool)

applyExpected :: StreamModel -> IndexedOperation -> StreamModel
applyExpected model operation =
    Map.alter update targetIndex model
  where
    newItem = fragmentItem operation.operationFragment
    targetIndex = fromMaybe (nextModelIndex model) $
        operation.operationIndex
            <|> findMatchingIndex newItem model
            <|> if operation.operationDone
                then fst <$> Map.lookupMin (Map.filter (not . snd) model)
                else Nothing
    update Nothing = Just (newItem, operation.operationDone)
    update (Just (_, wasDone)) =
        Just (newItem, wasDone || operation.operationDone)

findMatchingIndex :: ResponseItem -> StreamModel -> Maybe Int
findMatchingIndex (FunctionCallItem wanted) model =
    firstMatching (.itemId) wanted.itemId
        <|> firstMatching (Just . (.callId)) (Just wanted.callId)
  where
    firstMatching project expected =
        fst <$> Map.lookupMin
            (Map.filter
                (\case
                    (FunctionCallItem item, _) ->
                        expected /= Nothing && project item == expected
                    _ -> False)
                model)
findMatchingIndex _ _ = Nothing

nextModelIndex :: StreamModel -> Int
nextModelIndex model =
    maybe 0 ((+ 1) . fst) (Map.lookupMax model)

newtype LifecycleTrace = LifecycleTrace ([Int], Int, [Int])
    deriving (Show)

instance Arbitrary LifecycleTrace where
    arbitrary = LifecycleTrace
        <$> ((,,) <$> listOf arbitrary <*> arbitrary <*> listOf arbitrary)
    shrink _ = []

responseWithId :: Int -> Response
responseWithId identifier =
    (response []) { responseId = identifierText identifier }

identifierText :: Int -> Text
identifierText = Text.pack . show
