module Agent.Responses.StreamAssemblySpec (spec) where

import Agent.Error (ApiError(..))
import Agent.Responses.SSE (parseSseEvents)
import qualified Agent.Responses.Codec as Codec
import Agent.Responses.StreamAssembly
import Agent.Responses.Types
import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
    ( Arbitrary(..)
    , chooseInt
    , conjoin
    , counterexample
    , elements
    , listOf
    , listOf1
    , oneof
    , (===)
    )

spec :: Spec
spec = describe "buildStreamResponse" do
    it "merges output_item.done events into the terminal response" do
        events <- expectRight $ parseSseEvents $ Text.intercalate ""
            [ sseBlock "response.output_item.done"
                "{\"type\":\"response.output_item.done\",\"output_index\":0,\"item\":{\"type\":\"function_call\",\"call_id\":\"call-1\",\"name\":\"echo\",\"arguments\":\"{}\"}}"
            , sseBlock "response.completed"
                "{\"type\":\"response.completed\",\"response\":{\"id\":\"resp-1\",\"created_at\":0,\"model\":\"test\",\"status\":\"completed\",\"output\":[]}}"
            ]
        response <- expectRight (buildStreamResponse config events)
        [name | FunctionCallItem FunctionCall { name } <- response.output]
            `shouldBe` ["echo"]

    it "assembles a partial response.done after a function call" do
        events <- expectRight $ parseSseEvents $ Text.intercalate ""
            [ sseBlock "response.created"
                "{\"type\":\"response.created\",\"response\":{\"id\":\"resp-done\"}}"
            , sseBlock "response.output_item.done"
                "{\"type\":\"response.output_item.done\",\"output_index\":0,\"item\":{\"type\":\"function_call\",\"call_id\":\"call-done\",\"name\":\"shell_command\",\"arguments\":\"{\\\"command\\\":\\\"echo done\\\"}\"}}"
            , sseBlock "response.done"
                "{\"type\":\"response.done\",\"response\":{\"usage\":{\"input_tokens\":10,\"output_tokens\":2,\"total_tokens\":12}}}"
            ]
        response <- expectRight
            (buildStreamResponseWithModel config (Just "request-model") events)
        response.responseId `shouldBe` "resp-done"
        response.model `shouldBe` "request-model"
        response.status `shouldBe` ResponseCompleted
        fmap (.totalTokens) response.usage `shouldBe` Just 12
        [name | FunctionCallItem FunctionCall { name } <- response.output]
            `shouldBe` ["shell_command"]

    it "preserves a cancelled status carried by response.done" do
        events <- expectRight $ parseSseEvents $ Text.intercalate ""
            [ sseBlock "response.created"
                "{\"type\":\"response.created\",\"response\":{\"id\":\"resp-cancelled\"}}"
            , sseBlock "response.done"
                "{\"type\":\"response.done\",\"response\":{\"status\":\"cancelled\"}}"
            ]
        response <- expectRight
            (buildStreamResponseWithModel config (Just "request-model") events)
        response.status `shouldBe` ResponseCancelled

    it "assembles minimal created and completed lifecycle fragments" do
        events <- expectRight $ parseSseEvents $ Text.intercalate ""
            [ sseBlock "response.created"
                "{\"type\":\"response.created\",\"response\":{\"id\":\"resp-partial\"}}"
            , sseBlock "response.completed"
                "{\"type\":\"response.completed\",\"response\":{\"id\":\"resp-partial\",\"usage\":{\"input_tokens\":7,\"output_tokens\":3,\"total_tokens\":10}}}"
            ]
        response <- expectRight
            (buildStreamResponseWithModel config (Just "request-model") events)
        response.responseId `shouldBe` "resp-partial"
        response.createdAt `shouldBe` 0
        response.model `shouldBe` "request-model"
        response.object `shouldBe` "response"
        response.status `shouldBe` ResponseCompleted
        response.output `shouldBe` []
        fmap (.totalTokens) response.usage `shouldBe` Just 10

    it "classifies an incomplete lifecycle fragment as a failure" do
        events <- expectRight $ parseSseEvents $ Text.intercalate ""
            [ sseBlock "response.created"
                "{\"type\":\"response.created\",\"response\":{\"id\":\"resp-incomplete\"}}"
            , sseBlock "response.incomplete"
                "{\"type\":\"response.incomplete\",\"response\":{\"incomplete_details\":{\"reason\":\"max_output_tokens\"}}}"
            ]
        buildStreamResponseWithModel config (Just "request-model") events
            `shouldBe`
                Left (ConnectionError
                    "failed: response.incomplete: max_output_tokens")

    it "recovers a dropped stream after response.created as incomplete" do
        -- A real response.created frame carries status "in_progress". When the
        -- socket dies mid-stream the recovery path must force an "incomplete"
        -- status rather than leaking the stale "in_progress" through, which
        -- would otherwise be classified as a completed turn.
        let created = ResponseCreatedEvent
                (decodeResponseValue (Aeson.object
                    [ "id" Aeson..= ("resp-dropped" :: Text)
                    , "created_at" Aeson..= (0 :: Int)
                    , "model" Aeson..= ("test" :: Text)
                    , "status" Aeson..= ("in_progress" :: Text)
                    ]))
                Nothing
            state = applyStreamEvent emptyStreamAssemblyState created
        response <- expectRight (finishAssembledIncomplete (Just "test") state)
        response.status `shouldBe` ResponseIncomplete
        response.responseId `shouldBe` "resp-dropped"

    it "replaces indexed added items with done items without duplicates" do
        events <- expectRight $ parseSseEvents $ Text.intercalate ""
            [ sseBlock "response.created"
                "{\"type\":\"response.created\",\"response\":{\"id\":\"resp-indexed\"}}"
            , sseBlock "response.output_item.added"
                "{\"type\":\"response.output_item.added\",\"output_index\":1,\"item\":{\"type\":\"function_call\",\"call_id\":\"call-2\",\"name\":\"stale-second\",\"arguments\":\"\"}}"
            , sseBlock "response.output_item.added"
                "{\"type\":\"response.output_item.added\",\"output_index\":0,\"item\":{\"type\":\"function_call\",\"call_id\":\"call-1\",\"name\":\"first\",\"arguments\":\"{}\"}}"
            , sseBlock "response.output_item.done"
                "{\"type\":\"response.output_item.done\",\"output_index\":1,\"item\":{\"type\":\"function_call\",\"call_id\":\"call-2\",\"name\":\"second\",\"arguments\":\"{}\"}}"
            , sseBlock "response.completed"
                "{\"type\":\"response.completed\",\"response\":{\"id\":\"resp-indexed\"}}"
            ]
        response <- expectRight
            (buildStreamResponseWithModel config (Just "request-model") events)
        [name | FunctionCallItem FunctionCall { name } <- response.output]
            `shouldBe` ["first", "second"]
        length response.output `shouldBe` 2

    it "assembles indexless done-only output items in wire order" do
        events <- expectRight $ parseSseEvents $ Text.intercalate ""
            [ sseBlock "response.created"
                "{\"type\":\"response.created\",\"response\":{\"id\":\"resp-indexless\"}}"
            , sseBlock "response.output_item.done"
                "{\"type\":\"response.output_item.done\",\"item\":{\"type\":\"function_call\",\"call_id\":\"call-1\",\"name\":\"first\",\"arguments\":\"{}\"}}"
            , sseBlock "response.output_item.done"
                "{\"type\":\"response.output_item.done\",\"item\":{\"type\":\"function_call\",\"call_id\":\"call-2\",\"name\":\"second\",\"arguments\":\"{}\"}}"
            , sseBlock "response.done"
                "{\"type\":\"response.done\",\"response\":{}}"
            ]
        response <- expectRight
            (buildStreamResponseWithModel config (Just "request-model") events)
        [name | FunctionCallItem FunctionCall { name } <- response.output]
            `shouldBe` ["first", "second"]

    it "assembles custom-tool input events without output_index" do
        events <- expectRight $ parseSseEvents $ Text.intercalate ""
            [ sseBlock "response.created"
                "{\"type\":\"response.created\",\"response\":{\"id\":\"resp-custom\"}}"
            , sseBlock "response.output_item.added"
                "{\"type\":\"response.output_item.added\",\"output_index\":0,\"item\":{\"type\":\"custom_tool_call\",\"id\":\"ctc-1\",\"call_id\":\"call-1\",\"name\":\"apply_patch\",\"input\":\"\"}}"
            , sseBlock "response.custom_tool_call_input.delta"
                "{\"type\":\"response.custom_tool_call_input.delta\",\"item_id\":\"not-the-item\",\"call_id\":\"call-1\",\"delta\":\"*** Begin\"}"
            , sseBlock "response.custom_tool_call_input.delta"
                "{\"type\":\"response.custom_tool_call_input.delta\",\"call_id\":\"call-1\",\"delta\":\" Patch\"}"
            , sseBlock "response.custom_tool_call_input.done"
                "{\"type\":\"response.custom_tool_call_input.done\",\"item_id\":\"ctc-1\",\"input\":\"*** Begin Patch\"}"
            , sseBlock "response.completed"
                "{\"type\":\"response.completed\",\"response\":{\"id\":\"resp-custom\"}}"
            ]
        response <- expectRight
            (buildStreamResponseWithModel config (Just "request-model") events)
        case response.output of
            [CustomToolCallItem CustomToolCall { name, input }] ->
                (name, input)
                    `shouldBe` ("apply_patch", "*** Begin Patch")
            other -> expectationFailure
                ("expected one custom tool call, got " <> show other)

    it "assembles function-call arguments without output_item.done" do
        events <- expectRight $ parseSseEvents $ Text.intercalate ""
            [ sseBlock "response.created"
                "{\"type\":\"response.created\",\"response\":{\"id\":\"resp-args\"}}"
            , sseBlock "response.output_item.added"
                "{\"type\":\"response.output_item.added\",\"output_index\":0,\"item\":{\"type\":\"function_call\",\"id\":\"fc-1\",\"call_id\":\"call-1\",\"name\":\"read_file\",\"arguments\":\"\"}}"
            , sseBlock "response.function_call_arguments.delta"
                "{\"type\":\"response.function_call_arguments.delta\",\"item_id\":\"fc-1\",\"output_index\":0,\"delta\":\"{\\\"target_file\\\":\\\"\"}"
            , sseBlock "response.function_call_arguments.delta"
                "{\"type\":\"response.function_call_arguments.delta\",\"item_id\":\"fc-1\",\"delta\":\"README.md\\\"}\"}"
            , sseBlock "response.function_call_arguments.done"
                "{\"type\":\"response.function_call_arguments.done\",\"item_id\":\"fc-1\",\"name\":\"read_file\",\"arguments\":\"{\\\"target_file\\\":\\\"README.md\\\"}\"}"
            , sseBlock "response.completed"
                "{\"type\":\"response.completed\",\"response\":{\"id\":\"resp-args\",\"output\":[]}}"
            ]
        response <- expectRight
            (buildStreamResponseWithModel config (Just "request-model") events)
        case response.output of
            [FunctionCallItem FunctionCall { name, arguments }] ->
                (name, arguments)
                    `shouldBe` ("read_file", "{\"target_file\":\"README.md\"}")
            other -> expectationFailure
                ("expected one function call, got " <> show other)

    it "assembles a function call after a reasoning item from argument events" do
        events <- expectRight $ parseSseEvents $ Text.intercalate ""
            [ sseBlock "response.created"
                "{\"type\":\"response.created\",\"response\":{\"id\":\"resp-reason-then-call\"}}"
            , sseBlock "response.output_item.added"
                "{\"type\":\"response.output_item.added\",\"output_index\":0,\"item\":{\"type\":\"reasoning\",\"id\":\"rs-1\",\"summary\":[]}}"
            , sseBlock "response.output_item.done"
                "{\"type\":\"response.output_item.done\",\"output_index\":0,\"item\":{\"type\":\"reasoning\",\"id\":\"rs-1\",\"summary\":[]}}"
            , sseBlock "response.output_item.added"
                "{\"type\":\"response.output_item.added\",\"output_index\":1,\"item\":{\"type\":\"function_call\",\"id\":\"fc-1\",\"call_id\":\"call-1\",\"name\":\"echo\",\"arguments\":\"\"}}"
            , sseBlock "response.function_call_arguments.done"
                "{\"type\":\"response.function_call_arguments.done\",\"item_id\":\"fc-1\",\"output_index\":1,\"name\":\"echo\",\"arguments\":\"{\\\"message\\\":\\\"hi\\\"}\"}"
            , sseBlock "response.completed"
                "{\"type\":\"response.completed\",\"response\":{\"id\":\"resp-reason-then-call\",\"output\":[]}}"
            ]
        response <- expectRight
            (buildStreamResponseWithModel config (Just "request-model") events)
        [name | FunctionCallItem FunctionCall { name } <- response.output]
            `shouldBe` ["echo"]
        [arguments | FunctionCallItem FunctionCall { arguments } <- response.output]
            `shouldBe` ["{\"message\":\"hi\"}"]

    it "assembles reasoning summary part and text events by item id" do
        events <- expectRight $ parseSseEvents $ Text.intercalate ""
            [ sseBlock "response.created"
                "{\"type\":\"response.created\",\"response\":{\"id\":\"resp-reasoning\"}}"
            , sseBlock "response.output_item.added"
                "{\"type\":\"response.output_item.added\",\"output_index\":0,\"item\":{\"type\":\"reasoning\",\"id\":\"rs-1\",\"summary\":[]}}"
            , sseBlock "response.reasoning_summary_part.added"
                "{\"type\":\"response.reasoning_summary_part.added\",\"item_id\":\"rs-1\",\"summary_index\":0,\"part\":{\"type\":\"summary_text\",\"text\":\"\"}}"
            , sseBlock "response.reasoning_summary_text.done"
                "{\"type\":\"response.reasoning_summary_text.done\",\"item_id\":\"rs-1\",\"summary_index\":0,\"text\":\"Checked the repository.\"}"
            , sseBlock "response.completed"
                "{\"type\":\"response.completed\",\"response\":{\"id\":\"resp-reasoning\"}}"
            ]
        response <- expectRight
            (buildStreamResponseWithModel config (Just "request-model") events)
        case response.output of
            [ReasoningItemValue ReasoningItem
                { summary = [ReasoningSummaryPart { text = Just partText }]
                }] ->
                    partText `shouldBe` "Checked the repository."
            other -> expectationFailure
                ("expected one reasoning summary, got " <> show other)

    it "assembles indexless reasoning summary text deltas by item id" do
        events <- expectRight $ parseSseEvents $ Text.intercalate ""
            [ sseBlock "response.created"
                "{\"type\":\"response.created\",\"response\":{\"id\":\"resp-reasoning-delta\"}}"
            , sseBlock "response.output_item.added"
                "{\"type\":\"response.output_item.added\",\"item\":{\"type\":\"reasoning\",\"id\":\"rs-1\",\"summary\":[]}}"
            , sseBlock "response.reasoning_summary_part.added"
                "{\"type\":\"response.reasoning_summary_part.added\",\"item_id\":\"rs-1\",\"summary_index\":0,\"part\":{\"type\":\"summary_text\",\"text\":\"\"}}"
            , sseBlock "response.reasoning_summary_text.delta"
                "{\"type\":\"response.reasoning_summary_text.delta\",\"item_id\":\"rs-1\",\"summary_index\":0,\"delta\":\"Checked \"}"
            , sseBlock "response.reasoning_summary_text.delta"
                "{\"type\":\"response.reasoning_summary_text.delta\",\"item_id\":\"rs-1\",\"summary_index\":0,\"delta\":\"the repository.\"}"
            , sseBlock "response.completed"
                "{\"type\":\"response.completed\",\"response\":{\"id\":\"resp-reasoning-delta\"}}"
            ]
        response <- expectRight
            (buildStreamResponseWithModel config (Just "request-model") events)
        case response.output of
            [ReasoningItemValue ReasoningItem
                { summary = [ReasoningSummaryPart { text = Just partText }]
                }] ->
                    partText `shouldBe` "Checked the repository."
            other -> expectationFailure
                ("expected one reasoning summary, got " <> show other)

    it "uses provider classifiers when no terminal response is present" do
        streamEvents <- expectRight $ parseSseEvents $ sseBlock "error"
            "{\"type\":\"error\",\"error\":{\"message\":\"stream broke\"}}"
        buildStreamResponse config streamEvents
            `shouldBe` Left (ConnectionError "stream: stream broke")

        failedEvents <- expectRight $ parseSseEvents $ sseBlock "response.failed"
            "{\"type\":\"response.failed\",\"response\":{\"id\":\"resp-f\",\"created_at\":0,\"model\":\"test\",\"status\":\"failed\",\"incomplete_details\":{\"reason\":\"overloaded\"}}}"
        buildStreamResponse config failedEvents
            `shouldBe` Left (ConnectionError "failed: response.failed: overloaded")

        emptyFailedEvents <- expectRight $ parseSseEvents $
            sseBlock "response.failed"
                "{\"type\":\"response.failed\",\"response\":{}}"
        buildStreamResponse config emptyFailedEvents
            `shouldBe` Left
                (ConnectionError "failed: response.failed (no details)")

        messageFailedEvents <- expectRight $ parseSseEvents $
            sseBlock "response.failed"
                "{\"type\":\"response.failed\",\"response\":{\"error\":{\"message\":\"exploded\"}}}"
        buildStreamResponse config messageFailedEvents
            `shouldBe` Left (ConnectionError "failed: exploded")

    it "accepts code-only stream errors" do
        streamEvents <- expectRight $ parseSseEvents $ sseBlock "error"
            "{\"type\":\"error\",\"code\":\"rate_limit\"}"
        case streamEvents of
            [ResponseErrorEvent { streamError }] -> do
                streamError.code `shouldBe` Just "rate_limit"
                streamError.message `shouldBe` ""
            other -> expectationFailure
                ("expected one stream error event, got " <> show other)

    it "uses the configured missing-completion message" do
        case buildStreamResponse config [] of
            Left (JsonDecodeError message _) ->
                message `shouldBe` "custom missing completion"
            other -> expectationFailure
                ("expected missing-completion JsonDecodeError, got " <> show other)

    modifyMaxSuccess (const 500) $
        prop "matches an independent model for adversarial indexed events" $
            \(AdversarialOperations operations) ->
                let events =
                        [ ResponseCreatedEvent
                            (responseFragment "adversarial")
                            Nothing
                        ]
                        <> map operationEvent operations
                        <> [ ResponseCompletedEvent
                                (responseFragment "adversarial")
                                Nothing
                           ]
                    expectedModel =
                        foldl applyExpected Map.empty operations
                    expected =
                        expectedOutput expectedModel
                in case buildStreamResponse config events of
                    Left err ->
                        counterexample
                            ( "unexpected assembly failure: "
                                <> show err
                                <> "\noperations: "
                                <> show operations
                            )
                            False
                    Right response ->
                        let actual = map Aeson.toJSON response.output
                        in counterexample
                            ( "operations: " <> show operations
                                <> "\nexpected: " <> show expected
                                <> "\nactual: " <> show actual
                            )
                            (actual === expected)

    modifyMaxSuccess (const 300) $
        prop "keeps done precedence when a late added event is merged" $
            \doneFragment lateFragment terminalFragment ->
                let doneOperation = IndexedOperation (Just 0) True doneFragment
                    lateOperation = IndexedOperation (Just 0) False lateFragment
                    terminalValue =
                        Aeson.toJSON (toResponseItem terminalFragment)
                    streamedValue =
                        mergeModelValues
                            (Aeson.toJSON (toResponseItem doneFragment))
                            (Aeson.toJSON (toResponseItem lateFragment))
                    expected =
                        [mergeModelValues terminalValue streamedValue]
                    events =
                        [ ResponseCreatedEvent
                            (responseFragment "sticky-done")
                            Nothing
                        , operationEvent doneOperation
                        , operationEvent lateOperation
                        , ResponseCompletedEvent
                            (responseFragmentWithOutput
                                "sticky-done"
                                [toResponseItem terminalFragment])
                            Nothing
                        ]
                in case buildStreamResponse config events of
                    Left err ->
                        counterexample
                            ("unexpected assembly failure: " <> show err)
                            False
                    Right response ->
                        map Aeson.toJSON response.output === expected

    modifyMaxSuccess (const 300) $
        prop "ignores all events after the first terminal lifecycle event" $
            \(LifecycleTrace before terminal after) ->
                let events =
                        map lifecycleEvent before
                        <> [lifecycleEvent terminal]
                        <> map lifecycleEvent after
                    expectedId = terminal.lifecycleId
                in case buildStreamResponse config events of
                    Left err ->
                        counterexample
                            ("unexpected assembly failure: " <> show err)
                            False
                    Right response ->
                        conjoin
                            [ counterexample
                                ("terminal: " <> show terminal
                                    <> "\nafter: " <> show after)
                                (response.responseId === expectedId)
                            ]
  where
    config = StreamAssemblyConfig
        { missingCompletionMessage = "custom missing completion"
        , classifyStreamError =
            \streamError -> ConnectionError ("stream: " <> streamError.message)
        , classifyFailedResponse =
            \failure ->
                ConnectionError
                    ("failed: " <> failedStreamResponseMessage failure)
        , incompleteAsFailure = True
        }

responseFragment :: Text -> Response
responseFragment responseId =
    decodeResponseValue $ Aeson.object
        [ "id" Aeson..= responseId
        , "created_at" Aeson..= (0 :: Int)
        , "model" Aeson..= ("generated-model" :: Text)
        , "status" Aeson..= ("completed" :: Text)
        , "output" Aeson..= ([] :: [Aeson.Value])
        ]

responseFragmentWithOutput :: Text -> [ResponseItem] -> Response
responseFragmentWithOutput responseId output =
    decodeResponseValue $ Aeson.object
        [ "id" Aeson..= responseId
        , "created_at" Aeson..= (0 :: Int)
        , "model" Aeson..= ("generated-model" :: Text)
        , "status" Aeson..= ("completed" :: Text)
        , "output" Aeson..= output
        ]

decodeResponseValue :: Aeson.Value -> Response
decodeResponseValue value =
    either error id
        (Codec.decodeResponse (LBS.toStrict (Aeson.encode value)))

-- This model deliberately stores raw item values rather than reusing the
-- implementation's progress type. An explicit output index identifies one
-- item, and later object fields overlay earlier fields.
type StreamModel = Map.Map Int (Aeson.Value, Bool)

data CallFragment = CallFragment
    { fragmentItemId    :: !(Maybe Text)
    , fragmentCallId    :: !Text
    , fragmentName      :: !Text
    , fragmentNamespace :: !(Maybe Text)
    , fragmentArguments  :: !Text
    , fragmentStatus     :: !(Maybe ItemStatus)
    , fragmentMarker     :: !Text
    }
    deriving (Eq, Show)

data LifecycleStep = LifecycleStep
    { lifecycleId       :: !Text
    , lifecycleTerminal :: !Bool
    }
    deriving (Eq, Show)

instance Arbitrary LifecycleStep where
    arbitrary = LifecycleStep
        <$> elements ["response-before-a", "response-before-b", "response-final"]
        <*> arbitrary

data LifecycleTrace = LifecycleTrace
    { lifecycleBefore   :: ![LifecycleStep]
    , lifecycleTerminalStep :: !LifecycleStep
    , lifecycleAfter    :: ![LifecycleStep]
    }
    deriving (Eq, Show)

instance Arbitrary LifecycleTrace where
    arbitrary = do
        beforeIds <- listOf
            (elements ["response-before-a", "response-before-b"])
        terminal <- LifecycleStep
            <$> elements ["response-final", "response-final-b"]
            <*> pure True
        after <- listOf arbitrary
        pure (LifecycleTrace
            (map (`LifecycleStep` False) beforeIds)
            terminal
            after)
    shrink (LifecycleTrace before terminal after) =
        [ LifecycleTrace before' terminal after
        | before' <- shrink before
        ]
        <> [ LifecycleTrace before terminal after'
           | after' <- shrink after
           ]

lifecycleEvent :: LifecycleStep -> ResponseStreamEvent
lifecycleEvent step
    | step.lifecycleTerminal =
        ResponseCompletedEvent
            (responseFragment step.lifecycleId)
            Nothing
    | otherwise =
        ResponseCreatedEvent
            (responseFragment step.lifecycleId)
            Nothing

instance Arbitrary CallFragment where
    arbitrary = CallFragment
        <$> arbitraryOptional
        <*> elements ["call-a", "call-b", "call-c", "call-latest"]
        <*> elements ["first", "second", "third", "latest"]
        <*> arbitraryOptional
        <*> elements ["{}", "{\"value\":1}", "{\"value\":2}", ""]
        <*> arbitraryOptionalStatus
        <*> elements ["marker-a", "marker-b", "marker-c", "marker-latest"]
      where
        arbitraryOptional =
            elements
                [ Nothing
                , Just "item-a"
                , Just "item-b"
                , Just "item-c"
                ]
        arbitraryOptionalStatus =
            elements
                [ Nothing
                , Just ItemInProgress
                , Just ItemCompleted
                , Just ItemIncomplete
                ]

data IndexedOperation = IndexedOperation
    { operationIndex :: !(Maybe Int)
    , operationDone  :: !Bool
    , operationCall  :: !CallFragment
    }
    deriving (Eq, Show)

instance Arbitrary IndexedOperation where
    arbitrary = IndexedOperation
        <$> oneof
            [ pure Nothing
            , Just <$> chooseInt (-3, 100)
            ]
        <*> arbitrary
        <*> arbitrary

newtype AdversarialOperations = AdversarialOperations [IndexedOperation]
    deriving (Eq, Show)

instance Arbitrary AdversarialOperations where
    arbitrary = do
        -- Keep these operations in every generated stream so that each
        -- property run exercises duplicates, both orderings, and sparse,
        -- out-of-order indexes. Additional operations remain arbitrary.
        doneBeforeAddedIndex <- chooseInt (30, 40)
        addedBeforeDoneIndex <- chooseInt (60, 70)
        lateAddedIndex <- chooseInt (0, 10)
        forced <- traverse
            (\(index, done) ->
                IndexedOperation index done <$> arbitrary)
            [ (Just doneBeforeAddedIndex, True)
            , (Just doneBeforeAddedIndex, False)
            , (Just addedBeforeDoneIndex, False)
            , (Just addedBeforeDoneIndex, True)
            , (Just lateAddedIndex, True)
            , (Just lateAddedIndex, False)
            ]
        noise <- listOf1
            (IndexedOperation
                <$> (oneof
                    [ pure Nothing
                    , Just <$> chooseInt (0, 100)
                    ])
                <*> arbitrary
                <*> arbitrary)
        let shared = CallFragment
                { fragmentItemId = Just "shared-item"
                , fragmentCallId = "shared-call"
                , fragmentName = "shared"
                , fragmentNamespace = Nothing
                , fragmentArguments = "{}"
                , fragmentStatus = Just ItemInProgress
                , fragmentMarker = "shared"
                }
            conflicting = shared
                { fragmentCallId = "other-call"
                , fragmentName = "conflicting"
                , fragmentMarker = "conflicting"
                }
            identityOperations =
                [ IndexedOperation Nothing True shared
                , IndexedOperation Nothing False conflicting
                ]
        pure (AdversarialOperations
            (forced <> identityOperations <> noise))
    shrink (AdversarialOperations operations) =
        AdversarialOperations <$> shrink operations

operationEvent :: IndexedOperation -> ResponseStreamEvent
operationEvent operation
    | operation.operationDone =
        ResponseOutputItemDoneEvent
            (toResponseItem operation.operationCall)
            operation.operationIndex
            Nothing
    | otherwise =
        ResponseOutputItemAddedEvent
            (toResponseItem operation.operationCall)
            operation.operationIndex
            Nothing

toResponseItem :: CallFragment -> ResponseItem
toResponseItem fragment =
    FunctionCallItem FunctionCall
        { itemId = fragment.fragmentItemId
        , callId = fragment.fragmentCallId
        , name = fragment.fragmentName
        , namespace = fragment.fragmentNamespace
        , provider = Nothing
        , arguments = fragment.fragmentArguments
        , encryptedFunctionArgs = Nothing
        , status = fragment.fragmentStatus

        }

applyExpected :: StreamModel -> IndexedOperation -> StreamModel
applyExpected expected operation =
    Map.alter update outputIndex expected
  where
    newValue = Aeson.toJSON (toResponseItem operation.operationCall)
    outputIndex =
        fromMaybe (nextModelIndex expected) $
            operation.operationIndex
                <|> findModelItemIndex newValue expected
                <|> if operation.operationDone
                    then findPendingModelIndex newValue expected
                    else Nothing
    update Nothing = Just (newValue, operation.operationDone)
    update (Just (oldValue, wasDone)) =
        Just
            ( mergeModelValues oldValue newValue
            , wasDone || operation.operationDone
            )

mergeModelValues :: Aeson.Value -> Aeson.Value -> Aeson.Value
mergeModelValues (Aeson.Object oldObject) (Aeson.Object newObject) =
    Aeson.Object (KeyMap.union newObject oldObject)
mergeModelValues _ newValue = newValue

expectedOutput :: StreamModel -> [Aeson.Value]
expectedOutput = map fst . Map.elems

nextModelIndex :: StreamModel -> Int
nextModelIndex model =
    maybe 0 ((+ 1) . fst) (Map.lookupMax model)

findModelItemIndex :: Aeson.Value -> StreamModel -> Maybe Int
findModelItemIndex value model =
    firstJustModel
        [ findIdentityModel identity model
        | identity <- itemIdentitiesModel value
        ]

findIdentityModel :: Text -> StreamModel -> Maybe Int
findIdentityModel wanted model =
    fst <$> Map.lookupMin
        (Map.filter
            (matchesIdentityValue wanted . fst)
            model)

matchesIdentityValue :: Text -> Aeson.Value -> Bool
matchesIdentityValue wanted candidate =
    objectTextFieldModel "id" candidate == Just wanted
        || objectTextFieldModel "call_id" candidate == Just wanted

itemIdentitiesModel :: Aeson.Value -> [Text]
itemIdentitiesModel value =
    [ identity
    | fieldName <- ["id", "call_id"]
    , Just identity <- [objectTextFieldModel fieldName value]
    ]

findPendingModelIndex :: Aeson.Value -> StreamModel -> Maybe Int
findPendingModelIndex value model =
    case objectTextFieldModel "type" value of
        Nothing -> Nothing
        Just wantedType ->
            fst <$> Map.lookupMin
                (Map.filter
                    (\(item, done) ->
                        not done
                            && objectTextFieldModel "type" item
                                == Just wantedType)
                    model)

firstJustModel :: [Maybe value] -> Maybe value
firstJustModel = foldr (<|>) Nothing

objectTextFieldModel :: Text -> Aeson.Value -> Maybe Text
objectTextFieldModel fieldName value =
    case value of
        Aeson.Object object ->
            case KeyMap.lookup (Key.fromText fieldName) object of
                Just (Aeson.String text) -> Just text
                _ -> Nothing
        _ -> Nothing

sseBlock :: Text -> Text -> Text
sseBlock eventType dataText =
    "event: " <> eventType <> "\ndata: " <> dataText <> "\n\n"

expectRight :: Show error => Either error value -> IO value
expectRight = \case
    Left err ->
        expectationFailure ("expected Right, got Left " <> show err)
            >> fail "unreachable"
    Right value -> pure value
