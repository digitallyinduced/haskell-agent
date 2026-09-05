-- | Response-item generators, round-trip properties, and wire fixtures.
module Agent.CLI.SessionSpec.ResponseItems
    ( storedResponseItemRoundTrip
    , storedContentPartRoundTrip
    , asyncPersistenceItems
    , rawJsonValue
    ) where

import Agent.CLI.Session.StoreCodec (fromStoredResponseItem, toStoredResponseItem)
import Agent.Json (RawJson, rawJsonFromEncoding)
import Agent.Responses.Types
import Agent.ToolOutcome (ToolOutcome(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as Text
import Test.QuickCheck
    ( Arbitrary(..)
    , Gen
    , Property
    , checkCoverage
    , chooseInt
    , counterexample
    , cover
    , elements
    , frequency
    , oneof
    , resize
    , sized
    , suchThatMap
    , vectorOf
    , (===)
    )

newtype StoredRoundTripItem = StoredRoundTripItem ResponseItem
    deriving (Show)

newtype StoredRoundTripContentPart =
    StoredRoundTripContentPart ResponseContentPart
    deriving (Show)

instance Arbitrary StoredRoundTripItem where
    arbitrary = StoredRoundTripItem <$> genResponseItem
    shrink _ = []

instance Arbitrary StoredRoundTripContentPart where
    arbitrary = StoredRoundTripContentPart <$> genContentPart
    shrink _ = []

storedResponseItemRoundTrip :: StoredRoundTripItem -> Property
storedResponseItemRoundTrip (StoredRoundTripItem item) =
    checkCoverage $
        foldr
            (\label -> cover 5 (responseItemKind item == label) label)
            (counterexample ("failed to round-trip " <> show item) $
            fromStoredResponseItem (toStoredResponseItem item)
                === Right item)
            responseItemKinds

storedContentPartRoundTrip :: StoredRoundTripContentPart -> Property
storedContentPartRoundTrip (StoredRoundTripContentPart part) =
    checkCoverage $
        foldr
            (\label -> cover 7 (contentPartKind part == label) label)
            (counterexample ("failed to round-trip " <> show part) $
            fromStoredResponseItem (toStoredResponseItem item)
                === Right item)
            contentPartKinds
  where
    item = MessageItem ResponseMessage
        { messageId = Just "generated-message"
        , content = MessageContentParts [part]
        , role = RoleAssistant
        , status = Just ItemCompleted
        , phase = Just "final"
        , passthrough = Nothing
        }

genResponseItem :: Gen ResponseItem
genResponseItem =
    oneof
        [ MessageItem <$> genResponseMessage
        , FunctionCallItem <$> genFunctionCall
        , FunctionCallOutputItem <$> genFunctionCallOutput
        , CustomToolCallItem <$> genCustomToolCall
        , CustomToolCallOutputItem <$> genCustomToolCallOutput
        , ComputerCallItem <$> genComputerCall
        , ComputerCallOutputItem <$> genComputerCallOutput
        , ReasoningItemValue <$> genReasoningItem
        , ItemReferenceValue <$> genItemReference
        , AgentMessageItem <$> genResponseAgentMessage
        , AdditionalToolsItemValue <$> genAdditionalToolsItem
        , CompactionTriggerItemValue <$> genCompactionTriggerItem
        , CompactionItemValue <$> genCompactionItem
        , do
            tagged <- genTaggedObject "known-item-"
            pure (KnownResponseItem (parseResponseItemType tagged.tag) tagged)
        , UnknownResponseItem <$> genTaggedObject "unknown-item-"
        ]

genResponseMessage :: Gen ResponseMessage
genResponseMessage =
    ResponseMessage
        <$> genMaybe genText
        <*> genMessageContent
        <*> genResponseRole
        <*> genMaybe genItemStatus
        <*> genMaybe genText
        <*> pure Nothing

genResponseAgentMessage :: Gen ResponseAgentMessage
genResponseAgentMessage =
    suchThatMap generate jsonRoundTrip
  where
    generate =
        ResponseAgentMessage
            <$> genMaybe genText
            <*> genMaybe genText
            <*> genMaybe genText
            <*> genSmallList genContentPart
            <*> pure Nothing

jsonRoundTrip :: a -> Maybe a
jsonRoundTrip = Just

genAdditionalToolsItem :: Gen AdditionalToolsItem
genAdditionalToolsItem =
    suchThatMap generate jsonRoundTrip
  where
    generate =
        AdditionalToolsItem
            <$> genMaybe genText
            <*> genText
            <*> genSmallList genRawJson

genCompactionTriggerItem :: Gen CompactionTriggerItem
genCompactionTriggerItem =
    pure CompactionTriggerItem

genCompactionItem :: Gen CompactionItem
genCompactionItem =
    suchThatMap generate jsonRoundTrip
  where
    generate =
        CompactionItem
            <$> genMaybe genText
            <*> genMaybe genText

genMessageContent :: Gen MessageContent
genMessageContent =
    frequency
        [ (2, MessageContentText <$> genText)
        , (3, MessageContentParts <$> genSmallList genContentPart)
        ]

genFunctionCall :: Gen FunctionCall
genFunctionCall =
    FunctionCall
        <$> genMaybe genText
        <*> genText
        <*> genText
        <*> genMaybe genText
        <*> genMaybe genText
        <*> genText
        <*> genMaybe (genSmallList genText)
        <*> genMaybe genItemStatus
        <*> genMaybe arbitrary

genFunctionCallOutput :: Gen FunctionCallOutput
genFunctionCallOutput =
    FunctionCallOutput
        <$> genMaybe genText
        <*> genText
        <*> genMaybe genText
        <*> genMaybe genText
        <*> genMaybe genText
        <*> genRawJson
        <*> genMaybe genItemStatus
        <*> genMaybe arbitrary
        <*> genMaybe genToolOutcome

genCustomToolCall :: Gen CustomToolCall
genCustomToolCall =
    CustomToolCall
        <$> genMaybe genText
        <*> genText
        <*> genText
        <*> genMaybe genText
        <*> genText
        <*> genMaybe genItemStatus
        <*> genMaybe arbitrary

genCustomToolCallOutput :: Gen CustomToolCallOutput
genCustomToolCallOutput =
    CustomToolCallOutput
        <$> genMaybe genText
        <*> genText
        <*> genMaybe genText
        <*> genRawJson
        <*> genMaybe genItemStatus
        <*> genMaybe arbitrary
        <*> genMaybe genToolOutcome

genToolOutcome :: Gen ToolOutcome
genToolOutcome =
    oneof
        [ elements
            [ ToolSucceeded
            , ToolFailed
            , ToolDenied
            , ToolCancelled
            , ShellCancelled
            , ShellTimedOut
            ]
        , ShellRunning <$> chooseInt (0, 1000)
        , ShellExited <$> chooseInt (-128, 255)
        ]

asyncPersistenceItems :: Maybe Bool -> [ResponseItem]
asyncPersistenceItems asyncValue =
    [ FunctionCallItem FunctionCall
        { itemId = Just "function-call"
        , callId = "function"
        , name = "shell"
        , namespace = Nothing
        , provider = Nothing
        , arguments = "{}"
        , encryptedFunctionArgs = Nothing
        , status = Just ItemCompleted
        , async = asyncValue
        }
    , FunctionCallOutputItem FunctionCallOutput
        { itemId = Just "function-output"
        , callId = "function"
        , name = Just "shell"
        , namespace = Nothing
        , provider = Nothing
        , output = rawJsonValue ("done" :: Text.Text)
        , status = Just ItemCompleted
        , async = asyncValue
        , localOutcome = Nothing
        }
    , CustomToolCallItem CustomToolCall
        { itemId = Just "custom-call"
        , callId = "custom"
        , name = "apply_patch"
        , namespace = Nothing
        , input = "*** Begin Patch"
        , status = Just ItemCompleted
        , async = asyncValue
        }
    , CustomToolCallOutputItem CustomToolCallOutput
        { itemId = Just "custom-output"
        , callId = "custom"
        , name = Just "apply_patch"
        , output = rawJsonValue ("done" :: Text.Text)
        , status = Just ItemCompleted
        , async = asyncValue
        , localOutcome = Nothing
        }
    ]

genComputerCall :: Gen ComputerCall
genComputerCall =
    ComputerCall
        <$> genMaybe genText
        <*> genText
        <*> genSmallList genComputerAction
        <*> genSmallList genSafetyCheck
        <*> genMaybe genItemStatus
        <*> fmap
            (withoutReservedKeys
                [ "type", "id", "call_id", "actions"
                , "pending_safety_checks", "status"
                ])
            genJsonObject

genComputerCallOutput :: Gen ComputerCallOutput
genComputerCallOutput =
    ComputerCallOutput
        <$> genMaybe genText
        <*> genText
        <*> (("data:image/png;base64," <>) <$> genText)
        <*> genSmallList genSafetyCheck
        <*> genMaybe genItemStatus
        <*> fmap
            (withoutReservedKeys
                [ "type", "id", "call_id", "output"
                , "acknowledged_safety_checks", "status"
                ])
            genJsonObject

genComputerAction :: Gen ComputerAction
genComputerAction =
    oneof
        [ pure ScreenshotAction
        , ClickAction <$> smallInt <*> smallInt <*> pure "left"
            <*> genSmallList genText
        , DoubleClickAction <$> smallInt <*> smallInt
            <*> genSmallList genText
        , TypeAction <$> genText
        , KeypressAction <$> genSmallList genText
        , ScrollAction <$> smallInt <*> smallInt <*> smallInt <*> smallInt
            <*> genSmallList genText
        , MoveAction <$> smallInt <*> smallInt <*> genSmallList genText
        , pure WaitAction
        , DragAction <$> genSmallList
            (ComputerPoint <$> smallInt <*> smallInt)
            <*> genSmallList genText
        ]
  where
    smallInt = chooseInt (0, 1000)

genSafetyCheck :: Gen SafetyCheck
genSafetyCheck =
    SafetyCheck
        <$> genText
        <*> genMaybe genText
        <*> genMaybe genText
        <*> fmap
            (withoutReservedKeys ["id", "code", "message"])
            genJsonObject

genReasoningItem :: Gen ReasoningItem
genReasoningItem =
    ReasoningItem
        <$> genMaybe genText
        <*> genSmallList genReasoningSummaryPart
        <*> genMaybe (genSmallList genContentPart)
        <*> genMaybe genText
        <*> genMaybe genItemStatus

genReasoningSummaryPart :: Gen ReasoningSummaryPart
genReasoningSummaryPart =
    ReasoningSummaryPart
        <$> genText
        <*> genMaybe genText

genItemReference :: Gen ItemReference
genItemReference =
    ItemReference
        <$> genText

genContentPart :: Gen ResponseContentPart
genContentPart =
    oneof
        [ InputTextPart
            <$> genText
            <*> genMaybe genNonNullRawJson
        , InputImagePart
            <$> genMaybe genText
            <*> genMaybe genText
            <*> genMaybe genText
            <*> genMaybe genNonNullRawJson
        , InputFilePart
            <$> genMaybe genText
            <*> genMaybe genText
            <*> genMaybe genText
            <*> genMaybe genText
            <*> genMaybe genText
            <*> genMaybe genNonNullRawJson
        , InputAudioPart
            <$> genRawJson
        , OutputTextPart
            <$> genText
            <*> genMaybe (genSmallList genRawJson)
            <*> genMaybe (genSmallList genRawJson)
        , RefusalPart
            <$> genText
        , ReasoningTextPart
            <$> genText
        , SummaryTextPart
            <$> genText
        , EncryptedContentPart
            <$> genText
        , PlainTextPart
            <$> genText
        , UnknownContentPart <$> genTaggedObject "unknown-content-"
        ]

genResponseRole :: Gen ResponseRole
genResponseRole =
    frequency
        [ (4, elements
            [ RoleUser
            , RoleAssistant
            , RoleSystem
            , RoleDeveloper
            ])
        , (1, RoleUnknown . ("role-" <>) <$> genText)
        ]

genItemStatus :: Gen ItemStatus
genItemStatus =
    frequency
        [ (3, elements
            [ ItemInProgress
            , ItemCompleted
            , ItemIncomplete
            ])
        , (1, ItemStatusUnknown . ("status-" <>) <$> genText)
        ]

genTaggedObject :: Text.Text -> Gen TaggedObject
genTaggedObject prefix =
    TaggedObject
        <$> ((prefix <>) <$> genText)

withoutReservedKeys :: [Text.Text] -> Aeson.Object -> Aeson.Object
withoutReservedKeys names object =
    foldr (KeyMap.delete . Key.fromText) object names

genJsonObject :: Gen Aeson.Object
genJsonObject = sized genJsonObjectAt

genJsonObjectAt :: Int -> Gen Aeson.Object
genJsonObjectAt size = do
    count <- chooseInt (0, min 4 (max 0 size))
    fields <-
        vectorOf count $
            (,)
                <$> (Key.fromText <$> genText)
                <*> resize (max 0 (size - 1)) genJsonValue
    pure (KeyMap.fromList fields)

genJsonValue :: Gen Aeson.Value
genJsonValue = sized go
  where
    go size
        | size <= 0 = scalar
        | otherwise =
            frequency
                [ (6, scalar)
                , (2, do
                    count <- chooseInt (0, min 4 size)
                    Aeson.toJSON
                        <$> vectorOf count
                            (resize (size `div` 2) genJsonValue))
                , (2, Aeson.Object
                        <$> genJsonObjectAt (size `div` 2))
                ]

    scalar =
        oneof
            [ pure Aeson.Null
            , Aeson.Bool <$> arbitrary
            , Aeson.String <$> genText
            , Aeson.Number . fromIntegral
                <$> chooseInt (-100000, 100000)
            ]

genRawJson :: Gen RawJson
genRawJson = rawJsonValue <$> genJsonValue

genNonNullRawJson :: Gen RawJson
genNonNullRawJson =
    suchThatMap genJsonValue \case
        Aeson.Null -> Nothing
        value -> Just (rawJsonValue value)

rawJsonValue :: Aeson.ToJSON value => value -> RawJson
rawJsonValue = rawJsonFromEncoding . Aeson.toEncoding

genText :: Gen Text.Text
genText = do
    length' <- chooseInt (0, 24)
    Text.pack <$> vectorOf length' genTextChar

genTextChar :: Gen Char
genTextChar =
    frequency
        [ (20, elements ['a' .. 'z'])
        , (5, elements ['A' .. 'Z'])
        , (5, elements ['0' .. '9'])
        , (4, elements [' ', '\n', '\t', '"', '\\'])
        , (3, elements ['界', '語', '漢'])
        , (2, elements ['🙂', '🚀', '✓'])
        , (1, elements ['\x0301', 'é', 'ß'])
        ]

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe value =
    frequency
        [ (1, pure Nothing)
        , (3, Just <$> value)
        ]

genSmallList :: Gen a -> Gen [a]
genSmallList value = do
    count <- chooseInt (0, 4)
    vectorOf count value

responseItemKinds :: [String]
responseItemKinds =
    [ "message", "function call", "function output"
    , "custom call", "custom output", "computer call", "computer output"
    , "reasoning"
    , "reference", "agent message", "known tagged", "unknown tagged"
    ]

responseItemKind :: ResponseItem -> String
responseItemKind = \case
    MessageItem{} -> "message"
    FunctionCallItem{} -> "function call"
    FunctionCallOutputItem{} -> "function output"
    CustomToolCallItem{} -> "custom call"
    CustomToolCallOutputItem{} -> "custom output"
    ComputerCallItem{} -> "computer call"
    ComputerCallOutputItem{} -> "computer output"
    ReasoningItemValue{} -> "reasoning"
    ItemReferenceValue{} -> "reference"
    AgentMessageItem{} -> "agent message"
    AdditionalToolsItemValue{} -> "additional tools"
    LocalShellCallItem{} -> "local shell"
    ToolSearchCallItem{} -> "tool search call"
    ToolSearchOutputItem{} -> "tool search output"
    WebSearchCallItem{} -> "web search"
    ImageGenerationCallItem{} -> "image generation"
    CompactionItemValue{} -> "compaction"
    CompactionTriggerItemValue{} -> "compaction trigger"
    ContextCompactionItemValue{} -> "context compaction"
    KnownResponseItem{} -> "known tagged"
    UnknownResponseItem{} -> "unknown tagged"

contentPartKinds :: [String]
contentPartKinds =
    [ "input text", "input image", "input file"
    , "input audio", "output text", "refusal"
    , "reasoning text", "summary text", "encrypted content"
    , "unknown content"
    ]

contentPartKind :: ResponseContentPart -> String
contentPartKind = \case
    InputTextPart{} -> "input text"
    InputImagePart{} -> "input image"
    InputFilePart{} -> "input file"
    InputAudioPart{} -> "input audio"
    OutputTextPart{} -> "output text"
    RefusalPart{} -> "refusal"
    ReasoningTextPart{} -> "reasoning text"
    SummaryTextPart{} -> "summary text"
    EncryptedContentPart{} -> "encrypted content"
    PlainTextPart{} -> "plain text"
    UnknownContentPart{} -> "unknown content"
