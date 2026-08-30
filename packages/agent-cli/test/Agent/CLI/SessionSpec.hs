module Agent.CLI.SessionSpec (spec) where

import Agent.CLI.Session
import Agent.CLI.SessionLock
    ( acquireSessionLock
    , releaseSessionLock
    )
import Agent.CLI.Session.StoreCodec
    ( fromStoredResponseItem
    , toStoredResponseItem
    )
import Agent.CLI.Models (ModelTarget(..))
import Agent.Dialect (DialectId(..))
import Agent.Loop (TokenUsage(..))
import Agent.Provider (Provider(..))
import Agent.Responses.Types
import Agent.Store.Postgres
    ( Store
    , closeStore
    , defaultManagedPostgresConfig
    , openStore
    , storeConfig
    , trustedPool
    )
import Agent.Store.Postgres.Managed (stopManagedPostgres)
import Agent.Store.Postgres.Connection (StorePool)
import Agent.Store.Types (renderStoreError)
import Control.Exception (bracket)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import qualified System.Directory as Directory
import System.Directory.OsPath
    ( createDirectory
    , doesDirectoryExist
    , doesFileExist
    , listDirectory
    , removePathForcibly
    )
import qualified System.FilePath as FilePath
import System.OsPath (OsPath, decodeUtf, unsafeEncodeUtf, (</>))
import System.Posix.Files (fileMode, getFileStatus)
import System.Posix.Temp (mkdtemp)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
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

fromFilePath :: FilePath -> OsPath
fromFilePath = unsafeEncodeUtf

toFilePath :: OsPath -> FilePath
toFilePath path = either (error . show) id (decodeUtf path)

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
        , extraFields = KeyMap.empty
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
        <*> fmap
            (withoutReservedKeys
                ["internal_chat_message_metadata_passthrough"])
            genJsonObject

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
            <*> fmap
                (withoutReservedKeys
                    [ "type", "id", "author", "recipient", "content"
                    , "internal_chat_message_metadata_passthrough"
                    ])
                genJsonObject

jsonRoundTrip :: (Aeson.ToJSON a, Aeson.FromJSON a) => a -> Maybe a
jsonRoundTrip value =
    case Aeson.fromJSON (Aeson.toJSON value) of
        Aeson.Success decoded -> Just decoded
        Aeson.Error _ -> Nothing

withoutReservedKeys :: [Text.Text] -> Aeson.Object -> Aeson.Object
withoutReservedKeys names object =
    foldl
        (flip (KeyMap.delete . Key.fromText))
        object
        names

genAdditionalToolsItem :: Gen AdditionalToolsItem
genAdditionalToolsItem =
    suchThatMap generate jsonRoundTrip
  where
    generate =
        AdditionalToolsItem
            <$> genMaybe genText
            <*> genText
            <*> genSmallList genJsonValue
            <*> fmap
                (withoutReservedKeys ["type", "id", "role", "tools"])
                genJsonObject

genCompactionTriggerItem :: Gen CompactionTriggerItem
genCompactionTriggerItem =
    suchThatMap generate jsonRoundTrip
  where
    generate =
        CompactionTriggerItem
            <$> fmap (withoutReservedKeys ["type"]) genJsonObject

genCompactionItem :: Gen CompactionItem
genCompactionItem =
    suchThatMap generate jsonRoundTrip
  where
    generate =
        CompactionItem
            <$> genMaybe genText
            <*> genMaybe genText
            <*> fmap
                (withoutReservedKeys ["type", "id", "encrypted_content"])
                genJsonObject

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
        <*> genText
        <*> genMaybe (genSmallList genText)
        <*> genMaybe genItemStatus
        <*> fmap
            (withoutReservedKeys ["namespace", "encrypted_function_args"])
            genJsonObject

genFunctionCallOutput :: Gen FunctionCallOutput
genFunctionCallOutput =
    FunctionCallOutput
        <$> genMaybe genText
        <*> genText
        <*> genMaybe genText
        <*> genMaybe genText
        <*> genJsonValue
        <*> genMaybe genItemStatus
        <*> fmap
            (withoutReservedKeys ["name", "namespace"])
            genJsonObject

genCustomToolCall :: Gen CustomToolCall
genCustomToolCall =
    CustomToolCall
        <$> genMaybe genText
        <*> genText
        <*> genText
        <*> genMaybe genText
        <*> genText
        <*> genMaybe genItemStatus
        <*> fmap (withoutReservedKeys ["namespace"]) genJsonObject

genCustomToolCallOutput :: Gen CustomToolCallOutput
genCustomToolCallOutput =
    CustomToolCallOutput
        <$> genMaybe genText
        <*> genText
        <*> genMaybe genText
        <*> genJsonValue
        <*> genMaybe genItemStatus
        <*> genJsonObject

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
        <*> genJsonObject

genReasoningSummaryPart :: Gen ReasoningSummaryPart
genReasoningSummaryPart =
    ReasoningSummaryPart
        <$> genText
        <*> genMaybe genText
        <*> genJsonObject

genItemReference :: Gen ItemReference
genItemReference =
    ItemReference
        <$> genText
        <*> genJsonObject

genContentPart :: Gen ResponseContentPart
genContentPart =
    oneof
        [ InputTextPart
            <$> genText
            <*> genMaybe genJsonValue
            <*> genJsonObject
        , InputImagePart
            <$> genMaybe genText
            <*> genMaybe genText
            <*> genMaybe genText
            <*> genMaybe genJsonValue
            <*> genJsonObject
        , InputFilePart
            <$> genMaybe genText
            <*> genMaybe genText
            <*> genMaybe genText
            <*> genMaybe genText
            <*> genMaybe genText
            <*> genMaybe genJsonValue
            <*> genJsonObject
        , InputAudioPart
            <$> genJsonValue
            <*> genJsonObject
        , OutputTextPart
            <$> genText
            <*> genMaybe (genSmallList genJsonValue)
            <*> genMaybe (genSmallList genJsonValue)
            <*> genJsonObject
        , RefusalPart
            <$> genText
            <*> genJsonObject
        , ReasoningTextPart
            <$> genText
            <*> genJsonObject
        , SummaryTextPart
            <$> genText
            <*> genJsonObject
        , EncryptedContentPart
            <$> genText
            <*> genJsonObject
        , PlainTextPart
            <$> genText
            <*> genJsonObject
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
        <*> genJsonObject

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

spec :: Spec
spec = describe "Agent.CLI.Session" do
    describe "pure compatibility helpers" do
        it "keeps the legacy artifact root and safe ids" do
            sessionsRoot (fromFilePath "/home/marc")
                `shouldBe` fromFilePath "/home/marc/.haskell-agent/sessions"
            sessionTempsRoot
                (fromFilePath "/home/marc/.haskell-agent/sessions")
                `shouldBe`
                    fromFilePath "/home/marc/.haskell-agent/tmp/sessions"
            isValidSessionId "normal-id" `shouldBe` True
            isValidSessionId "../outside" `shouldBe` False
            isValidSessionId "nested/id" `shouldBe` False

        it "derives bounded titles and shell-safe resume hints" do
            sessionTitleFromPrompt
                "one two three four five six seven eight nine ten eleven"
                `shouldBe` "one two three four five six seven eight nine ten"
            resumeHint "it's" "id"
                `shouldBe` "Resume this session with: 'it'\\''s' --resume id"

        it "keeps response-item JSON codecs at the CLI storage boundary" do
            let items =
                    [ MessageItem ResponseMessage
                        { messageId = Just "message-1"
                        , content = MessageContentText "hello"
                        , role = RoleAssistant
                        , status = Just ItemCompleted
                        , phase = Nothing
                        , passthrough = Nothing
                        , extraFields =
                            KeyMap.singleton
                                "provider_extension"
                                (Aeson.Bool True)
                        }
                    , MessageItem ResponseMessage
                        { messageId = Just "message-2"
                        , content = MessageContentParts
                            [ InputTextPart
                                { text = "input"
                                , promptCacheBreakpoint =
                                    Just
                                        (Aeson.object
                                            ["scope" Aeson..= ("turn" :: Text.Text)])
                                , extraFields = KeyMap.empty
                                }
                            , OutputTextPart
                                { text = "output"
                                , annotations =
                                    Just
                                        [ Aeson.object
                                            ["type" Aeson..= ("citation" :: Text.Text)]
                                        ]
                                , logprobs =
                                    Just
                                        [ Aeson.object
                                            ["token" Aeson..= ("output" :: Text.Text)]
                                        ]
                                , extraFields = KeyMap.empty
                                }
                            , UnknownContentPart TaggedObject
                                { tag = "provider_content"
                                , fields =
                                    KeyMap.singleton
                                        "opaque"
                                        (Aeson.Bool True)
                                }
                            ]
                        , role = RoleDeveloper
                        , status = Just ItemInProgress
                        , phase = Just "commentary"
                        , passthrough = Nothing
                        , extraFields = KeyMap.empty
                        }
                    , FunctionCallItem FunctionCall
                        { itemId = Just "call-item"
                        , callId = "call-1"
                        , name = "shell"
                        , namespace = Nothing
                        , arguments = "{\"command\":\"pwd\"}"
                        , encryptedFunctionArgs = Nothing
                        , status = Just ItemCompleted
                        , extraFields = KeyMap.empty
                        }
                    , FunctionCallOutputItem FunctionCallOutput
                        { itemId = Just "output-item"
                        , callId = "call-1"
                        , name = Nothing
                        , namespace = Nothing
                        , output =
                            Aeson.object
                                ["stdout" Aeson..= ("/tmp/project" :: Text.Text)]
                        , status = Just ItemCompleted
                        , extraFields = KeyMap.empty
                        }
                    , CustomToolCallItem CustomToolCall
                        { itemId = Nothing
                        , callId = "custom-1"
                        , name = "apply_patch"
                        , namespace = Nothing
                        , input = "*** Begin Patch"
                        , status = Nothing
                        , extraFields = KeyMap.empty
                        }
                    , CustomToolCallOutputItem CustomToolCallOutput
                        { itemId = Nothing
                        , callId = "custom-1"
                        , name = Just "apply_patch"
                        , output = Aeson.String "Done"
                        , status = Just ItemCompleted
                        , extraFields = KeyMap.empty
                        }
                    , ReasoningItemValue ReasoningItem
                        { itemId = Just "reasoning-1"
                        , summary =
                            [ ReasoningSummaryPart
                                { partType = "summary_text"
                                , text = Just "Checked the schema"
                                , extraFields = KeyMap.empty
                                }
                            ]
                        , content =
                            Just
                                [ ReasoningTextPart
                                    { text = "private placeholder"
                                    , extraFields = KeyMap.empty
                                    }
                                ]
                        , encryptedContent = Just "encrypted"
                        , status = Just ItemCompleted
                        , extraFields = KeyMap.empty
                        }
                    , ItemReferenceValue ItemReference
                        { itemId = "call-item"
                        , extraFields = KeyMap.empty
                        }
                    , AgentMessageItem ResponseAgentMessage
                        { messageId = Nothing
                        , author = Just "researcher"
                        , recipient = Just "root"
                        , content =
                            [ InputTextPart
                                { text = "Found it."
                                , promptCacheBreakpoint = Nothing
                                , extraFields = KeyMap.empty
                                }
                            , EncryptedContentPart
                                { encryptedContent = "opaque-provider-payload"
                                , extraFields = KeyMap.empty
                                }
                            ]
                        , passthrough = Nothing
                        , extraFields = KeyMap.empty
                        }
                    , CompactionTriggerItemValue CompactionTriggerItem
                        { extraFields = KeyMap.empty
                        }
                    , UnknownResponseItem TaggedObject
                        { tag = "provider_item"
                        , fields =
                            KeyMap.singleton "payload" (Aeson.String "opaque")
                        }
                    ]
            traverse fromStoredResponseItem (map toStoredResponseItem items)
                `shouldBe` Right items

        modifyMaxSuccess (const 500) $
            prop "round-trips generated response items through storage" $
                storedResponseItemRoundTrip

        modifyMaxSuccess (const 500) $
            prop "round-trips every generated response content part" $
                storedContentPartRoundTrip

    describe "PostgreSQL session persistence" do
        it "round-trips and clears ephemeral session activity" $
            withTempStore \store root -> do
                let pool = trustedPool store
                handle <- createSession (testCreate pool root)
                persistence <- newActivePersistence handle
                setPersistenceActivity
                    persistence
                    "provider_cooldown"
                    "Waiting before retrying."
                    (Just fixedTime)

                activity <-
                    loadSessionActivity root handle.sessionMeta.metaId
                activity `shouldSatisfy` maybe False
                    (\current ->
                        current.activityKind == "provider_cooldown"
                            && current.activityMessage
                                == "Waiting before retrying."
                            && current.activityRetryAt == Just fixedTime)

                clearPersistenceActivity persistence
                loadSessionActivity root handle.sessionMeta.metaId
                    `shouldReturn` Nothing

        it "clears stale activity when a session is resumed" $
            withTempStore \store root -> do
                let pool = trustedPool store
                handle <- createSession (testCreate pool root)
                persistence <- newActivePersistence handle
                setPersistenceActivity
                    persistence
                    "provider_retry"
                    "Retrying."
                    Nothing
                _ <- newActivePersistence handle
                loadSessionActivity root handle.sessionMeta.metaId
                    `shouldReturn` Nothing

        it "round-trips metadata, provider items, usage, and compaction markers" $
            withTempStore \store root -> do
                let pool = trustedPool store
                handle <- createSession (testCreate pool root)
                doesDirectoryExist handle.sessionDir `shouldReturn` True
                doesDirectoryExist handle.sessionTempDir `shouldReturn` True
                doesFileExist handle.sessionMetaPath `shouldReturn` False
                handle.sessionMeta.metaTitle `shouldBe` "untitled"
                modeOf handle.sessionDir `shouldReturn` 0o700
                modeOf handle.sessionTempDir `shouldReturn` 0o700

                let item = MessageItem ResponseMessage
                        { messageId = Nothing
                        , content = MessageContentParts
                            [InputTextPart "hi" Nothing KeyMap.empty]
                        , role = RoleUser
                        , status = Nothing
                        , phase = Nothing
                        , passthrough = Nothing
                        , extraFields = KeyMap.empty
                        }
                    normalTurn = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "hi there"
                        , turnAssistantText = Just "hello"
                        , turnError = Nothing
                        , turnResponseId = Just "resp-1"
                        , turnItems = [item]
                        , turnUsage = Just TokenUsage
                            { inputTokens = 10
                            , outputTokens = 4
                            , cachedTokens = 2
                            }
                        , turnEffect = TranscriptAppend
                        }
                    compactTurn = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "/compact"
                        , turnAssistantText = Just "Context compacted remotely."
                        , turnError = Nothing
                        , turnResponseId = Nothing
                        , turnItems = []
                        , turnUsage = Nothing
                        , turnEffect = TranscriptReplace
                        }
                withNormal <- appendTurn handle normalTurn
                final <- appendTurnWithMetaUpdate withNormal compactTurn
                    \meta -> meta { metaLastResponseId = Nothing }

                loadSession pool root final.sessionMeta.metaId >>= \case
                    Left err -> expectationFailure (Text.unpack err)
                    Right (meta, turns) -> do
                        meta.metaTitle `shouldBe` "hi there"
                        meta.metaLastResponseId `shouldBe` Nothing
                        turns `shouldBe` [normalTurn, compactTurn]
                        sessionUsageFromTurns meta turns `shouldBe` TokenUsage
                            { inputTokens = 10
                            , outputTokens = 4
                            , cachedTokens = 2
                            }
                loadSessions pool root
                    [final.sessionMeta.metaId, "missing", final.sessionMeta.metaId]
                    >>= \results ->
                        fmap (fmap (\(meta, turns) -> (meta.metaId, turns))) results
                            `shouldBe`
                                [ Right
                                    (final.sessionMeta.metaId, [normalTurn, compactTurn])
                                , Left "session not found: missing"
                                , Right
                                    (final.sessionMeta.metaId, [normalTurn, compactTurn])
                                ]

                listed <- listSessions pool root
                map (.metaId) listed `shouldBe` [handle.sessionMeta.metaId]
                bracket
                    (acquireSessionLock
                        handle.sessionDir
                        handle.sessionMeta.metaId >>= \case
                            Left err -> expectationFailure (Text.unpack err)
                                >> fail "could not acquire session lock"
                            Right lock -> pure lock)
                    releaseSessionLock
                    \_ -> do
                        renameSession
                            pool
                            root
                            handle.sessionMeta.metaId
                            "  Manual   title  " >>= \case
                                Left err ->
                                    expectationFailure (Text.unpack err)
                                Right renamed -> do
                                    renamed.metaTitle `shouldBe` "Manual title"
                                    renamed.metaTitleIsManual `shouldBe` True
                        setSessionArchived
                            pool
                            root
                            handle.sessionMeta.metaId
                            True `shouldReturn` Right ()
                loadSessionMeta pool root handle.sessionMeta.metaId >>= \case
                    Left err -> expectationFailure (Text.unpack err)
                    Right renamed ->
                        renamed.metaTitle `shouldBe` "Manual title"
                listArchivedSessionIds pool
                    `shouldReturn` Right [handle.sessionMeta.metaId]
                _ <- appendTurn final normalTurn
                _ <- setGeneratedSessionTitle 3 "Generated replacement" final
                loadSessionMeta pool root handle.sessionMeta.metaId >>= \case
                    Left err -> expectationFailure (Text.unpack err)
                    Right renamed -> do
                        renamed.metaTitle `shouldBe` "Manual title"
                        renamed.metaTitleIsManual `shouldBe` True
                setSessionArchived
                    pool
                    root
                    handle.sessionMeta.metaId
                    False `shouldReturn` Right ()
                listArchivedSessionIds pool `shouldReturn` Right []
                deleteSession pool root handle.sessionMeta.metaId
                    `shouldReturn` Right ()
                doesDirectoryExist handle.sessionDir `shouldReturn` False
                doesDirectoryExist handle.sessionTempDir `shouldReturn` False
                deleteSession pool root "../outside"
                    `shouldReturn` Left "invalid session id"
                loadSession pool root handle.sessionMeta.metaId
                    `shouldReturn`
                        Left ("session not found: " <> handle.sessionMeta.metaId)

        it "imports a legacy meta.json and JSONL transcript once" $
            withTempStore \store root -> do
                let
                    pool = trustedPool store
                    sessionId = "2026-08-19-legacy"
                    dir = root </> unsafeEncodeUtf (Text.unpack sessionId)
                    metaPath = dir </> unsafeEncodeUtf "meta.json"
                    transcriptPath = dir </> unsafeEncodeUtf "transcript.jsonl"
                    meta = testMeta sessionId
                    turn = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "from disk"
                        , turnAssistantText = Just "imported"
                        , turnError = Nothing
                        , turnResponseId = Nothing
                        , turnItems = []
                        , turnUsage = Nothing
                        , turnEffect = TranscriptAppend
                        }
                createDirectory dir
                LBS.writeFile (toFilePath metaPath) (Aeson.encode meta)
                LBS.writeFile
                    (toFilePath transcriptPath)
                    (Aeson.encode turn <> "\n")

                loadSession pool root sessionId
                    `shouldReturn` Right (meta, [turn])
                -- Removing the source proves the second load is PostgreSQL-only.
                Directory.removeDirectoryRecursive (toFilePath dir)
                loadSession pool root sessionId
                    `shouldReturn` Right (meta, [turn])

        it "keeps pending persistence lazy" $
            withTempStore \store root -> do
                let pool = trustedPool store
                PersistenceEnabled slot <-
                    newPendingPersistence (testCreate pool root)
                listDirectory root `shouldReturn` []
                PersistencePending _ reservedId tempDir <- readIORef slot
                doesDirectoryExist tempDir `shouldReturn` True
                modeOf tempDir `shouldReturn` 0o700
                handle <- ensureSession slot
                doesDirectoryExist handle.sessionDir `shouldReturn` True
                handle.sessionMeta.metaId `shouldBe` reservedId
                handle.sessionTempDir `shouldBe` tempDir
                PersistenceActive again <- readIORef slot
                again.sessionMeta.metaId `shouldBe` handle.sessionMeta.metaId

        it "cleans scratch space for a pending session that never persists" $
            withTempStore \store root -> do
                let pool = trustedPool store
                persist@(PersistenceEnabled slot) <-
                    newPendingPersistence (testCreate pool root)
                PersistencePending _ _ tempDir <- readIORef slot
                cleanupPendingPersistence persist
                doesDirectoryExist tempDir `shouldReturn` False
                listDirectory root `shouldReturn` []

        it "recreates missing scratch space when a session resumes" $
            withTempStore \store root -> do
                let pool = trustedPool store
                handle <- createSession (testCreate pool root)
                removePathForcibly handle.sessionTempDir
                doesDirectoryExist handle.sessionTempDir `shouldReturn` False
                _ <- newActivePersistence handle
                doesDirectoryExist handle.sessionTempDir `shouldReturn` True
                modeOf handle.sessionTempDir `shouldReturn` 0o700

    describe "json codec" do
        it "encodes and decodes SessionTurn" do
            let turn = SessionTurn
                    { turnAt = fixedTime
                    , turnUserText = "q"
                    , turnAssistantText = Nothing
                    , turnError = Just "cancelled"
                    , turnResponseId = Nothing
                    , turnItems = []
                    , turnUsage = Nothing
                    , turnEffect = TranscriptAppend
                    }
            Aeson.eitherDecode (Aeson.encode turn) `shouldBe` Right turn

        it "round-trips recap metadata" do
            let meta =
                    (testMeta "session-1")
                        { metaLastRecap = Just "We fixed auth retries."
                        , metaLastTurnSummary = Just "Auth retries wired"
                        , metaLastRecapMainTurns = 3
                        }
            Aeson.eitherDecode (Aeson.encode meta) `shouldBe` Right meta

        it "infers transcript effects when importing legacy JSON turns" do
            let legacy = Aeson.object
                    [ "at" Aeson..= fixedTime
                    , "userText" Aeson..= ("/compact focus" :: Text.Text)
                    , "assistantText" Aeson..= (Nothing :: Maybe Text.Text)
                    , "error" Aeson..= (Nothing :: Maybe Text.Text)
                    , "responseId" Aeson..= (Nothing :: Maybe Text.Text)
                    , "items" Aeson..= ([] :: [ResponseItem])
                    , "usage" Aeson..= (Nothing :: Maybe TokenUsage)
                    ]
                decoded =
                    Aeson.eitherDecode (Aeson.encode legacy)
                        :: Either String SessionTurn
            fmap (\turn -> turn.turnEffect) decoded
                `shouldBe` Right TranscriptReplace

testCreate :: StorePool -> OsPath -> SessionCreate
testCreate pool root = SessionCreate
    { createPool = pool
    , createRoot = root
    , createTarget = ModelTarget
        { targetProvider = XAIProvider
        , targetConnectionId = "xai"
        , targetModelId = "grok-4"
        , targetWireModelId = "grok-4"
        , targetDialect = GrokBuildDialect
        }
    , createCwd = fromFilePath "/tmp/work"
    , createEffort = "low"
    , createTitleHint = Nothing
    , createTitleIsManual = False
    }

testMeta :: Text.Text -> SessionMeta
testMeta sessionId = SessionMeta
    { metaVersion = 1
    , metaId = sessionId
    , metaCreatedAt = fixedTime
    , metaUpdatedAt = fixedTime
    , metaProvider = XAIProvider
    , metaConnection = "xai"
    , metaModel = "grok-4"
    , metaTransportModel = Just "grok-4"
    , metaDialect = GrokBuildDialect
    , metaLegacySubagentTarget = Just LegacySubagentTarget
        { legacyTargetProvider = XAIProvider
        , legacyTargetConnection = "xai"
        , legacyTargetEffectiveModel = "grok-4"
        , legacyTargetDialect = GrokBuildDialect
        }
    , metaCwd = fromFilePath "/tmp/work"
    , metaEffort = "low"
    , metaTitle = "legacy"
    , metaTitleIsManual = False
    , metaTitleRefreshIndex = 0
    , metaTitleUserTurns = 0
    , metaLastResponseId = Nothing
    , metaInputTokens = 0
    , metaOutputTokens = 0
    , metaCachedTokens = 0
    , metaLastRecap = Nothing
    , metaLastTurnSummary = Nothing
    , metaLastRecapMainTurns = 0
    }

fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2026 8 19) (secondsToDiffTime 0)

modeOf :: OsPath -> IO Integer
modeOf path = do
    status <- getFileStatus (toFilePath path)
    pure (fromIntegral (fileMode status `mod` 0o1000))

withTempStore :: (Store -> OsPath -> IO a) -> IO a
withTempStore action = do
    tmp <- Directory.getTemporaryDirectory
    bracket
        (mkdtemp (tmp FilePath.</> "hs"))
        Directory.removeDirectoryRecursive
        \basePath -> do
            let
                stateDirectory = basePath FilePath.</> ".haskell-agent"
                sessionsDirectory =
                    stateDirectory FilePath.</> "sessions"
                config = defaultManagedPostgresConfig stateDirectory ""
            Directory.createDirectoryIfMissing True sessionsDirectory
            bracket
                (openStore config >>= either
                    (fail . Text.unpack . renderStoreError)
                    pure)
                (\store -> do
                    closeStore store
                    _ <- stopManagedPostgres (storeConfig store)
                    pure ())
                (\store -> action store (fromFilePath sessionsDirectory))
