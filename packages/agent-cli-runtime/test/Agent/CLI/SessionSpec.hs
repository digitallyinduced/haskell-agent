module Agent.CLI.SessionSpec (spec) where

import Agent.CLI.Session
import Agent.CLI.SessionLock (acquireSessionLock, releaseSessionLock)
import Agent.CLI.Request (requestParams)
import Agent.CLI.Session.StoreCodec
    ( fromStoredResponseItem
    , toStoredResponseItem
    )
import Agent.CLI.Models (ModelTarget(..))
import Agent.Dialect (DialectId(..))
import Agent.Json (RawJson, rawJsonFromEncoding)
import Agent.Json.Decode qualified as Hermes
import Agent.Loop (TokenUsage(..))
import Agent.Telemetry (TurnTelemetry(..))
import Agent.Provider (Provider(..))
import Agent.Responses.Types
import Agent.Store.SessionItem
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
import qualified Agent.Store.Postgres.Session as Store
import Agent.Store.Types (renderStoreError)
import Control.Concurrent (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Concurrent.Async (concurrently, mapConcurrently)
import Control.Exception.Safe (bracket)
import Control.Monad (replicateM)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), getCurrentTime, secondsToDiffTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified System.Directory as Directory
import System.Directory.OsPath
    ( createDirectory
    , createDirectoryIfMissing
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
            (\label -> cover 7 (responseItemKind item == label) label)
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

genCustomToolCall :: Gen CustomToolCall
genCustomToolCall =
    CustomToolCall
        <$> genMaybe genText
        <*> genText
        <*> genText
        <*> genMaybe genText
        <*> genText
        <*> genMaybe genItemStatus

genCustomToolCallOutput :: Gen CustomToolCallOutput
genCustomToolCallOutput =
    CustomToolCallOutput
        <$> genMaybe genText
        <*> genText
        <*> genMaybe genText
        <*> genRawJson
        <*> genMaybe genItemStatus

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
    , "custom call", "custom output", "reasoning"
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
        it "reuses prompt bytes only for the same target and tool identities" do
            let sessionId = "session-prompt"
                snapshot =
                    (testPromptSnapshot sessionId)
                        { promptSnapshotTools =
                            [promptFunctionTool "lookup" "old documentation"]
                        }
                regenerated =
                    requestParams
                        XAIProvider
                        "grok-4"
                        "new binary instructions"
                        [promptFunctionTool "lookup" "new documentation"]
                        "low"
                renamed =
                    requestParams
                        XAIProvider
                        "grok-4"
                        "new binary instructions"
                        [promptFunctionTool "search" "new documentation"]
                        "low"
                compatible params cwd cacheKey =
                    compatibleSessionPromptSnapshot
                        XAIProvider
                        "xai"
                        GrokBuildDialect
                        cwd
                        cacheKey
                        params
                        (Just snapshot)
            compatible
                regenerated
                (fromFilePath "/tmp/work")
                (Just sessionId)
                `shouldBe` Just snapshot
            compatible
                renamed
                (fromFilePath "/tmp/work")
                (Just sessionId)
                `shouldBe` Nothing
            compatible
                regenerated
                (fromFilePath "/tmp/other")
                (Just sessionId)
                `shouldBe` Nothing
            compatible
                regenerated
                (fromFilePath "/tmp/work")
                (Just "other-session")
                `shouldBe` Nothing

        it "round-trips typed computer calls through storage" do
            let items =
                    [ ComputerCallItem ComputerCall
                        { computerCallItemId = Just "item-1"
                        , computerCallId = "call-1"
                        , computerActions = [ClickAction 12 34 "left"]
                        , pendingSafetyChecks = []
                        , computerCallStatus = Nothing
                        , computerCallExtra = KeyMap.empty
                        }
                    , ComputerCallOutputItem ComputerCallOutput
                        { computerOutputItemId = Nothing
                        , computerOutputCallId = "call-1"
                        , screenshotDataUrl = "data:image/png;base64,AA=="
                        , acknowledgedChecks = []
                        , computerOutputStatus = Nothing
                        , computerOutputExtra = KeyMap.empty
                        }
                    ]
            traverse fromStoredResponseItem (map toStoredResponseItem items)
                `shouldBe` Right items

        it "stores inline image and file payloads as binary data" do
            let imageUrl = "data:image/png;base64,cG5nLWJ5dGVz"
                fileData = "data:text/plain;base64,ZmlsZS1ieXRlcw=="
                item = MessageItem ResponseMessage
                    { messageId = Nothing
                    , content = MessageContentParts
                        [ InputImagePart
                            Nothing
                            Nothing
                            (Just imageUrl)
                            Nothing
                        , InputFilePart
                            Nothing
                            (Just fileData)
                            Nothing
                            Nothing
                            (Just "notes.txt")
                            Nothing
                        ]
                    , role = RoleUser
                    , status = Nothing
                    , phase = Nothing
                    , passthrough = Nothing
                    }
            case toStoredResponseItem item of
                StoredMessageItem StoredMessage
                    { storedMessageContent =
                        StoredMessageParts [imagePart, filePart]
                    } -> do
                        imagePart.storedContentPartImageUrl
                            `shouldBe` Nothing
                        imagePart.storedContentPartImageBinary
                            `shouldBe` Just StoredBinaryData
                                { storedBinaryDataMimeType = "image/png"
                                , storedBinaryDataBytes = "png-bytes"
                                }
                        filePart.storedContentPartFileData
                            `shouldBe` Nothing
                        filePart.storedContentPartFileBinary
                            `shouldBe` Just StoredBinaryData
                                { storedBinaryDataMimeType = "text/plain"
                                , storedBinaryDataBytes = "file-bytes"
                                }
                stored ->
                    expectationFailure
                        ("unexpected stored item: " <> show stored)
            fromStoredResponseItem (toStoredResponseItem item)
                `shouldBe` Right item

        it "keeps hosted and malformed attachment URLs as text" do
            let item = MessageItem ResponseMessage
                    { messageId = Nothing
                    , content = MessageContentParts
                        [ InputImagePart
                            Nothing
                            Nothing
                            (Just "https://example.com/image.png")
                            Nothing
                        , InputFilePart
                            Nothing
                            (Just "data:text/plain;base64,not base64")
                            Nothing
                            Nothing
                            Nothing
                            Nothing
                        ]
                    , role = RoleUser
                    , status = Nothing
                    , phase = Nothing
                    , passthrough = Nothing
                    }
            fromStoredResponseItem (toStoredResponseItem item)
                `shouldBe` Right item

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

        it "removes old session scratch directories beyond retention" $
            withTempSessionRoot \root -> do
                older <- addSessionTemp root "2026-08-20-00000001"
                newer <- addSessionTemp root "2026-08-21-00000002"

                report <- cleanupStaleSessionTemps root 1 []

                report.tempCleanupFailures `shouldBe` []
                report.tempCleanupRemoved `shouldBe` [older]
                doesDirectoryExist older `shouldReturn` False
                doesDirectoryExist newer `shouldReturn` True

        it "tolerates concurrent stale scratch directory cleanup" $
            withTempSessionRoot \root -> do
                let workerCount = 32
                    sessionId n =
                        "2026-08-20-"
                            <> replicate (8 - length (show n)) '0'
                            <> show n
                mapM_ (addSessionTemp root . sessionId) [1 .. workerCount]
                ready <- replicateM workerCount newEmptyMVar
                start <- newEmptyMVar
                (reports, ()) <-
                    concurrently
                        (mapConcurrently
                            (\readyVar -> do
                                putMVar readyVar ()
                                readMVar start
                                cleanupStaleSessionTemps root 1 [])
                            ready)
                        (mapM_ takeMVar ready >> putMVar start ())

                concatMap (.tempCleanupFailures) reports `shouldBe` []

        it "never collects scratch directories allocated today" $
            withTempSessionRoot \root -> do
                day <- formatTime defaultTimeLocale "%Y-%m-%d"
                    <$> getCurrentTime
                first <- addSessionTemp root (day <> "-00000001")
                second <- addSessionTemp root (day <> "-00000002")

                report <- cleanupStaleSessionTemps root 1 []

                report.tempCleanupRemoved `shouldBe` []
                doesDirectoryExist first `shouldReturn` True
                doesDirectoryExist second `shouldReturn` True

        it "preserves old session scratch directories with a live lease" $
            withTempSessionRoot \root -> do
                older <- addSessionTemp root "2026-08-20-00000001"
                _ <- addSessionTemp root "2026-08-21-00000002"
                lease <- acquireSessionTempLease root older >>= \case
                    Right (Just value) -> pure value
                    _ -> expectationFailure
                        "expected a managed session-temp lease"
                        >> fail "missing session-temp lease"

                report <- cleanupStaleSessionTemps root 1 []
                report.tempCleanupRemoved `shouldBe` []
                doesDirectoryExist older `shouldReturn` True

                releaseSessionTempLease lease
                second <- cleanupStaleSessionTemps root 1 []
                second.tempCleanupRemoved `shouldBe` [older]
                doesDirectoryExist older `shouldReturn` False

        it "preserves scratch for a running durable session" $
            withTempSessionRoot \root -> do
                let sessionId = "2026-08-20-00000001"
                    durableDir = root </> fromFilePath sessionId
                older <- addSessionTemp root sessionId
                _ <- addSessionTemp root "2026-08-21-00000002"
                createDirectory durableDir
                lock <- acquireSessionLock durableDir (Text.pack sessionId)
                    >>= \case
                        Left err ->
                            expectationFailure (Text.unpack err)
                                >> fail "missing durable session lock"
                        Right value -> pure value

                report <- cleanupStaleSessionTemps root 1 []
                report.tempCleanupRemoved `shouldBe` []
                doesDirectoryExist older `shouldReturn` True

                releaseSessionLock lock
                second <- cleanupStaleSessionTemps root 1 []
                second.tempCleanupRemoved `shouldBe` [older]
                doesDirectoryExist older `shouldReturn` False

        it "ignores non-session directories in the scratch root" $
            withTempSessionRoot \root -> do
                let custom =
                        sessionTempsRoot root
                            </> fromFilePath "keep-custom"
                createDirectoryIfMissing True custom
                _ <- addSessionTemp root "2026-08-21-00000002"

                report <- cleanupStaleSessionTemps root 1 []

                report.tempCleanupRemoved `shouldBe` []
                doesDirectoryExist custom `shouldReturn` True

        it "derives bounded titles and shell-safe resume hints" do
            sessionTitleFromPrompt
                "one two three four five six seven eight nine ten eleven"
                `shouldBe` "one two three four five six seven eight nine ten"
            resumeHint "it's" "id"
                `shouldBe` "Resume this session with: 'it'\\''s' --resume id"

        it "offers rewind targets from the current branch with retained prefixes" do
            let turn effect userText responseId = SessionTurn
                    { turnAt = fixedTime
                    , turnUserText = userText
                    , turnAssistantText = Just "answer"
                    , turnError = Nothing
                    , turnResponseId = responseId
                    , turnItems = []
                    , turnUsage = Nothing
                    , turnEffect = effect
                    , turnProviderTelemetry = []
                    }
                old = turn TranscriptAppend "old prompt" (Just "old")
                reset = turn TranscriptReset "/clear" Nothing
                first = turn TranscriptAppend "first prompt" (Just "first")
                checkpoint =
                    turn TranscriptReplace "/compact" Nothing
                second = turn TranscriptAppend "second prompt" (Just "second")
                choices =
                    sessionRewindChoices
                        [old, reset, first, checkpoint, second]
            map
                (\(prompt, retained) ->
                    (prompt.turnUserText, map (.turnUserText) retained))
                choices
                `shouldBe`
                    [ ("first prompt", [])
                    , ("second prompt", ["first prompt", "/compact"])
                    ]

        it "keeps response-item JSON codecs at the CLI storage boundary" do
            let items =
                    [ MessageItem ResponseMessage
                        { messageId = Just "message-1"
                        , content = MessageContentText "hello"
                        , role = RoleAssistant
                        , status = Just ItemCompleted
                        , phase = Nothing
                        , passthrough = Nothing
                        }
                    , MessageItem ResponseMessage
                        { messageId = Just "message-2"
                        , content = MessageContentParts
                            [ InputTextPart
                                { text = "input"
                                , promptCacheBreakpoint =
                                    Just (rawJsonValue (Aeson.object
                                        ["scope" Aeson..= ("turn" :: Text.Text)]))
                                }
                            , OutputTextPart
                                { text = "output"
                                , annotations =
                                    Just
                                        [ rawJsonValue (Aeson.object
                                            ["type" Aeson..= ("citation" :: Text.Text)])
                                        ]
                                , logprobs =
                                    Just
                                        [ rawJsonValue (Aeson.object
                                            ["token" Aeson..= ("output" :: Text.Text)])
                                        ]
                                }
                            , UnknownContentPart (TaggedObject "provider_content")
                            ]
                        , role = RoleDeveloper
                        , status = Just ItemInProgress
                        , phase = Just "commentary"
                        , passthrough = Nothing
                        }
                    , FunctionCallItem FunctionCall
                        { itemId = Just "call-item"
                        , callId = "call-1"
                        , name = "shell"
                        , namespace = Nothing
                        , provider = Nothing
                        , arguments = "{\"command\":\"pwd\"}"
                        , encryptedFunctionArgs = Nothing
                        , status = Just ItemCompleted
                        }
                    , FunctionCallOutputItem FunctionCallOutput
                        { itemId = Just "output-item"
                        , callId = "call-1"
                        , name = Nothing
                        , namespace = Nothing
                        , provider = Nothing
                        , output = rawJsonValue (Aeson.object
                            ["stdout" Aeson..= ("/tmp/project" :: Text.Text)])
                        , status = Just ItemCompleted
                        }
                    , CustomToolCallItem CustomToolCall
                        { itemId = Nothing
                        , callId = "custom-1"
                        , name = "apply_patch"
                        , namespace = Nothing
                        , input = "*** Begin Patch"
                        , status = Nothing
                        }
                    , CustomToolCallOutputItem CustomToolCallOutput
                        { itemId = Nothing
                        , callId = "custom-1"
                        , name = Just "apply_patch"
                        , output = rawJsonValue ("Done" :: Text.Text)
                        , status = Just ItemCompleted
                        }
                    , ReasoningItemValue ReasoningItem
                        { itemId = Just "reasoning-1"
                        , summary =
                            [ ReasoningSummaryPart
                                { partType = "summary_text"
                                , text = Just "Checked the schema"
                                }
                            ]
                        , content =
                            Just
                                [ ReasoningTextPart
                                    { text = "private placeholder"
                                    }
                                ]
                        , encryptedContent = Just "encrypted"
                        , status = Just ItemCompleted
                        }
                    , ItemReferenceValue ItemReference
                        { itemId = "call-item"
                        }
                    , AgentMessageItem ResponseAgentMessage
                        { messageId = Nothing
                        , author = Just "researcher"
                        , recipient = Just "root"
                        , content =
                            [ InputTextPart
                                { text = "Found it."
                                , promptCacheBreakpoint = Nothing
                                }
                            , EncryptedContentPart
                                { encryptedContent = "opaque-provider-payload"
                                }
                            ]
                        , passthrough = Nothing
                        }
                    , CompactionTriggerItemValue CompactionTriggerItem
                    , UnknownResponseItem (TaggedObject "provider_item")
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
        it "forks turns, metadata, and only allowlisted durable artifacts" $
            withTempStore \store root -> do
                let pool = trustedPool store
                source0 <- createSession (testCreate pool root)
                let sourceTurn = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "branch from here"
                        , turnAssistantText = Just "ready"
                        , turnError = Nothing
                        , turnResponseId = Just "response-parent"
                        , turnItems = []
                        , turnUsage = Just TokenUsage
                            { inputTokens = 8
                            , outputTokens = 3
                            , cachedTokens = 1
                            }
                        , turnEffect = TranscriptAppend
                        , turnProviderTelemetry = []
                        }
                source <- appendTurnWithMetaUpdate source0 sourceTurn
                    \meta -> meta
                        { metaLastRecap = Just "recap"
                        , metaLastTurnSummary = Just "summary"
                        }
                let planPath = source.sessionDir </> unsafeEncodeUtf "plan.md"
                    agentsDir = source.sessionDir </> unsafeEncodeUtf "agents"
                    childDir = agentsDir </> unsafeEncodeUtf "child"
                    childPath = childDir </> unsafeEncodeUtf "meta.json"
                    ignoredPath = source.sessionDir </> unsafeEncodeUtf "agent.log"
                createDirectory agentsDir
                createDirectory childDir
                LBS.writeFile (toFilePath planPath) "plan"
                LBS.writeFile (toFilePath childPath) "child"
                LBS.writeFile (toFilePath ignoredPath) "runtime log"

                let forkCwd = root </> unsafeEncodeUtf "fork-worktree"
                forkSessionAt
                    root
                    source
                    [sourceTurn]
                    (Just "Fork title")
                    forkCwd >>= \case
                    Left err -> expectationFailure (Text.unpack err)
                    Right forked -> do
                        forked.sessionMeta.metaId
                            `shouldNotBe` source.sessionMeta.metaId
                        forked.sessionMeta.metaCreatedAt
                            `shouldSatisfy` (>= source.sessionMeta.metaCreatedAt)
                        forked.sessionMeta.metaUpdatedAt
                            `shouldBe` forked.sessionMeta.metaCreatedAt
                        forked.sessionMeta.metaTitle `shouldBe` "Fork title"
                        forked.sessionMeta.metaTitleIsManual `shouldBe` True
                        forked.sessionMeta.metaCwd `shouldBe` forkCwd
                        forked.sessionMeta.metaLastResponseId
                            `shouldBe` Just "response-parent"
                        forked.sessionMeta.metaLastRecap `shouldBe` Just "recap"
                        forked.sessionMeta.metaLastTurnSummary
                            `shouldBe` Just "summary"
                        loadSession pool root forked.sessionMeta.metaId
                            `shouldReturn`
                                Right (forked.sessionMeta, [sourceTurn])
                        doesFileExist
                            (forked.sessionDir </> unsafeEncodeUtf "plan.md")
                            `shouldReturn` True
                        doesFileExist
                            (forked.sessionDir
                                </> unsafeEncodeUtf "agents"
                                </> unsafeEncodeUtf "child"
                                </> unsafeEncodeUtf "meta.json")
                            `shouldReturn` True
                        doesFileExist
                            (forked.sessionDir </> unsafeEncodeUtf "agent.log")
                            `shouldReturn` False
                        let forkOnlyTurn = sourceTurn
                                { turnUserText = "continue only on fork"
                                , turnAssistantText = Just "fork response"
                                , turnResponseId = Just "fork-response"
                                , turnUsage = Nothing
                                }
                        forkedFinal <- appendTurn forked forkOnlyTurn
                        loadSession pool root source.sessionMeta.metaId
                            `shouldReturn`
                                Right (source.sessionMeta, [sourceTurn])
                        loadSession pool root forkedFinal.sessionMeta.metaId
                            `shouldReturn`
                                Right
                                    ( forkedFinal.sessionMeta
                                    , [sourceTurn, forkOnlyTurn]
                                    )

        it "rejects symlinked fork artifacts and cleans reserved state" $
            withTempStore \store root -> do
                let pool = trustedPool store
                source0 <- createSession (testCreate pool root)
                let sourceTurn = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "branch from here"
                        , turnAssistantText = Just "ready"
                        , turnError = Nothing
                        , turnResponseId = Nothing
                        , turnItems = []
                        , turnUsage = Nothing
                        , turnEffect = TranscriptAppend
                        , turnProviderTelemetry = []
                        }
                source <- appendTurn source0 sourceTurn
                let outside = source.sessionDir </> unsafeEncodeUtf "outside"
                    planPath = source.sessionDir </> unsafeEncodeUtf "plan.md"
                LBS.writeFile (toFilePath outside) "outside"
                Directory.createFileLink
                    (toFilePath outside)
                    (toFilePath planPath)
                before <- listDirectory root
                beforeTemps <- listDirectory (sessionTempsRoot root)
                forkSession root source [sourceTurn] Nothing >>= \case
                    Left err ->
                        err `shouldSatisfy`
                            Text.isInfixOf "refusing to copy symbolic link"
                    Right forked ->
                        expectationFailure
                            ("unexpected fork: " <> show forked.sessionMeta.metaId)
                after <- listDirectory root
                after `shouldMatchList` before
                afterTemps <- listDirectory (sessionTempsRoot root)
                afterTemps `shouldMatchList` beforeTemps

        it "requires a substantive persisted turn before forking" $
            withTempStore \store root -> do
                source <- createSession (testCreate (trustedPool store) root)
                forkSession root source [] Nothing >>= \case
                    Left err ->
                        err
                            `shouldBe`
                                "a session must contain at least one turn before it can be forked"
                    Right forked ->
                        expectationFailure
                            ("unexpected fork: " <> show forked.sessionMeta.metaId)

        it "requires a substantive turn after the latest reset before forking" $
            withTempStore \store root -> do
                let pool = trustedPool store
                source0 <- createSession (testCreate pool root)
                let sourceTurn = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "old conversation"
                        , turnAssistantText = Just "old answer"
                        , turnError = Nothing
                        , turnResponseId = Just "old-response"
                        , turnItems = []
                        , turnUsage = Nothing
                        , turnEffect = TranscriptAppend
                        , turnProviderTelemetry = []
                        }
                source <- appendTurn source0 sourceTurn
                let reset = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "/clear"
                        , turnAssistantText = Just "Conversation cleared."
                        , turnError = Nothing
                        , turnResponseId = Nothing
                        , turnItems = []
                        , turnUsage = Nothing
                        , turnEffect = TranscriptReset
                        , turnProviderTelemetry = []
                        }
                resetSource <- appendTurnKeepTitle source reset

                forkSession
                    root resetSource
                    [ sourceTurn
                    , reset
                    ]
                    Nothing >>= \case
                        Left err ->
                            err `shouldBe`
                                "a session must contain at least one turn before it can be forked"
                        Right forked ->
                            expectationFailure
                                ("unexpected fork: "
                                    <> show forked.sessionMeta.metaId)

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
                            [InputTextPart "hi" Nothing]
                        , role = RoleUser
                        , status = Nothing
                        , phase = Nothing
                        , passthrough = Nothing
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
                        , turnProviderTelemetry = [sampleTurnTelemetry]
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
                        , turnProviderTelemetry = []
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

                (listed, warnings) <- listSessions pool root
                map (.metaId) listed `shouldBe` [handle.sessionMeta.metaId]
                warnings `shouldBe` []
                deleteSession pool root handle.sessionMeta.metaId
                    `shouldReturn` Right ()
                doesDirectoryExist handle.sessionDir `shouldReturn` False
                doesDirectoryExist handle.sessionTempDir `shouldReturn` False
                deleteSession pool root "../outside"
                    `shouldReturn` Left "invalid session id"
                loadSession pool root handle.sessionMeta.metaId
                    `shouldReturn`
                        Left ("session not found: " <> handle.sessionMeta.metaId)

        it "publishes rewind branches while preserving checkpoints and usage" $
            withTempStore \store root -> do
                let
                    pool = trustedPool store
                    first = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "first prompt"
                        , turnAssistantText = Just "first answer"
                        , turnError = Nothing
                        , turnResponseId = Just "response-first"
                        , turnItems = []
                        , turnUsage = Just TokenUsage
                            { inputTokens = 10
                            , outputTokens = 4
                            , cachedTokens = 2
                            }
                        , turnEffect = TranscriptAppend
                        , turnProviderTelemetry = []
                        }
                    checkpoint = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "/compact"
                        , turnAssistantText = Just "Context compacted remotely."
                        , turnError = Nothing
                        , turnResponseId = Nothing
                        , turnItems = []
                        , turnUsage = Nothing
                        , turnEffect = TranscriptReplace
                        , turnProviderTelemetry = []
                        }
                    later = SessionTurn
                        { turnAt = fixedTime
                        , turnUserText = "later prompt"
                        , turnAssistantText = Just "later answer"
                        , turnError = Nothing
                        , turnResponseId = Just "response-later"
                        , turnItems = []
                        , turnUsage = Just TokenUsage
                            { inputTokens = 7
                            , outputTokens = 3
                            , cachedTokens = 1
                            }
                        , turnEffect = TranscriptAppend
                        , turnProviderTelemetry = []
                        }
                initial <- createSession (testCreate pool root)
                withFirst <- appendTurnWithMetaUpdate initial first
                    \meta -> meta { metaTitleUserTurns = 1 }
                withCheckpoint <-
                    appendTurnWithMetaUpdate withFirst checkpoint
                        \meta -> meta { metaLastResponseId = Nothing }
                final <- appendTurnWithMetaUpdate withCheckpoint later
                    \meta -> meta
                        { metaTitleRefreshIndex = 2
                        , metaTitleUserTurns = 2
                        , metaLastRecap = Just "stale recap"
                        , metaLastTurnSummary = Just "stale summary"
                        , metaLastRecapMainTurns = 2
                        }

                rewound <- rewindSession final [first, checkpoint] >>= \case
                    Left err ->
                        expectationFailure (Text.unpack err)
                            >> fail "rewind failed"
                    Right handle -> pure handle

                rewound.sessionMeta.metaLastResponseId `shouldBe` Nothing
                rewound.sessionMeta.metaTitleRefreshIndex `shouldBe` 0
                rewound.sessionMeta.metaTitleUserTurns `shouldBe` 1
                rewound.sessionMeta.metaLastRecap `shouldBe` Nothing
                rewound.sessionMeta.metaLastTurnSummary `shouldBe` Nothing
                rewound.sessionMeta.metaLastRecapMainTurns `shouldBe` 0
                rewound.sessionMeta.metaInputTokens `shouldBe` 17
                rewound.sessionMeta.metaOutputTokens `shouldBe` 7
                rewound.sessionMeta.metaCachedTokens `shouldBe` 3

                loadSession pool root rewound.sessionMeta.metaId >>= \case
                    Left err -> expectationFailure (Text.unpack err)
                    Right (meta, turns) -> do
                        meta `shouldBe` rewound.sessionMeta
                        case turns of
                            [ originalFirst
                                , originalCheckpoint
                                , originalLater
                                , marker
                                , replayedFirst
                                , replayedCheckpoint
                                ] -> do
                                    originalFirst `shouldBe` first
                                    originalCheckpoint `shouldBe` checkpoint
                                    originalLater `shouldBe` later
                                    marker.turnUserText `shouldBe` "/rewind"
                                    marker.turnAssistantText
                                        `shouldBe` Just "Conversation rewound."
                                    marker.turnResponseId `shouldBe` Nothing
                                    marker.turnEffect `shouldBe` TranscriptReset
                                    replayedFirst `shouldBe` first
                                    replayedCheckpoint `shouldBe` checkpoint
                            _ ->
                                expectationFailure
                                    ("unexpected rewind transcript: "
                                        <> show turns)

                loadActiveSession pool root rewound.sessionMeta.metaId
                    `shouldReturn`
                        Right (rewound.sessionMeta, [checkpoint])

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
                        , turnProviderTelemetry = []
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

        it "materializes pending persistence into a resumable session ID" $
            withTempStore \store root -> do
                let pool = trustedPool store
                persist@(PersistenceEnabled slot) <-
                    newPendingPersistence (testCreate pool root)
                listDirectory root `shouldReturn` []
                PersistencePending _ reservedId tempDir <- readIORef slot
                doesDirectoryExist tempDir `shouldReturn` True
                modeOf tempDir `shouldReturn` 0o700
                ensurePersistenceSessionId persist
                    `shouldReturn` Just reservedId
                PersistenceActive handle <- readIORef slot
                doesDirectoryExist handle.sessionDir `shouldReturn` True
                handle.sessionMeta.metaId `shouldBe` reservedId
                handle.sessionTempDir `shouldBe` tempDir
                loadSession pool root reservedId
                    `shouldReturn` Right (handle.sessionMeta, [])

        it "creates and advances immutable prompt epochs before first use" $
            withTempStore \store root -> do
                let pool = trustedPool store
                PersistenceEnabled slot <-
                    newPendingPersistence (testCreate pool root)
                PersistencePending _ reservedId _ <- readIORef slot
                let initial = testPromptSnapshot reservedId
                handle <-
                    ensureSessionWithPromptSnapshot slot initial
                handle.sessionMeta.metaPromptSnapshot
                    `shouldBe` Just initial
                initialEpoch <-
                    Store.loadLatestSessionPromptEpoch pool reservedId
                fmap (fmap (.sessionPromptEpochIndex)) initialEpoch
                    `shouldBe` Right (Just 0)

                -- Consuming the one-shot generated context does not create a
                -- new epoch; the original value remains available to repair
                -- a crash before the first transcript turn is durable.
                unchanged <-
                    ensureSessionWithPromptSnapshot
                        slot
                        initial
                            { promptSnapshotGeneratedContext = Nothing
                            , promptSnapshotGrokContext = Nothing
                            }
                unchanged.sessionMeta.metaPromptSnapshot
                    `shouldBe` Just initial
                unchangedEpoch <-
                    Store.loadLatestSessionPromptEpoch pool reservedId
                fmap (fmap (.sessionPromptEpochIndex)) unchangedEpoch
                    `shouldBe` Right (Just 0)

                let advanced = initial
                        { promptSnapshotInstructions =
                            "updated persisted instructions"
                        , promptSnapshotGeneratedContext = Nothing
                        , promptSnapshotGrokContext = Nothing
                        }
                latest <-
                    ensureSessionWithPromptSnapshot slot advanced
                latest.sessionMeta.metaPromptSnapshot
                    `shouldBe` Just advanced
                advancedEpoch <-
                    Store.loadLatestSessionPromptEpoch pool reservedId
                fmap (fmap (.sessionPromptEpochIndex)) advancedEpoch
                    `shouldBe` Right (Just 1)
                loadSession pool root reservedId >>= \case
                    Right (loadedMeta, []) ->
                        loadedMeta.metaPromptSnapshot
                            `shouldBe` Just advanced
                    result ->
                        expectationFailure
                            ("unexpected prompt session: " <> show result)

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
                    , turnProviderTelemetry = [sampleTurnTelemetry]
                    }
            Hermes.decodeEither sessionTurnDecoder
                (LBS.toStrict (Aeson.encode turn))
                `shouldBe` Right turn

        it "round-trips recap metadata" do
            let meta =
                    (testMeta "session-1")
                        { metaLastRecap = Just "We fixed auth retries."
                        , metaLastTurnSummary = Just "Auth retries wired"
                        , metaLastRecapMainTurns = 3
                        }
            Hermes.decodeEither sessionMetaDecoder
                (LBS.toStrict (Aeson.encode meta))
                `shouldBe` Right meta

        it "infers transcript effects when importing legacy JSON turns" do
            let legacy userText = Aeson.object
                    [ "at" Aeson..= fixedTime
                    , "userText" Aeson..= (userText :: Text.Text)
                    , "assistantText" Aeson..= (Nothing :: Maybe Text.Text)
                    , "error" Aeson..= (Nothing :: Maybe Text.Text)
                    , "responseId" Aeson..= (Nothing :: Maybe Text.Text)
                    , "items" Aeson..= ([] :: [ResponseItem])
                    , "usage" Aeson..= (Nothing :: Maybe TokenUsage)
                    ]
                effect userText =
                    fmap
                        (.turnEffect)
                        (Hermes.decodeEither sessionTurnDecoder
                            (LBS.toStrict (Aeson.encode (legacy userText))))
            effect "/compact focus"
                `shouldBe` Right TranscriptReplace
            effect "/rewind" `shouldBe` Right TranscriptReset

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
    , metaPromptSnapshot = Nothing
    }

testPromptSnapshot :: Text.Text -> SessionPromptSnapshot
testPromptSnapshot sessionId = SessionPromptSnapshot
    { promptSnapshotVersion = 1
    , promptSnapshotCreatedAt = fixedTime
    , promptSnapshotProvider = XAIProvider
    , promptSnapshotConnection = "xai"
    , promptSnapshotModel = "grok-4"
    , promptSnapshotDialect = GrokBuildDialect
    , promptSnapshotCwd = fromFilePath "/tmp/work"
    , promptSnapshotInstructions = "persisted instructions"
    , promptSnapshotTools = []
    , promptSnapshotGeneratedContext = Just "project and skill context"
    , promptSnapshotGrokContext = Just "grok first-turn context"
    , promptSnapshotCacheKey = sessionId
    }

promptFunctionTool :: Text.Text -> Text.Text -> ResponseTool
promptFunctionTool toolName documentation =
    FunctionToolValue FunctionTool
        { name = toolName
        , description = Just documentation
        , parameters = Nothing
        , strict = Just True
        }

fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2026 8 19) (secondsToDiffTime 0)

sampleTurnTelemetry :: TurnTelemetry
sampleTurnTelemetry = TurnTelemetry
    { telemetryDurationMs = Just 1250
    , telemetryApiDurationMs = Just 1100
    , telemetryCostUsd = Just 0.0125
    , telemetryStopReason = Just "end_turn"
    , telemetryProviderTurns = Just 2
    , telemetryModels = mempty
    , telemetryStructuredOutput = Nothing
    }

modeOf :: OsPath -> IO Integer
modeOf path = do
    status <- getFileStatus (toFilePath path)
    pure (fromIntegral (fileMode status `mod` 0o1000))

withTempSessionRoot :: (OsPath -> IO a) -> IO a
withTempSessionRoot action = do
    tmp <- Directory.getTemporaryDirectory
    bracket
        (mkdtemp (tmp FilePath.</> "agent-session-temp-XXXXXX"))
        Directory.removeDirectoryRecursive
        \basePath -> do
            let root =
                    fromFilePath
                        (basePath
                            FilePath.</> ".haskell-agent"
                            FilePath.</> "sessions")
            Directory.createDirectoryIfMissing True (toFilePath root)
            action root

addSessionTemp :: OsPath -> String -> IO OsPath
addSessionTemp root sessionId = do
    let path =
            sessionTempsRoot root
                </> fromFilePath sessionId
    Directory.createDirectoryIfMissing True (toFilePath path)
    pure path

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
