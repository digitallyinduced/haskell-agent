-- | Compatibility, storage round-trips, and scratch-space lifecycle.
module Agent.CLI.SessionSpec.Compatibility (spec) where

import Agent.CLI.Session
import Agent.CLI.SessionSpec.Fixtures
import Agent.CLI.SessionSpec.ResponseItems
import Agent.CLI.SessionLock (acquireSessionLock, releaseSessionLock)
import Agent.CLI.Request (requestParams)
import Agent.CLI.Session.StoreCodec (fromStoredResponseItem, toStoredResponseItem)
import Agent.Dialect (DialectId(..))
import Agent.Provider (Provider(..))
import Agent.Responses.Types
import Agent.Store.SessionItem
import Control.Concurrent (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Concurrent.Async (concurrently, mapConcurrently)
import Control.Monad (replicateM)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory.OsPath
    (createDirectory, createDirectoryIfMissing, doesDirectoryExist)
import System.OsPath ((</>))
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)

spec :: Spec
spec = do
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
                asyncRegenerated =
                    requestParams
                        XAIProvider
                        "grok-4"
                        "new binary instructions"
                        [asyncPromptFunctionTool "lookup" "new documentation"]
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
                asyncRegenerated
                (fromFilePath "/tmp/work")
                (Just sessionId)
                `shouldBe` Nothing
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
                        , computerActions = [ClickAction 12 34 "left" []]
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

        it "persists local summary and checkpoint provenance markers" do
            let summaryItem = MessageItem ResponseMessage
                    { messageId = Nothing
                    , content = MessageContentParts
                        [OutputTextPart "summary" Nothing Nothing]
                    , role = RoleAssistant
                    , status = Nothing
                    , phase = Nothing
                    , passthrough = Just InternalChatMetadata
                        { turnId = Nothing
                        , createTime = Nothing
                        , contentItemKinds =
                            Just [localCompactionSummaryContentItemKind]
                        , executedToolCalls = Nothing
                        }
                    }
                items =
                    [ summaryItem
                    , compactionCheckpointOriginItem "xai"
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
                    , turnDisplayItems = []
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
                        , async = Just True
                        }
                    , FunctionCallOutputItem FunctionCallOutput
                        { localOutcome = Nothing
                        , itemId = Just "output-item"
                        , callId = "call-1"
                        , name = Nothing
                        , namespace = Nothing
                        , provider = Nothing
                        , output = rawJsonValue (Aeson.object
                            ["stdout" Aeson..= ("/tmp/project" :: Text.Text)])
                        , status = Just ItemCompleted
                        , async = Just True
                        }
                    , CustomToolCallItem CustomToolCall
                        { itemId = Nothing
                        , callId = "custom-1"
                        , name = "apply_patch"
                        , namespace = Nothing
                        , input = "*** Begin Patch"
                        , status = Nothing
                        , async = Just False
                        }
                    , CustomToolCallOutputItem CustomToolCallOutput
                        { localOutcome = Nothing
                        , itemId = Nothing
                        , callId = "custom-1"
                        , name = Just "apply_patch"
                        , output = rawJsonValue ("Done" :: Text.Text)
                        , status = Just ItemCompleted
                        , async = Nothing
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

        it "preserves async flags and treats absent stored flags as legacy" do
            let items =
                    concatMap asyncPersistenceItems
                        [Nothing, Just False, Just True]
                stored = map toStoredResponseItem items
            traverse fromStoredResponseItem stored `shouldBe` Right items
            case map toStoredResponseItem (asyncPersistenceItems Nothing) of
                [ StoredFunctionCallItem functionCall
                    , StoredFunctionCallOutputItem functionOutput
                    , StoredCustomToolCallItem customCall
                    , StoredCustomToolCallOutputItem customOutput
                    ] ->
                        map (.storedOpaqueObjectText)
                            [ functionCall.storedFunctionCallExtraFields
                            , functionOutput.storedFunctionCallOutputExtraFields
                            , customCall.storedCustomToolCallExtraFields
                            , customOutput.storedCustomToolCallOutputExtraFields
                            ]
                            `shouldBe`
                                [ "{}"
                                , "{\"name\":\"shell\"}"
                                , "{}"
                                , "{}"
                                ]
                unexpected ->
                    expectationFailure
                        ("unexpected stored async fixtures: " <> show unexpected)

        modifyMaxSuccess (const 500) $
            prop "round-trips generated response items through storage" $
                storedResponseItemRoundTrip

        modifyMaxSuccess (const 500) $
            prop "round-trips every generated response content part" $
                storedContentPartRoundTrip
