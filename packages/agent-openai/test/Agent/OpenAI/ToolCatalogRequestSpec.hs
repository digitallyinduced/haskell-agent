module Agent.OpenAI.ToolCatalogRequestSpec (spec) where

import Agent.Json (rawJsonFromEncoding)
import Agent.Error (ErrorType(..))
import Agent.OpenAI.Error (mkOpenAIError)
import Agent.Loop
import Agent.OpenAI.Compaction (buildRemoteCompactionRequest)
import Agent.OpenAI.LoopBackend
import Agent.OpenAI.LoopBackendSpec.Fixtures hiding (withModel)
import Agent.OpenAI.ToolCatalog.Request
import Agent.Responses.Types
import qualified Data.Aeson as Aeson
import Data.IORef
import Data.List (isPrefixOf)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = describe "Responses Lite durable catalog requests" do
    it "commits a catalog once and sends only new input on unchanged continuations" do
        requests <- newIORef []
        let send request previous _ = do
                modifyIORef' requests (<> [(inputItems request, previous)])
                pure (Right (testResponse "r1" []))
            backend = openAiBackendWith send (pure (template [tool "read" "v1"]))
        Right first <- backend.submitTurn emptyBackendSnapshot Nothing [UserMessage "first"] (const (pure ()))
        first.backendState.backendProviderState `shouldSatisfy` isJust
        Right second <- backend.submitTurn first.backendState Nothing [UserMessage "second"] (const (pure ()))
        readIORef requests `shouldReturn`
            [ ([catalogItem [tool "read" "v1"]] <> user "first", Nothing)
            , (user "second", Just "r1")
            ]
        second.backendState.backendItems `shouldBe`
            [catalogItem [tool "read" "v1"]] <> user "first" <> user "second"

    it "appends changed definitions and batch removals without rewriting the history prefix" do
        initial <- prepare (template [tool "read" "v1", tool "write" "v1"]) emptyBackendSnapshot (user "first")
        let previous = committed initial
        next <- prepare (template [tool "read" "v2"]) previous (user "second")
        previous.backendItems `isPrefixOf` next.catalogCommittedInput `shouldBe` True
        case inputItems next.catalogDeltaRequest of
            [AdditionalToolsItemValue added, MessageItem removed, MessageItem _] -> do
                added.tools `shouldBe` [rawJsonFromEncoding (Aeson.toEncoding (tool "read" "v2"))]
                removed.content `shouldBe`
                    MessageContentText "The following tools or namespaces are no longer available: write."
            actual -> expectationFailure ("Unexpected delta: " <> show actual)
        -- Merely preparing a request does not advance the committed snapshot.
        retry <- prepare (template [tool "read" "v2"]) previous (user "second")
        inputItems retry.catalogDeltaRequest `shouldBe` inputItems next.catalogDeltaRequest

    it "replays the exact committed catalog history after reconnect" do
        initial <- prepare (template [tool "read" "v1"]) emptyBackendSnapshot (user "first")
        changed <- prepare (template [tool "read" "v2"]) (committed initial) (user "second")
        let snapshot = clearBackendContinuation (committed changed)
        resumed <- prepare (template [tool "read" "v2"]) snapshot (user "third")
        inputItems resumed.catalogDeltaRequest `shouldBe` user "third"
        inputItems resumed.catalogFullRequest `shouldBe` snapshot.backendItems <> user "third"

    it "rebases when base instructions are removed instead of retaining stale instructions" do
        let instructions = MessageItem ResponseMessage
                { messageId = Nothing, content = MessageContentText "Use terse answers."
                , role = RoleDeveloper, status = Nothing, phase = Nothing
                , passthrough = Just InternalChatMetadata
                    { turnId = Nothing, createTime = Nothing
                    , contentItemKinds = Just ["model.base_instructions"]
                    , executedToolCalls = Nothing
                    }
                }
            params :: ResponseCreateParams
            params = (template [tool "read" "v1"])
                { input = Just (ResponseInputItems [catalogItem [tool "read" "v1"], instructions]) }
        initial <- prepare params emptyBackendSnapshot (user "first")
        unchanged <- prepare params (committed initial) (user "second")
        inputItems unchanged.catalogDeltaRequest `shouldBe` user "second"
        removed <- prepare (template [tool "read" "v1"]) (committed unchanged) (user "third")
        removed.catalogRequiresReplay `shouldBe` True
        inputItems removed.catalogFullRequest `shouldBe`
            [catalogItem [tool "read" "v1"]] <> user "first" <> user "second" <> user "third"

    it "resets the catalog after compaction and does not include obsolete catalog context in compaction" do
        initial <- prepare (template [tool "read" "v1"]) emptyBackendSnapshot (user "first")
        let snapshot = committed initial
            compacted = advanceBackendSnapshot snapshot (user "summary") Nothing
            compactRequest = buildRemoteCompactionRequest (template [tool "read" "v2"]) snapshot.backendItems
        compacted.backendProviderState `shouldBe` Nothing
        filter isCatalogContextItem (inputItems compactRequest) `shouldBe` [catalogItem [tool "read" "v2"]]
        resumed <- prepare (template [tool "read" "v2"]) compacted (user "continue")
        inputItems resumed.catalogFullRequest `shouldBe`
            [catalogItem [tool "read" "v2"]] <> user "summary" <> user "continue"
        resumed.catalogRequiresReplay `shouldBe` True

    it "rebases old or invalid metadata rather than trusting a continuation with unknown tools" do
        initial <- prepare (template [tool "read" "v1"]) emptyBackendSnapshot (user "first")
        let snapshot = (committed initial) { backendProviderState = Nothing }
        resumed <- prepare (template [tool "write" "v1"]) snapshot (user "next")
        filter isCatalogContextItem resumed.catalogCommittedInput `shouldBe` [catalogItem [tool "write" "v1"]]
        resumed.catalogRequiresReplay `shouldBe` True

    it "requires replay and fresh full definitions when the Lite model changes" do
        initial <- prepare (template [tool "read" "v1"]) emptyBackendSnapshot (user "first")
        let params = withModel "gpt-6-sol" (template [tool "read" "v1"])
        changed <- prepare params (committed initial) (user "next")
        changed.catalogRequiresReplay `shouldBe` True
        filter isCatalogContextItem changed.catalogCommittedInput `shouldBe` [catalogItem [tool "read" "v1"]]

    it "leaves ordinary Responses requests outside the incremental capability boundary" do
        prepareCatalogRequest baseParams emptyBackendSnapshot (user "hi") `shouldSatisfy` isNothing
        prepareCatalogRequest (withModel "gpt-4.1" (template [])) emptyBackendSnapshot (user "hi")
            `shouldSatisfy` isNothing

    it "does not publish candidate catalog state when submission fails" do
        initial <- prepare (template [tool "read" "v1"]) emptyBackendSnapshot (user "first")
        requests <- newIORef []
        let snapshot = committed initial
            send request _ _ = do
                modifyIORef' requests (<> [inputItems request])
                pure (Left (mkOpenAIError InvalidRequestError "rejected" Nothing (Just 400)))
            backend = openAiBackendWith send (pure (template [tool "read" "v2"]))
        first <- backend.submitTurn snapshot Nothing [UserMessage "next"] (const (pure ()))
        second <- backend.submitTurn snapshot Nothing [UserMessage "next"] (const (pure ()))
        first `shouldBe` second
        actual <- readIORef requests
        length actual `shouldBe` 2
        head actual `shouldBe` last actual
        filter isCatalogContextItem (head actual) `shouldBe` [catalogItem [tool "read" "v2"]]

    it "retains the accepted catalog on a native interrupted response" do
        let response = (testResponse "interrupted" [])
                { incompleteDetails = Just (IncompleteDetails "interrupted") }
            backend = openAiBackendWith (\_ _ _ -> pure (Right response))
                (pure (template [tool "read" "v1"]))
        Right result <- backend.submitTurn emptyBackendSnapshot Nothing [UserMessage "first"] (const (pure ()))
        resumed <- prepare (template [tool "read" "v1"]) result.backendState (user "steer")
        inputItems resumed.catalogDeltaRequest `shouldBe` user "steer"

    it "drops Lite-only context and continuation when switching to ordinary Responses" do
        initial <- prepare (template [tool "read" "v1"]) emptyBackendSnapshot (user "first")
        requests <- newIORef []
        let send request previous _ = do
                modifyIORef' requests (<> [(inputItems request, previous)])
                pure (Right (testResponse "standard" []))
            backend = openAiBackendWith send (pure baseParams)
        Right result <- backend.submitTurn (committed initial) Nothing [UserMessage "next"] (const (pure ()))
        readIORef requests `shouldReturn` [(user "first" <> user "next", Nothing)]
        result.backendState.backendProviderState `shouldBe` Nothing
        filter isCatalogContextItem result.backendState.backendItems `shouldBe` []

prepare :: ResponseCreateParams -> BackendSnapshot -> [ResponseItem] -> IO CatalogRequest
prepare params snapshot items =
    maybe (fail "Expected Lite catalog request") pure (prepareCatalogRequest params snapshot items)

committed :: CatalogRequest -> BackendSnapshot
committed request =
    (advanceBackendSnapshot emptyBackendSnapshot request.catalogCommittedInput
        (Just (BackendContinuation "openai.responses" "r1")))
        { backendProviderState = Just request.catalogCandidateState }

user :: Text -> [ResponseItem]
user text = turnInputsToItems [UserMessage text]

template :: [Aeson.Value] -> ResponseCreateParams
template definitions = case baseParams of
    ResponseCreateParams{..} -> ResponseCreateParams
        { model = Just "gpt-6-luna"
        , instructions = Nothing
        , input = Just (ResponseInputItems [catalogItem definitions])
        , ..
        }

withModel :: Text -> ResponseCreateParams -> ResponseCreateParams
withModel name ResponseCreateParams{..} = ResponseCreateParams { model = Just name, .. }

catalogItem :: [Aeson.Value] -> ResponseItem
catalogItem = AdditionalToolsItemValue . AdditionalToolsItem Nothing "developer"
    . map (rawJsonFromEncoding . Aeson.toEncoding)

tool :: Text -> Text -> Aeson.Value
tool name description = Aeson.object
    [ "type" Aeson..= ("function" :: Text), "name" Aeson..= name, "description" Aeson..= description ]
