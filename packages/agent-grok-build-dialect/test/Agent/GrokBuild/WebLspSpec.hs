module Agent.GrokBuild.WebLspSpec (spec) where

import Agent.GrokBuild.Dialect.Lsp
    ( LspPosition(..)
    , LspPositionOperation(..)
    , LspRequest(..)
    , lspTool
    )
import Agent.GrokBuild.Dialect.WebFetch (webFetchTool)
import Agent.Loop (defaultLoopDispatch)
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , dispatchToolCall
    , functionToolCall
    )
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , jsonToolParameters
    )
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "Grok Build web_fetch/lsp contracts" do
    it "matches the upstream web_fetch wire shape" do
        let tool = webFetchTool (const (pure (Right "ok")))
            parameters = fromMaybe [] (jsonToolParameters tool)
        tool.appToolName `shouldBe` "web_fetch"
        map (.propertyName) parameters `shouldBe` ["url"]
        map (.propertyType) parameters `shouldBe` [PropertyString]
        map (.required) parameters `shouldBe` [True]
        expectReadOnly tool.appToolApproval

    it "matches the upstream lsp operations and snake_case fields" do
        let tool = lspTool (const (pure (Right "ok")))
            parameters = fromMaybe [] (jsonToolParameters tool)
        tool.appToolName `shouldBe` "lsp"
        map (.propertyName) parameters
            `shouldBe`
                [ "operation"
                , "file_path"
                , "line"
                , "character"
                , "query"
                ]
        case parameters of
            operation : _ ->
                operation.propertyType
                    `shouldBe`
                        PropertyEnum
                            [ "goToDefinition"
                            , "findReferences"
                            , "hover"
                            , "goToImplementation"
                            , "documentSymbol"
                            , "workspaceSymbol"
                            ]
            [] -> expectationFailure "missing lsp operation parameter"
        expectReadOnly tool.appToolApproval

    describe "lsp request decoding" do
        it "normalizes every valid operation into its required fields" do
            let position operation expected =
                    dispatchLspRequest
                        ( "{\"operation\":\"" <> operation
                            <> "\",\"file_path\":\"/tmp/Test.hs\","
                            <> "\"line\":2,\"character\":3}"
                        )
                        `shouldReturn`
                            Right
                                (LspAtPosition
                                    expected
                                    "/tmp/Test.hs"
                                    (LspPosition 2 3))
            position "goToDefinition" LspGoToDefinition
            position "findReferences" LspFindReferences
            position "hover" LspHover
            position "goToImplementation" LspGoToImplementation
            dispatchLspRequest
                "{\"operation\":\"documentSymbol\",\
                \\"file_path\":\"/tmp/Test.hs\"}"
                `shouldReturn`
                    Right (LspDocumentSymbols "/tmp/Test.hs")
            dispatchLspRequest
                "{\"operation\":\"workspaceSymbol\",\"query\":\"  Result  \"}"
                `shouldReturn`
                    Right (LspWorkspaceSymbols "Result")

        it "rejects operation-specific missing or invalid fields" do
            expectRejected
                "{\"operation\":\"documentSymbol\"}"
                "documentSymbol requires file_path"
            expectRejected
                "{\"operation\":\"hover\",\"line\":0,\"character\":0}"
                "hover requires file_path"
            expectRejected
                "{\"operation\":\"hover\",\"file_path\":\"/tmp/Test.hs\"}"
                "hover requires non-negative line and character"
            expectRejected
                "{\"operation\":\"goToDefinition\",\
                \\"file_path\":\"/tmp/Test.hs\",\"line\":0,\"character\":-1}"
                "goToDefinition requires non-negative line and character"
            expectRejected
                "{\"operation\":\"workspaceSymbol\"}"
                "workspaceSymbol requires query"
            expectRejected
                "{\"operation\":\"workspaceSymbol\",\"query\":\"  \"}"
                "workspaceSymbol requires a non-empty query"

expectReadOnly :: ApprovalRule -> Expectation
expectReadOnly = \case
    AlwaysReadOnly -> pure ()
    _ -> expectationFailure "expected read-only tool approval"

dispatchLspRequest :: Text -> IO (Either Text LspRequest)
dispatchLspRequest arguments = do
    decoded <- newIORef Nothing
    let tool = lspTool \request -> do
            writeIORef decoded (Just request)
            pure (Right "ok")
    result <-
        dispatchToolCall defaultLoopDispatch [tool.appToolHandler] $
            functionToolCall "lsp-call" "lsp" arguments
    readIORef decoded >>= \case
        Just request -> pure (Right request)
        Nothing -> pure (Left result.output)

expectRejected :: Text -> Text -> Expectation
expectRejected arguments expected =
    dispatchLspRequest arguments >>= \case
        Left err ->
            err `shouldSatisfy` Text.isInfixOf expected
        Right request ->
            expectationFailure
                ("unexpected decoded request: " <> show request)
