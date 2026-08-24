module Agent.GrokBuild.WebLspSpec (spec) where

import Agent.GrokBuild.Dialect.Lsp (lspTool)
import Agent.GrokBuild.Dialect.WebFetch (webFetchTool)
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , jsonToolParameters
    )
import Data.Maybe (fromMaybe)
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

expectReadOnly :: ApprovalRule -> Expectation
expectReadOnly = \case
    AlwaysReadOnly -> pure ()
    _ -> expectationFailure "expected read-only tool approval"
