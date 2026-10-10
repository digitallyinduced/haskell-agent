module Agent.OpenAI.ToolCatalogSpec (spec) where

import Agent.OpenAI.ToolCatalog
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Either (isLeft)
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = describe "Responses Lite catalog transitions" do
    it "emits the complete initial catalog without an incremental notice" do
        current <- catalog [namespace "files" "Read files" [tool "read" "Read"]]
        diffToolCatalog Nothing current `shouldBe`
            ToolCatalogDelta [namespace "files" "Read files" [tool "read" "Read"]] [] [] False

    it "omits unchanged definitions" do
        current <- catalog [namespace "files" "" [tool "read" "Read"]]
        diffToolCatalog (Just current) current `shouldBe` ToolCatalogDelta [] [] [] False

    it "declares initially empty namespaces" do
        current <- catalog [namespace "files" "" []]
        diffToolCatalog Nothing current `shouldBe`
            ToolCatalogDelta [namespace "files" "" []] [] [] False

    it "declares newly added empty namespaces" do
        previous <- catalog []
        current <- catalog [namespace "files" "" []]
        diffToolCatalog (Just previous) current `shouldBe`
            ToolCatalogDelta [namespace "files" "" []] [] [] True

    it "replaces a function with an empty namespace of the same name" do
        previous <- catalog [tool "files" "Old"]
        current <- catalog [namespace "files" "" []]
        diffToolCatalog (Just previous) current `shouldBe`
            ToolCatalogDelta [namespace "files" "" []] [] [] True

    it "replaces a namespace with a function and removes its members" do
        previous <- catalog [namespace "files" "" [tool "read" "Read"]]
        current <- catalog [tool "files" "New"]
        diffToolCatalog (Just previous) current `shouldBe`
            ToolCatalogDelta [tool "files" "New"] ["files.read"] [] False

    it "sends a full declaration for structural namespace header changes" do
        let original = namespace "files" "" [tool "read" "Read"]
            extended = case original of
                Object fields -> Object (KeyMap.insert "extension" (Bool True) fields)
                _ -> original
        previous <- catalog [original]
        current <- catalog [extended]
        diffToolCatalog (Just previous) current `shouldBe`
            ToolCatalogDelta [extended] [] [] True

    it "ignores JSON object key order and catalog member order" do
        previous <- catalog [tool "read" "Read", tool "write" "Write"]
        current <- catalog
            [ tool "write" "Write"
            , object ["description" .= ("Read" :: Text), "name" .= ("read" :: Text), "type" .= ("function" :: Text)]
            ]
        diffToolCatalog (Just previous) current `shouldBe` ToolCatalogDelta [] [] [] False

    it "preserves current declaration order within each emitted batch" do
        previous <- catalog []
        current <- catalog [tool "write" "Write", tool "read" "Read"]
        diffToolCatalog (Just previous) current `shouldBe`
            ToolCatalogDelta [tool "write" "Write", tool "read" "Read"] [] [] False

    it "sends only new and changed members and one namespace notice" do
        previous <- catalog [namespace "files" "" [tool "read" "Read", tool "list" "List"]]
        current <- catalog [namespace "files" "" [tool "read" "Read", tool "list" "List all", tool "write" "Write"]]
        diffToolCatalog (Just previous) current `shouldBe`
            ToolCatalogDelta
                [namespace "files" "" [tool "list" "List all", tool "write" "Write"]]
                [] [] True

    it "reports removed members without repeating unchanged definitions" do
        previous <- catalog [namespace "files" "" [tool "read" "Read", tool "write" "Write"]]
        current <- catalog [namespace "files" "" [tool "read" "Read"]]
        diffToolCatalog (Just previous) current `shouldBe`
            ToolCatalogDelta [] ["files.write"] [] False

    it "collapses a removed namespace and its members into one removal" do
        previous <- catalog [namespace "files" "" [tool "read" "Read", tool "write" "Write"]]
        current <- catalog []
        diffToolCatalog (Just previous) current `shouldBe`
            ToolCatalogDelta [] ["files"] [] False

    it "updates namespace instructions without repeating members" do
        previous <- catalog [namespace "files" "Old" [tool "read" "Read"]]
        current <- catalog [namespace "files" "New" [tool "read" "Read"]]
        diffToolCatalog (Just previous) current `shouldBe`
            ToolCatalogDelta [] [] [("files", "New")] False

    it "represents cleared namespace instructions explicitly" do
        previous <- catalog [namespace "files" "Old" [tool "read" "Read"]]
        current <- catalog [namespace "files" "" [tool "read" "Read"]]
        diffToolCatalog (Just previous) current `shouldBe`
            ToolCatalogDelta [] [] [("files", "")] False

    it "uses built-in type names and does not request namespace notices" do
        current <- catalog [object ["type" .= ("web_search" :: Text)]]
        previous <- catalog []
        diffToolCatalog (Just previous) current `shouldBe`
            ToolCatalogDelta [object ["type" .= ("web_search" :: Text)]] [] [] False

    it "rejects duplicate top-level names and members" do
        buildToolCatalog [tool "read" "", tool "read" ""] `shouldSatisfy` isLeft
        buildToolCatalog [namespace "files" "" [tool "read" "", tool "read" ""]]
            `shouldSatisfy` isLeft

    it "rejects nested namespaces and ambiguous qualified names" do
        buildToolCatalog [namespace "outer" "" [namespace "inner" "" []]]
            `shouldSatisfy` isLeft
        buildToolCatalog [tool "files.read" ""] `shouldSatisfy` isLeft

    it "re-adds a removed tool even when its definition is unchanged" do
        empty <- catalog []
        current <- catalog [tool "read" "Read"]
        diffToolCatalog (Just empty) current `shouldBe`
            ToolCatalogDelta [tool "read" "Read"] [] [] False

catalog :: [Value] -> IO ToolCatalog
catalog = either (fail . show) pure . buildToolCatalog

tool :: Text -> Text -> Value
tool name description = object
    ["type" .= ("function" :: Text), "name" .= name, "description" .= description]

namespace :: Text -> Text -> [Value] -> Value
namespace name description tools = object
    [ "type" .= ("namespace" :: Text)
    , "name" .= name
    , "description" .= description
    , "tools" .= tools
    ]
