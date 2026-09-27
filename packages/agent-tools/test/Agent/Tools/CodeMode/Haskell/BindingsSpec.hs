module Agent.Tools.CodeMode.Haskell.BindingsSpec (spec) where

import Agent.Tools.CodeMode.Haskell.Bindings
import Control.Exception.Safe (bracket)
import Data.Aeson (Value(..), object, (.=))
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (findExecutable, getTemporaryDirectory, removePathForcibly)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Posix.Temp (mkdtemp)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "Haskell code-mode bindings" do
    it "normalizes punctuation, leading digits and reserved words" do
        normalizeHaskellIdentifier "list-issues" `shouldBe` "list_issues"
        normalizeHaskellIdentifier "3search" `shouldBe` "tool_3search"
        normalizeHaskellIdentifier "case" `shouldBe` "tool_case"
        normalizeHaskellIdentifier "" `shouldBe` "tool_"
        normalizeHaskellIdentifier "GitHub__issues" `shouldBe` "tool_GitHub__issues"

    it "rejects tool normalization collisions rather than replacing a binding" do
        generateHaskellBindings [("read-file", Nothing), ("read_file", Nothing)]
            `shouldSatisfy` isLeft
        generateHaskellBindings [("case", Nothing), ("tool_case", Nothing)]
            `shouldSatisfy` isLeft

    it "renders freeform calls as Text and preserves their original wire name" do
        bindings <- requireBindings [("apply-patch", Nothing)]
        bindings.bindingsDeclarations `shouldSatisfy`
            Text.isInfixOf "apply_patch :: (Text) -> IO Value"
        bindings.bindingsModuleSource `shouldSatisfy`
            Text.isInfixOf "CodeModeSupport.callTool \"apply-patch\""

    it "escapes external names rather than interpolating executable source" do
        bindings <- requireBindings [("line\n\"name", Nothing)]
        bindings.bindingsModuleSource `shouldSatisfy`
            Text.isInfixOf "CodeModeSupport.callTool \"line\\n\\\"name\""

    it "generates strict typed fields and omits optional absent properties" do
        bindings <- requireBindings [("search", Just $ object
            [ "type" .= String "object"
            , "required" .= (["query"] :: [Text])
            , "properties" .= object
                [ "query" .= typed "string"
                , "limit" .= typed "integer"
                ]
            ])]
        bindings.bindingsDeclarations `shouldSatisfy` Text.isInfixOf "query :: !(Text)"
        bindings.bindingsDeclarations `shouldSatisfy` Text.isInfixOf "limit :: !(Maybe (Integer))"
        bindings.bindingsModuleSource `shouldSatisfy`
            Text.isInfixOf "P.maybe [] (\\value -> [(\"limit\", Aeson.toJSON value)])"

    it "keeps absent and explicit null distinct" do
        bindings <- requireBindings [("update", Just $ object
            [ "type" .= String "object"
            , "properties" .= object
                [ "title" .= object ["type" .= (["string", "null"] :: [Text])]
                ]
            ])]
        bindings.bindingsDeclarations `shouldSatisfy`
            Text.isInfixOf "title :: !(Maybe (Maybe (Text)))"

    it "generates nested object and array types" do
        bindings <- requireBindings [("inspect", Just $ object
            [ "type" .= String "object"
            , "required" .= (["entries"] :: [Text])
            , "properties" .= object
                [ "entries" .= object
                    [ "type" .= String "array"
                    , "items" .= object
                        [ "type" .= String "object"
                        , "required" .= (["enabled"] :: [Text])
                        , "properties" .= object ["enabled" .= typed "boolean"]
                        ]
                    ]
                ]
            ])]
        bindings.bindingsDeclarations `shouldSatisfy`
            Text.isInfixOf "entries :: !([ToolArguments_inspect'Field0'Element])"
        bindings.bindingsDeclarations `shouldSatisfy`
            Text.isInfixOf "enabled :: !(Bool)"

    it "falls back to Value for unions and extensible object maps" do
        bindings <- requireBindings
            [ ("union", Just $ object ["oneOf" .= [typed "string", typed "integer"]])
            , ("mapping", Just $ object
                ["type" .= String "object", "additionalProperties" .= typed "string"])
            ]
        bindings.bindingsDeclarations `shouldSatisfy`
            Text.isInfixOf "union :: (Value) -> IO Value"
        bindings.bindingsDeclarations `shouldSatisfy`
            Text.isInfixOf "mapping :: (Value) -> IO Value"

    it "rejects field normalization collisions" do
        generateHaskellBindings [("inspect", Just $ object
            [ "type" .= String "object"
            , "properties" .= object
                ["field-name" .= typed "string", "field_name" .= typed "integer"]
            ])] `shouldSatisfy` isLeft

    it "does not confuse a nested type with another tool's argument type" do
        bindings <- requireBindings
            [ ("inspect", Just $ object
                [ "type" .= String "object"
                , "properties" .= object
                    ["nested" .= object ["type" .= String "object", "additionalProperties" .= False]]
                ])
            , ("inspect_Field0", Just $ object
                ["type" .= String "object", "additionalProperties" .= False])
            ]
        bindings.bindingsDeclarations `shouldSatisfy`
            Text.isInfixOf "data ToolArguments_inspect'Field0"
        bindings.bindingsDeclarations `shouldSatisfy`
            Text.isInfixOf "data ToolArguments_inspect_Field0"

    it "produces deterministic source independent of tool ordering" do
        generateHaskellBindings [("first", Nothing), ("second", Just (typed "integer"))]
            `shouldBe`
            generateHaskellBindings [("second", Just (typed "integer")), ("first", Nothing)]

    it "generates declared output records without changing untyped tools" do
        bindings <- requireOutputBindings
            [ ("read", Just (typed "integer"), Just nullableOutput)
            , ("unknown", Nothing, Nothing)
            ]
        bindings.bindingsDeclarations `shouldSatisfy`
            Text.isInfixOf "read :: (Integer) -> IO (ToolResult (ToolOutput_read))"
        bindings.bindingsDeclarations `shouldSatisfy`
            Text.isInfixOf "unknown :: (Text) -> IO Value"
        bindings.bindingsModuleSource `shouldSatisfy`
            Text.isInfixOf "instance Aeson.FromJSON ToolOutput_read"

    it "falls back to Value for unsupported output schemas" do
        bindings <- requireOutputBindings
            [ ("union", Nothing, Just $ object ["oneOf" .= [typed "string", typed "integer"]])
            , ("reference", Nothing, Just $ object ["$ref" .= String "#/$defs/value"])
            , ("mapping", Nothing, Just $ object
                ["type" .= String "object", "additionalProperties" .= typed "string"])
            ]
        mapM_ (\name -> bindings.bindingsDeclarations `shouldSatisfy`
            Text.isInfixOf (name <> " :: (Text) -> IO (ToolResult (Value))"))
            ["union", "reference", "mapping"]

    it "rejects output field collisions and escapes wire property names" do
        generateHaskellBindingsWithOutputs [("inspect", Nothing, Just $ object
            [ "type" .= String "object"
            , "properties" .= object
                ["field-name" .= typed "string", "field_name" .= typed "integer"]
            ])] `shouldSatisfy` isLeft
        bindings <- requireOutputBindings [("inspect", Nothing, Just $ object
            [ "type" .= String "object"
            , "required" .= (["line\n\"name"] :: [Text])
            , "properties" .= object ["line\n\"name" .= typed "string"]
            ])]
        bindings.bindingsModuleSource `shouldSatisfy`
            Text.isInfixOf "object Aeson..: \"line\\n\\\"name\""

    it "retains additional output properties without requiring typed fields for them" do
        let schema = object
                [ "type" .= String "object"
                , "required" .= (["count"] :: [Text])
                , "properties" .= object ["count" .= typed "integer"]
                , "additionalProperties" .= True
                ]
        bindings <- requireOutputBindings [("inspect", Just (object []), Just schema)]
        withBindingModule bindings \executable directory -> do
            result <- timeout 30000000 $ readProcessWithExitCode executable
                [ "-ignore-dot-ghci", "-v0", "-XOverloadedStrings"
                , "-i" <> directory, directory </> "Tools.hs"
                , "-e", "inspect (Aeson.object [(\"count\", Aeson.Number 3), (\"extra\", Aeson.String \"retained\")]) P.>>= P.print"
                ] ""
            case result of
                Just (ExitSuccess, output, "") -> do
                    output `shouldContain` "count = 3"
                    output `shouldContain` "\"retained\""
                other -> expectationFailure (show other)

    it "decodes missing, explicit null and present output fields distinctly in GHCi" do
        bindings <- requireOutputBindings [("inspect", Just (object []), Just nullableOutput)]
        withBindingModule bindings \executable directory -> do
            result <- timeout 30000000 $ readProcessWithExitCode executable
                [ "-ignore-dot-ghci", "-v0", "-XOverloadedStrings"
                , "-i" <> directory, directory </> "Tools.hs"
                , "-e", "P.mapM_ (\\v -> inspect v P.>>= P.print) [Aeson.object [], Aeson.object [(\"title\", Aeson.Null)], Aeson.object [(\"title\", Aeson.String \"present\")]]"
                ] ""
            case result of
                Just (ExitSuccess, output, "") -> do
                    output `shouldContain` "title = Nothing"
                    output `shouldContain` "title = Just Nothing"
                    output `shouldContain` "title = Just (Just \"present\")"
                other -> expectationFailure (show other)

    it "preserves invalid raw outputs and never repeats the underlying call" do
        bindings <- requireOutputBindings
            [("write", Just (object []), Just $ object
                [ "type" .= String "object"
                , "required" .= (["count"] :: [Text])
                , "properties" .= object ["count" .= typed "integer"]
                ])]
        withBindingModule bindings \executable directory -> do
            -- An observable callback records exactly one invocation; decoding
            -- is performed afterwards on the already returned value.
            Text.writeFile (directory </> "CodeModeSupport.hs") $ Text.unlines
                [ "module CodeModeSupport where"
                , "import Data.Aeson (Value)"
                , "import Data.Text (Text)"
                , "callTool :: Text -> Value -> IO Value"
                , "callTool _ value = putStrLn \"called\" >> pure value"
                ]
            result <- timeout 30000000 $ readProcessWithExitCode executable
                [ "-ignore-dot-ghci", "-v0", "-XOverloadedStrings"
                , "-i" <> directory, directory </> "Tools.hs"
                , "-e", "write (Aeson.object [(\"count\", Aeson.String \"invalid\"), (\"extra\", Aeson.Bool P.True)]) P.>>= \\(ToolResult raw decoded) -> P.print (raw, P.either (P.const P.True) (P.const P.False) decoded)"
                ] ""
            case result of
                Just (ExitSuccess, output, "") -> do
                    length (filter (== "called") (lines output)) `shouldBe` 1
                    output `shouldContain` "\"invalid\""
                    output `shouldContain` "\"extra\""
                    output `shouldContain` ",True)"
                other -> expectationFailure (show other)

    it "decodes nested arrays, required nullable fields and empty output records" do
        let item = object
                [ "type" .= String "object"
                , "required" .= (["label"] :: [Text])
                , "properties" .= object
                    ["label" .= object ["type" .= (["string", "null"] :: [Text])]]
                ]
            schema = object ["type" .= String "array", "items" .= item]
            emptyObject = object ["type" .= String "object", "additionalProperties" .= False]
        bindings <- requireOutputBindings
            [("list", Just (object []), Just schema), ("empty", Just (object []), Just emptyObject)]
        withBindingModule bindings \executable directory -> do
            result <- timeout 30000000 $ readProcessWithExitCode executable
                [ "-ignore-dot-ghci", "-v0", "-XOverloadedStrings"
                , "-i" <> directory, directory </> "Tools.hs"
                , "-e", "do { list (Aeson.toJSON [Aeson.object [(\"label\", Aeson.Null)]]) P.>>= P.print; empty (Aeson.object []) P.>>= P.print; list (Aeson.toJSON [Aeson.object []]) P.>>= \\(ToolResult _ decoded) -> P.print (P.either (P.const P.True) (P.const P.False) decoded) }"
                ] ""
            case result of
                Just (ExitSuccess, output, "") -> do
                    output `shouldContain` "label = Nothing"
                    output `shouldContain` "Right ToolOutput_empty"
                    lines output `shouldSatisfy` ((== "True") . last)
                other -> expectationFailure (show other)

    it "loads generated records in GHCi and encodes missing, null and present values distinctly" do
        withGeneratedBindings nullableSpecification \executable directory -> do
            result <- timeout 30000000 $ readProcessWithExitCode executable
                [ "-ignore-dot-ghci", "-v0", "-XOverloadedStrings"
                , "-i" <> directory, directory </> "Tools.hs"
                , "-e", "P.mapM_ (\\arguments -> update arguments P.>>= CodeModeSupport.output) [ToolArguments_update Nothing, ToolArguments_update (Just Nothing), ToolArguments_update (Just (Just \"specified\"))]"
                ] ""
            result `shouldBe` Just (ExitSuccess, "{}\n{\"title\":null}\n{\"title\":\"specified\"}\n", "")

    it "rejects omitted strict fields before execution" do
        withGeneratedBindings nullableSpecification \executable directory -> do
            result <- timeout 30000000 $ readProcessWithExitCode executable
                [ "-ignore-dot-ghci", "-v0", "-i" <> directory
                , directory </> "Tools.hs", "-e", "update ToolArguments_update{}"
                ] ""
            case result of
                Just (ExitFailure _, "", diagnostics) ->
                    Text.pack diagnostics `shouldSatisfy` Text.isInfixOf "title"
                other -> expectationFailure ("expected missing-field compilation failure, received " <> show other)

    it "compiles bindings named after imported types and implementation helpers" do
        let names =
                [ "Text", "Value", "Maybe", "IO", "Integer", "Bool", "Show", "Eq"
                , "Scientific", "toJSON", "concat", "maybe", "arguments"
                ]
            expression = "P.sequence_ ["
                <> Text.intercalate ", "
                    [ normalizeHaskellIdentifier name <> " \"value\" P.>>= CodeModeSupport.output"
                    | name <- names
                    ]
                <> "]"
        withGeneratedBindings [(name, Nothing) | name <- names] \executable directory -> do
            result <- timeout 30000000 $ readProcessWithExitCode executable
                [ "-ignore-dot-ghci", "-v0", "-XOverloadedStrings"
                , "-i" <> directory, directory </> "Tools.hs"
                , "-e", Text.unpack expression
                ] ""
            result `shouldBe` Just
                (ExitSuccess, concat (replicate (length names) "\"value\"\n"), "")

    it "compiles output fields and tool names matching result helpers and qualified imports" do
        let names = ["ToolResult", "rawResult", "decodedResult", "KeyMap", "Text"]
            outputSchema = object
                [ "type" .= String "object"
                , "required" .= (["rawResult", "decodedResult"] :: [Text])
                , "properties" .= object
                    ["rawResult" .= typed "string", "decodedResult" .= typed "integer"]
                ]
            expression = "P.sequence_ ["
                <> Text.intercalate ", "
                    [ normalizeHaskellIdentifier name
                        <> " (Aeson.object [(\"rawResult\", Aeson.String \"value\"), (\"decodedResult\", Aeson.Number 7)])"
                        <> " P.>>= \\result -> case result.decodedResult of"
                        <> " { P.Left message -> P.fail (Text.unpack message); P.Right decoded -> P.print (decoded.rawResult, decoded.decodedResult) }"
                    | name <- names
                    ]
                <> "]"
        bindings <- requireOutputBindings [(name, Just (object []), Just outputSchema) | name <- names]
        withBindingModule bindings \executable directory -> do
            result <- timeout 30000000 $ readProcessWithExitCode executable
                [ "-ignore-dot-ghci", "-v0", "-XOverloadedStrings", "-XOverloadedRecordDot"
                , "-i" <> directory, directory </> "Tools.hs"
                , "-e", Text.unpack expression
                ] ""
            result `shouldBe` Just
                (ExitSuccess, concat (replicate (length names) "(\"value\",7)\n"), "")

    it "compiles empty records and retains null or unknown schemas as Value" do
        let emptyObject = object
                ["type" .= String "object", "properties" .= object [], "additionalProperties" .= False]
            specification =
                [ ("empty", Just emptyObject)
                , ("implicit_empty", Just $ object ["type" .= String "object", "additionalProperties" .= False])
                , ("null_input", Just $ typed "null")
                , ("unknown", Just Null)
                , ("unspecified", Just $ object [])
                , ("nested", Just $ object
                    [ "type" .= String "object"
                    , "required" .= (["child"] :: [Text])
                    , "properties" .= object ["child" .= emptyObject]
                    ])
                ]
            expression = "P.sequence_ ["
                <> "empty ToolArguments_empty P.>>= CodeModeSupport.output, "
                <> "implicit_empty ToolArguments_implicit_empty P.>>= CodeModeSupport.output, "
                <> "null_input Aeson.Null P.>>= CodeModeSupport.output, "
                <> "unknown Aeson.Null P.>>= CodeModeSupport.output, "
                <> "unspecified (Aeson.object []) P.>>= CodeModeSupport.output, "
                <> "nested (ToolArguments_nested ToolArguments_nested'Field0) P.>>= CodeModeSupport.output]"
        withGeneratedBindings specification \executable directory -> do
            result <- timeout 30000000 $ readProcessWithExitCode executable
                [ "-ignore-dot-ghci", "-v0", "-i" <> directory
                , directory </> "Tools.hs", "-e", expression
                ] ""
            result `shouldBe` Just (ExitSuccess, "{}\n{}\nnull\nnull\n{}\n{\"child\":{}}\n", "")

typed :: Text -> Value
typed name = object ["type" .= name]

requireBindings :: [(Text, Maybe Value)] -> IO HaskellBindings
requireBindings specifications = case generateHaskellBindings specifications of
    Left message -> expectationFailure (Text.unpack message) >> fail "binding generation failed"
    Right bindings -> pure bindings

requireOutputBindings :: [(Text, Maybe Value, Maybe Value)] -> IO HaskellBindings
requireOutputBindings specifications = case generateHaskellBindingsWithOutputs specifications of
    Left message -> expectationFailure (Text.unpack message) >> fail "binding generation failed"
    Right bindings -> pure bindings

nullableOutput :: Value
nullableOutput = object
    [ "type" .= String "object"
    , "properties" .= object
        ["title" .= object ["type" .= (["string", "null"] :: [Text])]]
    ]

nullableSpecification :: [(Text, Maybe Value)]
nullableSpecification =
    [ ("update", Just $ object
        [ "type" .= String "object"
        , "properties" .= object
            ["title" .= object ["type" .= (["string", "null"] :: [Text])]]
        ])
    ]

withGeneratedBindings
    :: [(Text, Maybe Value)]
    -> (FilePath -> FilePath -> IO ())
    -> IO ()
withGeneratedBindings specifications action =
    requireBindings specifications >>= \bindings -> withBindingModule bindings action

withBindingModule :: HaskellBindings -> (FilePath -> FilePath -> IO ()) -> IO ()
withBindingModule bindings action = do
    executable <- findExecutable "ghci"
    case executable of
        Nothing -> pendingWith "GHCi is required for generated-binding integration tests"
        Just ghci -> do
            root <- getTemporaryDirectory
            bracket (mkdtemp (root </> "haskell-binding-tests-")) removePathForcibly \directory -> do
                Text.writeFile (directory </> "Tools.hs") bindings.bindingsModuleSource
                Text.writeFile (directory </> "CodeModeSupport.hs") $ Text.unlines
                    [ "module CodeModeSupport where"
                    , "import Data.Aeson (Value, encode)"
                    , "import Data.Text (Text)"
                    , "import qualified Data.ByteString.Lazy.Char8 as Bytes"
                    , "callTool :: Text -> Value -> IO Value"
                    , "callTool _ = pure"
                    , "output :: Value -> IO ()"
                    , "output = Bytes.putStrLn . encode"
                    ]
                action ghci directory
