module Agent.Runtime.LocalProtocolSpec (spec) where

import Agent.Json (rawJsonFromEncoding)
import Agent.Loop
import Agent.MCP
import Agent.Runtime.Request
import Agent.Runtime.Session.Events
import Agent.Runtime.Session.Interaction
import Agent.Runtime.Session.Settings
import Agent.Runtime.Session.SecretExchange
import Agent.ToolDispatch
import Control.Exception.Safe (bracket)
import Data.Aeson
import Data.Bits ((.&.))
import Data.Text qualified as Text
import System.Directory (doesFileExist, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))
import System.Posix.Files (fileMode, getFileStatus, createSymbolicLink)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

spec :: Spec
spec = describe "local session presentation and interaction protocol" do
    it "retains large tool output, images, structured results and asynchronous mode" do
        let result = ToolCallResultWithStructured "call" (Text.replicate 100000 "λ") FunctionCallKind
                AsyncToolCall [ToolResultImage "data:image/png;base64,AAAA" (Just "high")] Nothing
                (object ["path" .= ("result.json" :: Text.Text)]) []
        decodeDisplayEvent (encodeDisplayEvent (ToolFinished result)) `shouldBe` Right (ToolFinished result)

    it "never transmits encrypted tool arguments to the terminal" do
        let call = (functionToolCall "call" "tool" "private"){argumentsEncrypted = True}
            expected = ToolStarted call{arguments = ""}
        decodeDisplayEvent (encodeDisplayEvent (ToolStarted call)) `shouldBe` Right expected

    it "round-trips local policy settings and rejects an excessive agent cap" do
        let settings = defaultSessionSettings
                { sessionInteractionMode = NativePlan, sessionShellMode = NativeShellBoth
                , sessionOperation = HarnessCommand, sessionReadOnly = True, sessionMaxConcurrentAgents = Just 12
                , sessionAllowedTools = ["fixture"], sessionFast = True, sessionGoal = Just ("fixture", Just 100, False) }
        eitherDecode (encode settings) `shouldBe` Right settings
        (eitherDecode "{\"maxConcurrentAgents\":257}" :: Either String SessionSettings) `shouldSatisfy` either (const True) (const False)

    it "round-trips MCP forms and accepted replies without losing their schema" do
        let schema = rawJsonFromEncoding (toEncoding (object ["type" .= ("object" :: Text.Text)]))
            request = McpElicitRequest "fixture" "Choose a value" (McpElicitForm schema)
        case decodeLocalInput (encodeLocalInput (LocalElicitation request)) of
            Right (LocalElicitation restored) -> restored `shouldBe` request
            _ -> expectationFailure "elicitation request changed in transit"
        decodeElicitationReply (encodeElicitationReply (McpElicitAccept (Just schema)))
            `shouldBe` Right (McpElicitAccept (Just schema))

    it "exchanges secret input through a private file and removes it after use" do
        withDirectory \directory -> do
            path <- withSecretExchange directory \path -> do
                status <- getFileStatus path
                fileMode status .&. 0o077 `shouldBe` 0
                writeSecretExchange path "fixture value"
                readSecretExchange path `shouldReturn` "fixture value"
                writeSecretExchange path "short"
                readSecretExchange path `shouldReturn` "short"
                pure path
            doesFileExist path `shouldReturn` False

    it "rejects oversized UTF-8 input before writing the exchange file" do
        withDirectory \directory -> withSecretExchange directory \path -> do
            writeSecretExchange path (Text.replicate 600000 "λ") `shouldThrow` anyException
            readSecretExchange path `shouldReturn` ""

    it "does not follow a symbolic link when receiving secret input" do
        withDirectory \directory -> withSecretExchange directory \path -> do
            let link = directory </> "link"
            createSymbolicLink path link
            readSecretExchange link `shouldThrow` anyException

withDirectory :: (FilePath -> IO a) -> IO a
withDirectory action = do
    temporary <- getTemporaryDirectory
    bracket (mkdtemp (temporary </> "local-protocol-")) removePathForcibly action
