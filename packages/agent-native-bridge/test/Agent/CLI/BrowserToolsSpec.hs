module Agent.CLI.BrowserToolsSpec (spec) where

import Agent.CLI.BrowserTools
    ( BrowserCommand(..)
    , browserTools
    )
import Agent.Loop (defaultLoopDispatch)
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , dispatchToolCall
    , functionToolCall
    )
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , appToolHandlers
    )
import Control.Monad (forM_)
import Data.IORef
    ( newIORef
    , readIORef
    , writeIORef
    )
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "native browser tools" do
    it "registers the complete stable tool surface" do
        let names = map (.appToolName) (browserTools successHandler)
        names `shouldBe`
            [ "browser_navigate"
            , "browser_snapshot"
            , "browser_click"
            , "browser_type"
            , "browser_key"
            , "browser_scroll"
            , "browser_back"
            , "browser_forward"
            , "browser_reload"
            ]

    it "maps typed arguments and submit flags to host commands" do
        observed <- newIORef Nothing
        let handler command = do
                writeIORef observed (Just command)
                pure (Right "host result")
            run name arguments = do
                result <- dispatchToolCall defaultLoopDispatch
                    (appToolHandlers (browserTools handler))
                    (functionToolCall "browser-call" name arguments)
                result.output `shouldBe` "host result"
                readIORef observed
        run "browser_navigate" "{\"url\":\"https://example.com\"}"
            `shouldReturn` Just (BrowserNavigate "https://example.com")
        run "browser_click" "{\"selector\":\"#continue\"}"
            `shouldReturn` Just (BrowserClick "#continue")
        run "browser_type"
            "{\"selector\":\"input[name=q]\",\"text\":\"haskell\",\"submit\":true}"
            `shouldReturn`
                Just (BrowserType "input[name=q]" "haskell" True)
        run "browser_type"
            "{\"selector\":\"textarea\",\"text\":\"draft\"}"
            `shouldReturn` Just (BrowserType "textarea" "draft" False)
        run "browser_key" "{\"key\":\"Enter\"}"
            `shouldReturn` Just (BrowserKey "Enter")
        run "browser_scroll" "{\"delta_x\":12.5,\"delta_y\":-800}"
            `shouldReturn` Just (BrowserScroll 12.5 (-800))

    it "routes no-argument commands and preserves host failures" do
        let handler = \case
                BrowserSnapshot -> pure (Left "browser unavailable")
                command -> pure (Right (showText command))
            run name = dispatchToolCall defaultLoopDispatch
                (appToolHandlers (browserTools handler))
                (functionToolCall "browser-call" name "{}")
        result <- run "browser_snapshot"
        result.output `shouldBe` "Error: browser unavailable"
        back <- run "browser_back"
        back.output `shouldBe` "BrowserBack"

    it "prompts for interactions and navigation that can send requests" do
        let approvals =
                [ (tool.appToolName, tool.appToolApproval)
                | tool <- browserTools successHandler
                ]
        lookup "browser_snapshot" approvals `shouldSatisfyApproval`
            isAlwaysReadOnly
        lookup "browser_navigate" approvals `shouldSatisfyApproval`
            isAlwaysPrompt
        lookup "browser_click" approvals `shouldSatisfyApproval`
            isAlwaysPrompt
        lookup "browser_type" approvals `shouldSatisfyApproval`
            isAlwaysPrompt
        lookup "browser_key" approvals `shouldSatisfyApproval`
            isAlwaysPrompt
        lookup "browser_scroll" approvals `shouldSatisfyApproval`
            isAlwaysReadOnly
        lookup "browser_back" approvals `shouldSatisfyApproval`
            isAlwaysPrompt
        lookup "browser_forward" approvals `shouldSatisfyApproval`
            isAlwaysPrompt
        lookup "browser_reload" approvals `shouldSatisfyApproval`
            isAlwaysPrompt

    it "rejects invalid URLs before invoking the host" do
        invoked <- newIORef False
        let handler _ = do
                writeIORef invoked True
                pure (Right "unexpected")
            run url = dispatchToolCall defaultLoopDispatch
                (appToolHandlers (browserTools handler))
                (functionToolCall "browser-call" "browser_navigate"
                    ("{\"url\":\"" <> url <> "\"}"))
        forM_
            [ "file:///tmp/private"
            , "javascript:alert(1)"
            , "https:relative"
            , "http://"
            , "http:///missing-host"
            , "https://#missing-host"
            , " https://example.com"
            ]
            \url -> do
                result <- run url
                result.output `shouldSatisfy`
                    Text.isInfixOf "absolute HTTP or HTTPS URL"
        readIORef invoked `shouldReturn` False

    it "accepts case-insensitive HTTP schemes with a nonempty host" do
        observed <- newIORef Nothing
        let handler command = do
                writeIORef observed (Just command)
                pure (Right "ok")
        result <- dispatchToolCall defaultLoopDispatch
            (appToolHandlers (browserTools handler))
            (functionToolCall "browser-call" "browser_navigate"
                "{\"url\":\"HTTPS://example.com/path\"}")
        result.output `shouldBe` "ok"
        readIORef observed `shouldReturn`
            Just (BrowserNavigate "HTTPS://example.com/path")

    it "rejects URL userinfo before invoking the host" do
        invoked <- newIORef False
        let handler _ = do
                writeIORef invoked True
                pure (Right "unexpected")
            run url = dispatchToolCall defaultLoopDispatch
                (appToolHandlers (browserTools handler))
                (functionToolCall "browser-call" "browser_navigate"
                    ("{\"url\":\"" <> url <> "\"}"))
        forM_
            [ "https://user@example.com/private"
            , "https://user:password@example.com/private"
            , "https://%75ser@example.com/private"
            ]
            \url -> do
                result <- run url
                result.output `shouldSatisfy`
                    Text.isInfixOf
                        "url must not contain embedded credentials"
        readIORef invoked `shouldReturn` False

    it "enforces the app-group UTF-8 byte limits exactly" do
        invocationCount <- newIORef (0 :: Int)
        let handler _ = do
                current <- readIORef invocationCount
                writeIORef invocationCount (current + 1)
                pure (Right "ok")
            run name arguments = dispatchToolCall defaultLoopDispatch
                (appToolHandlers (browserTools handler))
                (functionToolCall "browser-call" name arguments)
            shouldSucceed name arguments = do
                result <- run name arguments
                result.output `shouldBe` "ok"
            shouldFailWith name arguments message = do
                result <- run name arguments
                result.output `shouldSatisfy` Text.isInfixOf message
            urlPrefix = "https://example.com/"
            exactUrl =
                urlPrefix <> Text.replicate (8192 - Text.length urlPrefix) "a"
            exactSelector = Text.replicate 4096 "a"
            exactText = Text.replicate 32768 "λ"
            exactKey = Text.replicate 64 "λ"
        shouldSucceed "browser_navigate"
            ("{\"url\":\"" <> exactUrl <> "\"}")
        shouldSucceed "browser_click"
            ("{\"selector\":\"" <> exactSelector <> "\"}")
        shouldSucceed "browser_type"
            ("{\"selector\":\"#q\",\"text\":\"" <> exactText <> "\"}")
        shouldSucceed "browser_key"
            ("{\"key\":\"" <> exactKey <> "\"}")
        shouldSucceed "browser_scroll"
            "{\"delta_x\":10000,\"delta_y\":-10000}"
        readIORef invocationCount `shouldReturn` 5

        shouldFailWith "browser_navigate"
            ("{\"url\":\"" <> exactUrl <> "a\"}")
            "url must not exceed 8192 UTF-8 bytes"
        shouldFailWith "browser_click"
            ("{\"selector\":\"" <> exactSelector <> "a\"}")
            "selector must not exceed 4096 UTF-8 bytes"
        shouldFailWith "browser_type"
            ("{\"selector\":\"#q\",\"text\":\"" <> exactText <> "a\"}")
            "text must not exceed 65536 UTF-8 bytes"
        shouldFailWith "browser_key"
            ("{\"key\":\"" <> exactKey <> "a\"}")
            "key must not exceed 128 UTF-8 bytes"
        shouldFailWith "browser_scroll"
            "{\"delta_x\":10000.01,\"delta_y\":0}"
            "delta_x and delta_y must be between -10000 and 10000"
        readIORef invocationCount `shouldReturn` 5

    it "rejects empty selectors, empty keys, and invalid scroll deltas" do
        let run name arguments = dispatchToolCall defaultLoopDispatch
                (appToolHandlers (browserTools successHandler))
                (functionToolCall "browser-call" name arguments)
            shouldFailWith name arguments message = do
                result <- run name arguments
                result.output `shouldSatisfy` Text.isInfixOf message
        shouldFailWith "browser_click" "{\"selector\":\"  \"}"
            "selector must not be empty"
        shouldFailWith "browser_key" "{\"key\":\"\"}"
            "key must not be empty"
        shouldFailWith "browser_scroll"
            "{\"delta_x\":0,\"delta_y\":0}"
            "must be nonzero"
        shouldFailWith "browser_scroll"
            "{\"delta_x\":10001,\"delta_y\":1}"
            "between -10000 and 10000"
        shouldFailWith "browser_scroll"
            "{\"delta_x\":\"down\",\"delta_y\":1}"
            "INCORRECT_TYPE"
        shouldFailWith "browser_scroll"
            "{\"delta_x\":1e999,\"delta_y\":1}"
            "Problem while parsing a number"

successHandler :: BrowserCommand -> IO (Either Text Text)
successHandler _ = pure (Right "ok")

showText :: Show a => a -> Text
showText = Text.pack . show

shouldSatisfyApproval
    :: Maybe ApprovalRule
    -> (ApprovalRule -> Bool)
    -> Expectation
shouldSatisfyApproval (Just approval) predicate =
    predicate approval `shouldBe` True
shouldSatisfyApproval Nothing _ =
    expectationFailure "missing browser tool approval rule"

isAlwaysReadOnly :: ApprovalRule -> Bool
isAlwaysReadOnly AlwaysReadOnly = True
isAlwaysReadOnly _ = False

isAlwaysPrompt :: ApprovalRule -> Bool
isAlwaysPrompt AlwaysPrompt = True
isAlwaysPrompt _ = False
