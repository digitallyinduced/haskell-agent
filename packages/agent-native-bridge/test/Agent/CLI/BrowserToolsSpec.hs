module Agent.CLI.BrowserToolsSpec (spec) where

import Agent.CLI.BrowserTools
    ( BrowserCommand(..)
    , BrowserInvocation(..)
    , browserTools
    )
import Agent.Loop (defaultLoopDispatch)
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , ToolHandlerResult(..)
    , ToolResultImage(..)
    , dispatchToolCall
    , functionToolCall
    , toolCallResultImages
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
    it "registers the complete current tool surface" do
        let names = map (.appToolName)
                (browserTools "turn-id" successHandler)
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
            , "browser_screenshot"
            , "browser_list_tabs"
            , "browser_switch_tab"
            , "browser_list_downloads"
            ]

    it "passes scope, call ID, refs, tab IDs, and submit flags" do
        observed <- newIORef Nothing
        let handler invocation = do
                writeIORef observed (Just invocation)
                pure (success "host result")
            run name arguments = do
                result <- dispatchToolCall defaultLoopDispatch
                    (appToolHandlers (browserTools "turn-42" handler))
                    (functionToolCall "browser-call" name arguments)
                result.output `shouldBe` "host result"
                readIORef observed
            expected command = Just BrowserInvocation
                { browserScopeId = "turn-42"
                , browserCallId = "browser-call"
                , browserCommand = command
                }
        run "browser_navigate" "{\"url\":\"https://example.com\"}"
            `shouldReturn` expected
                (BrowserNavigate "https://example.com")
        run "browser_click" "{\"ref\":\"r-frame-generation-node\"}"
            `shouldReturn` expected
                (BrowserClick "r-frame-generation-node")
        run "browser_type"
            "{\"ref\":\"r-query\",\"text\":\"haskell\",\"submit\":true}"
            `shouldReturn` expected
                (BrowserType "r-query" "haskell" True)
        run "browser_type"
            "{\"ref\":\"r-draft\",\"text\":\"draft\"}"
            `shouldReturn` expected
                (BrowserType "r-draft" "draft" False)
        run "browser_switch_tab" "{\"tab_id\":\"tab-popup\"}"
            `shouldReturn` expected
                (BrowserSwitchTab "tab-popup")
        run "browser_key" "{\"key\":\"Enter\"}"
            `shouldReturn` expected (BrowserKey "Enter")
        run "browser_scroll" "{\"delta_x\":12.5,\"delta_y\":-800}"
            `shouldReturn` expected (BrowserScroll 12.5 (-800))

    it "routes all no-argument commands and preserves failures" do
        observed <- newIORef []
        let handler invocation = do
                let command = invocation.browserCommand
                previous <- readIORef observed
                writeIORef observed (previous <> [command])
                if command == BrowserSnapshot
                    then pure (Left "browser unavailable")
                    else pure (success (showText command))
            run name = dispatchToolCall defaultLoopDispatch
                (appToolHandlers (browserTools "turn" handler))
                (functionToolCall "call" name "{}")
        snapshot <- run "browser_snapshot"
        snapshot.output `shouldBe` "Error: browser unavailable"
        forM_
            [ ("browser_back", BrowserBack)
            , ("browser_forward", BrowserForward)
            , ("browser_reload", BrowserReload)
            , ("browser_screenshot", BrowserScreenshot)
            , ("browser_list_tabs", BrowserListTabs)
            , ("browser_list_downloads", BrowserListDownloads)
            ]
            \(name, command) -> do
                result <- run name
                result.output `shouldBe` showText command

    it "preserves rich screenshot images outside ordinary output" do
        let image = ToolResultImage
                { imageUrl = "data:image/png;base64,iVBORw0KGgo="
                , imageDetail = Just "high"
                }
            handler _ = pure (Right ToolHandlerResult
                { resultText = "Captured browser viewport."
                , resultImages = [image]
                })
        result <- dispatchToolCall defaultLoopDispatch
            (appToolHandlers (browserTools "turn" handler))
            (functionToolCall "shot" "browser_screenshot" "{}")
        result.output `shouldBe` "Captured browser viewport."
        toolCallResultImages result `shouldBe` [image]

    it "uses the intended approval rules" do
        let approvals =
                [ (tool.appToolName, tool.appToolApproval)
                | tool <- browserTools "turn" successHandler
                ]
            expect name predicate =
                lookup name approvals `shouldSatisfyApproval` predicate
        forM_
            [ "browser_snapshot"
            , "browser_scroll"
            , "browser_screenshot"
            , "browser_list_tabs"
            , "browser_switch_tab"
            , "browser_list_downloads"
            ]
            (`expect` isAlwaysReadOnly)
        forM_
            [ "browser_navigate"
            , "browser_click"
            , "browser_type"
            , "browser_key"
            , "browser_back"
            , "browser_forward"
            , "browser_reload"
            ]
            (`expect` isAlwaysPrompt)

    it "rejects invalid URLs before invoking the host" do
        invoked <- newIORef False
        let handler _ = do
                writeIORef invoked True
                pure (success "unexpected")
            run url = dispatchToolCall defaultLoopDispatch
                (appToolHandlers (browserTools "turn" handler))
                (functionToolCall "call" "browser_navigate"
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

    it "accepts case-insensitive HTTP schemes and rejects URL userinfo" do
        observed <- newIORef Nothing
        let handler invocation = do
                writeIORef observed (Just invocation.browserCommand)
                pure (success "ok")
            run url = dispatchToolCall defaultLoopDispatch
                (appToolHandlers (browserTools "turn" handler))
                (functionToolCall "call" "browser_navigate"
                    ("{\"url\":\"" <> url <> "\"}"))
        accepted <- run "HTTPS://example.com/path"
        accepted.output `shouldBe` "ok"
        readIORef observed `shouldReturn`
            Just (BrowserNavigate "HTTPS://example.com/path")
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

    it "enforces UTF-8 byte limits for refs, text, keys, and tab IDs" do
        invocationCount <- newIORef (0 :: Int)
        let handler _ = do
                current <- readIORef invocationCount
                writeIORef invocationCount (current + 1)
                pure (success "ok")
            run name arguments = dispatchToolCall defaultLoopDispatch
                (appToolHandlers (browserTools "turn" handler))
                (functionToolCall "call" name arguments)
            shouldSucceed name arguments = do
                result <- run name arguments
                result.output `shouldBe` "ok"
            shouldFailWith name arguments message = do
                result <- run name arguments
                result.output `shouldSatisfy` Text.isInfixOf message
            urlPrefix = "https://example.com/"
            exactUrl =
                urlPrefix <> Text.replicate (8192 - Text.length urlPrefix) "a"
            exactRef = Text.replicate 4096 "a"
            exactText = Text.replicate 32768 "λ"
            exactKey = Text.replicate 64 "λ"
            exactTab = Text.replicate 512 "t"
        shouldSucceed "browser_navigate"
            ("{\"url\":\"" <> exactUrl <> "\"}")
        shouldSucceed "browser_click"
            ("{\"ref\":\"" <> exactRef <> "\"}")
        shouldSucceed "browser_type"
            ("{\"ref\":\"r-q\",\"text\":\"" <> exactText <> "\"}")
        shouldSucceed "browser_key"
            ("{\"key\":\"" <> exactKey <> "\"}")
        shouldSucceed "browser_switch_tab"
            ("{\"tab_id\":\"" <> exactTab <> "\"}")
        shouldSucceed "browser_scroll"
            "{\"delta_x\":10000,\"delta_y\":-10000}"
        readIORef invocationCount `shouldReturn` 6

        shouldFailWith "browser_navigate"
            ("{\"url\":\"" <> exactUrl <> "a\"}")
            "url must not exceed 8192 UTF-8 bytes"
        shouldFailWith "browser_click"
            ("{\"ref\":\"" <> exactRef <> "a\"}")
            "ref must not exceed 4096 UTF-8 bytes"
        shouldFailWith "browser_type"
            ("{\"ref\":\"r-q\",\"text\":\"" <> exactText <> "a\"}")
            "text must not exceed 65536 UTF-8 bytes"
        shouldFailWith "browser_key"
            ("{\"key\":\"" <> exactKey <> "a\"}")
            "key must not exceed 128 UTF-8 bytes"
        shouldFailWith "browser_switch_tab"
            ("{\"tab_id\":\"" <> exactTab <> "a\"}")
            "tab_id must not exceed 512 UTF-8 bytes"
        shouldFailWith "browser_scroll"
            "{\"delta_x\":10000.01,\"delta_y\":0}"
            "delta_x and delta_y must be between -10000 and 10000"
        readIORef invocationCount `shouldReturn` 6

    it "rejects blank refs, keys, tab IDs, and invalid scroll deltas" do
        let run name arguments = dispatchToolCall defaultLoopDispatch
                (appToolHandlers (browserTools "turn" successHandler))
                (functionToolCall "call" name arguments)
            shouldFailWith name arguments message = do
                result <- run name arguments
                result.output `shouldSatisfy` Text.isInfixOf message
        shouldFailWith "browser_click" "{\"ref\":\"  \"}"
            "ref must not be empty"
        shouldFailWith "browser_key" "{\"key\":\"\"}"
            "key must not be empty"
        shouldFailWith "browser_switch_tab" "{\"tab_id\":\"  \"}"
            "tab_id must not be empty"
        shouldFailWith "browser_scroll"
            "{\"delta_x\":0,\"delta_y\":0}"
            "must be nonzero"
        shouldFailWith "browser_scroll"
            "{\"delta_x\":10001,\"delta_y\":1}"
            "between -10000 and 10000"

successHandler
    :: BrowserInvocation
    -> IO (Either Text ToolHandlerResult)
successHandler _ = pure (success "ok")

success :: Text -> Either Text ToolHandlerResult
success text = Right ToolHandlerResult
    { resultText = text
    , resultImages = []
    }

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
