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

    it "prompts only for page interactions that can submit data" do
        let approvals =
                [ (tool.appToolName, tool.appToolApproval)
                | tool <- browserTools successHandler
                ]
        lookup "browser_snapshot" approvals `shouldSatisfyApproval`
            isAlwaysReadOnly
        lookup "browser_navigate" approvals `shouldSatisfyApproval`
            isAlwaysReadOnly
        lookup "browser_click" approvals `shouldSatisfyApproval`
            isAlwaysPrompt
        lookup "browser_type" approvals `shouldSatisfyApproval`
            isAlwaysPrompt
        lookup "browser_key" approvals `shouldSatisfyApproval`
            isAlwaysPrompt
        lookup "browser_scroll" approvals `shouldSatisfyApproval`
            isAlwaysReadOnly
        lookup "browser_back" approvals `shouldSatisfyApproval`
            isAlwaysReadOnly
        lookup "browser_forward" approvals `shouldSatisfyApproval`
            isAlwaysReadOnly
        lookup "browser_reload" approvals `shouldSatisfyApproval`
            isAlwaysReadOnly

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
            "{\"delta_x\":100001,\"delta_y\":1}"
            "between -100000 and 100000"
        shouldFailWith "browser_scroll"
            "{\"delta_x\":\"down\",\"delta_y\":1}"
            "Expected number for key: delta_x"
        shouldFailWith "browser_scroll"
            "{\"delta_x\":1e999,\"delta_y\":1}"
            "delta_x must be a finite number"

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
