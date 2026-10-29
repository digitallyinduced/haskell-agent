module Agent.Tools.MultiAgentsSuspensionSpec (spec) where

import Agent.Loop (LoopError(..), defaultLoopDispatch)
import Agent.Subagents
import Agent.ToolDispatch (ToolCall(..), ToolCallKind(..), ToolCallResult(..), dispatchToolCall)
import Agent.Tools.MultiAgents
import Agent.Tools.Types (appToolHandlers)
import Control.Exception.Safe (bracket)
import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime(..), fromGregorian)
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec

spec :: Spec
spec = describe "suspend_until deadlines" do
    forM_
        [ "2026-10-29T09:32:00Z"
        , "2026-10-29T09:32:00+00:00"
        , "2026-10-29T09:32:00-00:00"
        , "2026-10-29T09:32:00-0000"
        , "2026-10-29T10:32:00+01:00"
        , "2026-10-29T15:02:00+05:30"
        , "2026-10-29T04:02:00-05:30"
        , "2026-10-29T10:32:00+0100"
        , "2026-10-29T04:02:00-0530"
        , " 2026-10-29T09:32:00+00:00 "
        ] \timestamp ->
            it ("normalizes " <> Text.unpack timestamp <> " to UTC") $
                parseSuspensionDeadline timestamp `shouldBe` Just expected

    forM_ ["Z", "+00:00"] \zone ->
        it ("preserves fractional seconds with " <> Text.unpack zone) $
            parseSuspensionDeadline ("2026-10-29T09:32:00.123456" <> zone)
                `shouldBe` Just (expected { utctDayTime = 34320.123456 })

    it "normalizes an offset crossing the UTC date boundary" $
        parseSuspensionDeadline "2026-10-30T00:32:00+02:00"
            `shouldBe` Just (UTCTime (fromGregorian 2026 10 29) 81120)

    it "preserves fractional seconds while normalizing a nonzero offset" $
        parseSuspensionDeadline "2026-10-29T10:32:00.123456+01:00"
            `shouldBe` Just (expected { utctDayTime = 34320.123456 })

    forM_
        [ "2026-10-29T09:32:00"
        , "2026-10-29"
        , "2026-02-30T09:32:00+00:00"
        , "2026-10-29T09:32:00+00:00garbage"
        , "2026-10-29T09:32:00+24:00"
        , "2026-10-29T09:32:00+01:99"
        , "2026-10-29T09:32:00-0199"
        , "tomorrow"
        , ""
        ] \timestamp ->
            it ("rejects an invalid or unzoned deadline: " <> Text.unpack timestamp) $
                parseSuspensionDeadline timestamp `shouldBe` Nothing

    it "passes an offset-bearing deadline through tool dispatch to suspension admission" $
        withChildContext \context -> do
            result <- callSuspend context "2099-10-29T10:32:00+01:00"
            result.output `shouldBe` "Error: Durable suspension storage is not configured."

    it "retains the future-deadline guard for offset timestamps" $
        withChildContext \context -> do
            result <- callSuspend context "2000-10-29T10:32:00+01:00"
            result.output `shouldBe` "Error: Suspension deadline must be in the future."

    it "rejects an unzoned deadline before suspension admission" $
        withChildContext \context -> do
            result <- callSuspend context "2099-10-29T10:32:00"
            result.output `shouldBe` "Error: deadline must be an ISO-8601 timestamp with timezone."
  where
    expected = UTCTime (fromGregorian 2026 10 29) 34320

withChildContext :: (MultiAgentContext -> IO a) -> IO a
withChildContext action = bracket
    (newSubagentRegistry defaultSubagentConfig (unsafeEncodeUtf ".")
        (\_ _ _ _ -> pure (Left LoopNoResponseId)) (\_ _ -> pure ()))
    closeSubagentRegistry \registry -> do
        root <- newRootSubagents NoticesToLoop registry
        action (rootMultiAgentContext root (unsafeEncodeUtf "."))
            { multiSelfId = Just (SubagentId "deadline-test"), multiDepth = 1 }

callSuspend :: MultiAgentContext -> Text -> IO ToolCallResult
callSuspend context timestamp = dispatchToolCall defaultLoopDispatch
    (appToolHandlers (multiAgentTools context))
    (ToolCall "suspend" "suspend_until"
        ("{\"deadline\":\"" <> timestamp <> "\",\"reason\":\"Wait for provider\"}")
        FunctionCallKind False)
