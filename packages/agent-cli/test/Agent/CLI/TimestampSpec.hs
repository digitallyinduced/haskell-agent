module Agent.CLI.TimestampSpec (spec) where

import Agent.CLI.Timestamp
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Time.LocalTime (TimeZone(..))
import qualified Data.Text as Text
import Test.Hspec

-- Fixed offset used as a stand-in for CEST (+02:00) in pure tests.
cest :: TimeZone
cest = TimeZone (2 * 60) True "CEST"

utcSample :: UTCTime
utcSample = UTCTime (fromGregorian 2026 8 20) (secondsToDiffTime (14 * 3600 + 45 * 60))
-- 14:45 UTC -> 16:45 CEST

spec :: Spec
spec = do
    describe "renderMessageTimestamp" do
        it "formats local wall-clock with timezone name" do
            renderMessageTimestamp cest utcSample
                `shouldBe` "[2026-08-20 16:45 CEST]"

    describe "stampUserTextAt" do
        it "appends a trailing stamp" do
            stampUserTextAt cest utcSample "hello"
                `shouldBe` "hello [2026-08-20 16:45 CEST]"

        it "does not double-stamp" do
            let once = stampUserTextAt cest utcSample "hello"
            stampUserTextAt cest utcSample once `shouldBe` once

        it "stamps an empty message as metadata-only" do
            stampUserTextAt cest utcSample ""
                `shouldBe` "[2026-08-20 16:45 CEST]"

    describe "stripBracketedTimestamps" do
        it "removes a trailing stamp glued without a space" do
            stripBracketedTimestamps
                "I'll retry now.[2026-04-20 18:06 CEST]"
                `shouldBe` "I'll retry now."

        it "removes a trailing stamp with a space" do
            stripBracketedTimestamps
                "Ok then. [2026-04-20 18:08 CEST]"
                `shouldBe` "Ok then."

        it "collapses a stamp-only reply to empty" do
            stripBracketedTimestamps "[2026-04-20 20:02 CEST]" `shouldBe` ""

        it "leaves non-timestamp brackets alone" do
            stripBracketedTimestamps "see [Anlage 1] please"
                `shouldBe` "see [Anlage 1] please"
            stripBracketedTimestamps "Beleg vom [2026-04-20] liegt vor"
                `shouldBe` "Beleg vom [2026-04-20] liegt vor"

        it "removes leading and mid-string stamps" do
            stripBracketedTimestamps "[2026-04-20 18:06 CEST] Alles klar."
                `shouldBe` "Alles klar."
            stripBracketedTimestamps "Ja[2026-04-20 18:06 CEST], passt."
                `shouldBe` "Ja, passt."

    describe "timeContextGuidance" do
        it "teaches the model about stamps and forbids echoing them" do
            timeContextGuidance `shouldSatisfy` Text.isInfixOf "[YYYY-MM-DD HH:MM"
            timeContextGuidance `shouldSatisfy` Text.isInfixOf "Never include"
