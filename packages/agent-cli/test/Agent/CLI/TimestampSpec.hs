module Agent.CLI.TimestampSpec (spec) where

import Agent.CLI.Timestamp
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), addUTCTime, secondsToDiffTime)
import Data.Time.LocalTime (TimeZone(..), minutesToTimeZone)
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
    describe "renderShortMessageTimestamp" do
        it "uses compact local clock labels" do
            renderShortMessageTimestamp
                (minutesToTimeZone 120)
                (UTCTime (fromGregorian 2026 8 24) (11 * 3600 + 53 * 60))
                `shouldBe` "1:53 PM"

    describe "renderMessageTimestamp" do
        it "formats local wall-clock with timezone name" do
            renderMessageTimestamp cest utcSample
                `shouldBe` "[2026-08-20 16:45 CEST]"

    describe "renderContextualMessageTimestamp" do
        it "omits the date on the conversation's start day" do
            renderContextualMessageTimestamp Hour12 cest utcSample utcSample
                `shouldBe` "[4:45pm CEST]"
            renderContextualMessageTimestamp Hour24 cest utcSample utcSample
                `shouldBe` "[16:45 CEST]"

        it "includes the date after the local day changes" do
            let nextDay =
                    UTCTime
                        (fromGregorian 2026 8 21)
                        (secondsToDiffTime (14 * 3600 + 45 * 60))
            renderContextualMessageTimestamp Hour12 cest utcSample nextDay
                `shouldBe` "[2026-08-21 4:45pm CEST]"

    describe "shouldShowMessageTimestamp" do
        it "only shows pauses longer than one minute" do
            shouldShowMessageTimestamp Nothing utcSample `shouldBe` False
            shouldShowMessageTimestamp
                (Just utcSample)
                (addUTCTime 60 utcSample)
                `shouldBe` False
            shouldShowMessageTimestamp
                (Just utcSample)
                (addUTCTime 61 utcSample)
                `shouldBe` True

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

        it "removes compact 12-hour and 24-hour stamps" do
            stripBracketedTimestamps "Okay [9:40pm EDT]"
                `shouldBe` "Okay"
            stripBracketedTimestamps "Okay [21:40 CEST]"
                `shouldBe` "Okay"
            stripBracketedTimestamps "Okay [2026-09-01 9:40pm EDT]"
                `shouldBe` "Okay"

    describe "timeContextGuidance" do
        it "teaches the model about stamps and forbids echoing them" do
            timeContextGuidance `shouldSatisfy` Text.isInfixOf "longer than one minute"
            timeContextGuidance `shouldSatisfy` Text.isInfixOf "Never include"
