module Agent.CLI.UsageLimitRecoverySpec (spec) where

import Agent.CLI.UsageLimitRecovery
    ( maximumAutomaticUsageLimitWait
    , minimumAutomaticUsageLimitWait
    , usageLimitResumeAt
    , usageLimitResumeMessage
    , usageLimitResumeTimeText
    , usageLimitWaitText
    )
import Agent.Error
    ( ApiError(..)
    , ErrorType(..)
    , credentialsExhausted
    )
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), addUTCTime)
import Data.Time.LocalTime (hoursToTimeZone, utc)
import Test.Hspec

spec :: Spec
spec = do
    -- Tuesday 2026-09-15 13:05 UTC.
    let now = UTCTime (fromGregorian 2026 9 15) (13 * 3600 + 5 * 60)

    describe "usageLimitResumeAt" do
        it "waits for a long credential cooldown until its reset deadline" do
            usageLimitResumeAt now (credentialsExhausted (addUTCTime 7200 now))
                `shouldBe` Just (addUTCTime 7200 now)

        it "uses the provider retry-after of a direct usage limit" do
            usageLimitResumeAt now
                (ProviderError UsageLimitReached "exhausted" (Just 3600))
                `shouldBe` Just (addUTCTime 3600 now)

        it "uses an explicit rate-limit retry-after" do
            usageLimitResumeAt now
                (ProviderError RateLimitError "slow down" (Just 300))
                `shouldBe` Just (addUTCTime 300 now)

        it "never resumes sooner than the minimum wait" do
            usageLimitResumeAt now (credentialsExhausted (addUTCTime (-5) now))
                `shouldBe` Just (addUTCTime minimumAutomaticUsageLimitWait now)
            usageLimitResumeAt now
                (ProviderError UsageLimitReached "exhausted" (Just 1))
                `shouldBe` Just (addUTCTime minimumAutomaticUsageLimitWait now)

        it "declines a usage limit without a reset deadline" do
            usageLimitResumeAt now
                (ProviderError UsageLimitReached "exhausted" Nothing)
                `shouldBe` Nothing
            usageLimitResumeAt now
                (ProviderError RateLimitError "slow down" Nothing)
                `shouldBe` Nothing

        it "declines a reset further away than the maximum wait" do
            usageLimitResumeAt now
                (credentialsExhausted
                    (addUTCTime (maximumAutomaticUsageLimitWait + 1) now))
                `shouldBe` Nothing
            usageLimitResumeAt now
                (credentialsExhausted
                    (addUTCTime maximumAutomaticUsageLimitWait now))
                `shouldBe`
                    Just (addUTCTime maximumAutomaticUsageLimitWait now)

        it "declines failures that are not a usage-window exhaustion" do
            map (usageLimitResumeAt now)
                [ ProviderError AuthenticationError "expired" (Just 60)
                , ProviderError OverloadedError "busy" (Just 30)
                , ProviderError UsageBalanceExhausted "no credits" (Just 60)
                , ProviderError QuotaExceeded "no quota" (Just 60)
                , HttpError 429 "too many requests"
                , ConnectionError "connection reset"
                ]
                `shouldBe` replicate 6 Nothing

    describe "usageLimitResumeTimeText" do
        let zone = hoursToTimeZone 2

        it "shows only the wall-clock time on the same local day" do
            usageLimitResumeTimeText zone now (addUTCTime 7200 now)
                `shouldBe` "17:05"

        it "adds the calendar day when the deadline is on another day" do
            usageLimitResumeTimeText zone now (addUTCTime (14 * 3600) now)
                `shouldBe` "Wed Sep 16 05:05"

        it "decides the day change in the local zone" do
            -- 22:30 UTC on Tuesday is already Wednesday at UTC+2.
            let evening = UTCTime (fromGregorian 2026 9 15) (22 * 3600 + 30 * 60)
            usageLimitResumeTimeText utc evening (addUTCTime 1800 evening)
                `shouldBe` "23:00"
            usageLimitResumeTimeText zone evening (addUTCTime 1800 evening)
                `shouldBe` "01:00"

    describe "usageLimitWaitText" do
        it "shows the local resume time and the coarse remaining duration" do
            usageLimitWaitText "17:05" 7980
                `shouldBe`
                    "Usage limit reached; resuming automatically at 17:05 \
                    \(in 2h 13m) · Esc to cancel"

        it "never displays a negative remaining duration" do
            usageLimitWaitText "17:05" (-1)
                `shouldSatisfy` Text.isInfixOf "(in 0s)"

    describe "usageLimitResumeMessage" do
        it "tells the model why the previous turn stopped" do
            usageLimitResumeMessage
                `shouldBe`
                    "I hit my usage limit while you were working, but it has \
                    \reset now. Please continue from where you left off."
