module Agent.Accounts.SelectionSpec (spec) where

import Agent.Accounts.Selection
import Agent.Provider (BillingMode(..), Provider(..))
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = describe "account ranking" do
    it "rejects unverifiable, exhausted, and negative capacity" do
        selectCandidates Nothing
            [candidate "unknown" Nothing, candidate "empty" (Just 0),
             candidate "negative" (Just (-1))]
            `shouldBe` Nothing
    it "prefers a usable remembered account to greater capacity" do
        let remembered = candidate "remembered" (Just 1)
        selectCandidates (Just ("selection-remembered", "remembered"))
            [candidate "larger" (Just 90), remembered]
            `shouldBe` Just remembered
    it "supports legacy account-id selection keys" do
        let remembered = candidate "remembered" (Just 1)
        selectCandidates (Just ("remembered", "remembered"))
            [candidate "larger" (Just 90), remembered]
            `shouldBe` Just remembered
    it "falls back when the remembered account is exhausted" do
        let usable = candidate "usable" (Just 90)
        selectCandidates (Just ("selection-empty", "empty"))
            [candidate "empty" (Just 0), usable]
            `shouldBe` Just usable
    it "preserves discovery order for equal capacity" do
        let first = candidate "first" (Just 20)
        selectCandidates Nothing [first, candidate "second" (Just 20)]
            `shouldBe` Just first

candidate :: Text -> Maybe Double -> AccountCandidate
candidate accountId capacity = AccountCandidate
    { candidateProvider = OpenAIProvider
    , candidateSelectionId = "selection-" <> accountId
    , candidateAccountId = accountId
    , candidateBillingMode = SubscriptionBilled
    , candidateLabel = accountId
    , candidateCapacity = capacity
    }
