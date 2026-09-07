module Agent.CLI.SessionRequestSpec (spec) where

import Agent.CLI.Session.Request
import Agent.CLI.Session.Types (Persistence(..))
import Agent.Provider (Provider(..))
import Agent.Responses.Types
import Data.IORef (newIORef)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "validated session requests" do
    it "rejects a missing model before a session starts" do
        result <- newSessionRequestState PersistenceDisabled defaultResponseCreateParams
        either Just (const Nothing) result
            `shouldBe` Just "provider request is missing a model"

    it "requires a cache key for persistent requests without accessing storage" do
        slot <- newIORef (error "request validation must not access persistence")
        result <- newSessionRequestState (PersistenceEnabled slot)
            defaultResponseCreateParams { model = Just "small" }
        either Just (const Nothing) result
            `shouldBe` Just "persistent provider request is missing a cache key"

    it "preserves persistent identity when optional settings change" do
        slot <- newIORef (error "request snapshot must not access persistence")
        state <- newSessionRequestState (PersistenceEnabled slot)
            defaultResponseCreateParams
                { model = Just "small", promptCacheKey = Just "session-key" }
            >>= either (fail . Text.unpack) pure
        modifySessionRequestOptions state $ \params ->
            params { model = Nothing, promptCacheKey = Nothing, serviceTier = Just "priority" }
        withPersistentSessionRequest state \actualSlot model cacheKey params -> do
            (actualSlot == slot) `shouldBe` True
            model `shouldBe` "small"
            cacheKey `shouldBe` "session-key"
            params.model `shouldBe` Just model
            params.promptCacheKey `shouldBe` Just cacheKey
            params.serviceTier `shouldBe` Just "priority"

    it "publishes model changes to both readers and transport projections" do
        state <- newSessionRequestState PersistenceDisabled
            defaultResponseCreateParams { model = Just "small", serviceTier = Just "priority" }
            >>= either (fail . Text.unpack) pure
        let readDisplayedModel = readSessionRequestModel state
        readDisplayedModel `shouldReturn` "small"
        setSessionRequestModel state OpenAIProvider "large"
        readDisplayedModel `shouldReturn` "large"
        params <- readSessionRequestParams state
        params.model `shouldBe` Just "large"
        params.serviceTier `shouldBe` Nothing

    it "preserves an ephemeral cache hint without enabling persistence" do
        state <- newSessionRequestState PersistenceDisabled
            defaultResponseCreateParams { model = Just "small", promptCacheKey = Just "hint" }
            >>= either (fail . Text.unpack) pure
        params <- readSessionRequestParams state
        params.promptCacheKey `shouldBe` Just "hint"
        withPersistentSessionRequest state \_ _ _ _ ->
            expectationFailure "ephemeral request attempted persistence"
