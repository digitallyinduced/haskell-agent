module Agent.CLI.ActiveAccountSpec (spec) where

import Agent.CLI.ActiveAccount
import Agent.Provider
    ( BillingMode(..)
    , Credential(..)
    , Provider(..)
    , getNextToken
    , tokenProvider
    )
import Control.Concurrent.Async (cancel, wait, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception.Safe (throwIO)
import Test.Hspec

spec :: Spec
spec = describe "active account snapshots" do
    it "publishes the identity and label together after resolving the label" do
        ref <- newActiveAccount original
        started <- newEmptyMVar
        finish <- newEmptyMVar
        let tracked = trackCredentialAccount ref
                (\_ -> putMVar started () >> takeMVar finish)
                (tokenProvider SubscriptionBilled (\_ -> pure (Right replacement)))
        withAsync (getNextToken tracked Nothing) \worker -> do
            takeMVar started
            readActiveAccount ref `shouldReturn` original
            putMVar finish "New account"
            wait worker `shouldReturn` Right replacement
        readActiveAccount ref `shouldReturn`
            ActiveAccount "new-id" "new-id" "New account"

    it "retains the entire previous account when label resolution throws" do
        ref <- newActiveAccount original
        let tracked = trackCredentialAccount ref
                (\_ -> throwIO (userError "label unavailable"))
                (tokenProvider SubscriptionBilled (\_ -> pure (Right replacement)))
        getNextToken tracked Nothing `shouldThrow` anyIOException
        readActiveAccount ref `shouldReturn` original

    it "retains the entire previous account when label resolution is cancelled" do
        ref <- newActiveAccount original
        started <- newEmptyMVar
        finish <- newEmptyMVar
        let tracked = trackCredentialAccount ref
                (\_ -> putMVar started () >> takeMVar finish)
                (tokenProvider SubscriptionBilled (\_ -> pure (Right replacement)))
        withAsync (getNextToken tracked Nothing) \worker -> do
            takeMVar started
            cancel worker
        readActiveAccount ref `shouldReturn` original

    it "preserves a stable selection id when refreshing the same account" do
        ref <- newActiveAccount original
        let credential = replacement { accountId = original.activeAccountId }
            tracked = trackCredentialAccount ref (\_ -> pure "Updated label")
                (tokenProvider SubscriptionBilled (\_ -> pure (Right credential)))
        getNextToken tracked Nothing `shouldReturn` Right credential
        readActiveAccount ref `shouldReturn`
            original { activeAccountLabel = "Updated label" }

original :: ActiveAccount
original = ActiveAccount "old-id" "managed:source" "Old account"

replacement :: Credential
replacement = Credential
    { accessToken = "token"
    , accountId = "new-id"
    , leaseId = Nothing
    , provider = OpenAIProvider
    }
