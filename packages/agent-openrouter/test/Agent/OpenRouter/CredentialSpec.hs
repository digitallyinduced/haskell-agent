module Agent.OpenRouter.CredentialSpec (spec) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.OpenRouter.Credential
import Agent.Provider
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Test.Hspec

spec :: Spec
spec = describe "staticApiKeyProvider" do
    it "returns the same bearer without an account id" do
        let provider = staticApiKeyProvider "or-key"
        tokenProviderBillingMode provider `shouldBe` ApiBilled
        result <- getNextToken provider Nothing
        case result of
            Right credential -> do
                credential.accessToken `shouldBe` "or-key"
                credential.accountId `shouldBe` ""
                credential.leaseId `shouldBe` Nothing
                credential.provider `shouldBe` OpenRouterProvider
            Left err -> expectationFailure ("expected credential, got " <> show err)

    it "surfaces rate limits as CredentialsExhausted instead of cycling the same key" do
        now <- getCurrentTime
        let provider = staticApiKeyProvider "or-key"
        first <- expectCredential =<< getNextToken provider Nothing
        exhausted <- getNextToken provider
            (Just FailedCredential
                { credential = first
                , failure = AccountRateLimited (Just 90)
                })
        case exhausted of
            Left CredentialsExhausted { retryAt } ->
                retryAt `shouldSatisfy` (> addUTCTime 80 now)
            other -> expectationFailure ("expected CredentialsExhausted, got " <> show other)

    it "does not treat a rejected static key as recoverable" do
        let provider = staticApiKeyProvider "or-key"
        first <- expectCredential =<< getNextToken provider Nothing
        rejected <- getNextToken provider
            (Just FailedCredential
                { credential = first
                , failure = AccountAuthenticationRejected
                })
        case rejected of
            Left (ProviderError AuthenticationError _ _) -> pure ()
            other -> expectationFailure
                ("expected AuthenticationError, got " <> show other)

    it "builds an OpenRouter credential from an API key" do
        let credential = credentialFromApiKey "sk-or-test"
        credential.provider `shouldBe` OpenRouterProvider
        credential.accessToken `shouldBe` "sk-or-test"

expectCredential :: Either ApiError Credential -> IO Credential
expectCredential = \case
    Right credential -> pure credential
    Left err -> expectationFailure ("expected credential, got " <> show err) >> fail "unreachable"
