module Agent.Server.RepositoryCheckoutSpec (spec) where

import Data.List (isInfixOf)
import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Agent.Server.RepositoryCheckout
    ( credentialHelperScript
    , ghWrapperScript
    , validateDescriptor
    )
import Agent.Server.Types (RepositoryDescriptor(..))

spec :: Spec
spec =
    describe "repository checkout descriptors" do
        it "accepts a canonical GitHub repository with an HTTPS broker" do
            validDescriptor `shouldSatisfy` isValid

        it "rejects a clone URL that does not match the repository" do
            validDescriptor
                { repositoryCloneUrl = "https://github.com/other/repository.git"
                }
                `shouldSatisfy` isInvalid

        it "rejects broker URLs with userinfo or fragments" do
            validDescriptor
                { repositoryCredentialBrokerUrl = "https://gateway.example@attacker.example/token"
                }
                `shouldSatisfy` isInvalid
            validDescriptor
                { repositoryCredentialBrokerUrl = "https://gateway.example/token#ignored"
                }
                `shouldSatisfy` isInvalid

        it "uses the JSON lease broker contract without exposing the lease as bearer auth" do
            credentialHelperScript `shouldSatisfy` isInfixOf "'{lease: $lease}'"
            credentialHelperScript `shouldSatisfy` isInfixOf "Content-Type: application/json"
            credentialHelperScript `shouldSatisfy` isInfixOf ".password"
            credentialHelperScript `shouldSatisfy` (not . isInfixOf "Authorization: Bearer")
            ghWrapperScript `shouldSatisfy` isInfixOf "'{lease: $lease}'"
            ghWrapperScript `shouldSatisfy` isInfixOf ".password"

        it "rejects refs and broker URLs containing unsafe characters" do
            validDescriptor
                { repositoryDefaultBranch = "--upload-pack=malicious"
                }
                `shouldSatisfy` isInvalid
            validDescriptor
                { repositoryCredentialBrokerUrl = "https://gateway.example/token\nAuthorization: injected"
                }
                `shouldSatisfy` isInvalid

        it "allows loopback HTTP only for local brokers" do
            validDescriptor
                { repositoryCredentialBrokerUrl = "http://127.0.0.1:8080/token"
                }
                `shouldSatisfy` isValid
            validDescriptor
                { repositoryCredentialBrokerUrl = "http://gateway.example/token"
                }
                `shouldSatisfy` isInvalid

validDescriptor :: RepositoryDescriptor
validDescriptor =
    RepositoryDescriptor
        { repositoryFullName = "digitallyinduced/haskell-agent"
        , repositoryCloneUrl = "https://github.com/digitallyinduced/haskell-agent.git"
        , repositoryDefaultBranch = "main"
        , repositoryCredentialBrokerUrl = "https://gateway.example/api/v1/github/repository-token"
        , repositoryCredentialLease = "opaque-lease"
        }

isValid :: RepositoryDescriptor -> Bool
isValid = either (const False) (const True) . validateDescriptor

isInvalid :: RepositoryDescriptor -> Bool
isInvalid = not . isValid
