module Agent.Integrations.AdminSpec (spec) where

import Agent.Integrations.Admin
import Agent.Integrations.Email.OAuth
    ( closeMailOAuthRuntime
    , newMailOAuthRuntime
    )
import Agent.Integrations.Registry (createIntegrationRegistry)
import Agent.Integrations.Server
    ( closeIntegrationHost
    , newIntegrationHost
    )
import Agent.Integrations.Types
import Agent.Json
    ( rawJsonBytes
    , rawJsonFromEncoding
    )
import qualified Agent.Json.Decode as Json
import Control.Monad ((>=>))
import Control.Exception.Safe (bracket)
import qualified Data.Aeson as Aeson
import Data.IORef
import Test.Hspec

newtype IncrementInput = IncrementInput Int

newtype IncrementOutput = IncrementOutput Int

spec :: Spec
spec = describe "typed integration administration" do
    it "registers the complete email operation catalog" do
        registry <- either
            (expectationFailure . show >=> const (fail "registry setup failed"))
            pure
            (createIntegrationRegistry [])
        bracket
            (newIntegrationHost registry)
            closeIntegrationHost
            \host ->
                bracket
                    newMailOAuthRuntime
                    closeMailOAuthRuntime
                    \runtime ->
                        fmap length (emailAdminOperations runtime host)
                            `shouldBe` Right 8

    it "decodes and encodes only at the registry boundary" do
        observed <- newIORef Nothing
        operation <- incrementOperation observed
        registry <- case createIntegrationAdminRegistry [operation] of
            Left err -> expectationFailure (show err) >> fail "registry setup failed"
            Right value -> pure value
        result <- callIntegrationAdmin
            registry
            "example.increment"
            (rawJsonFromEncoding
                (Aeson.pairs ("value" Aeson..= (41 :: Int))))
        fmap rawJsonBytes result `shouldBe` Right "{\"value\":42}"
        readIORef observed `shouldReturn` Just 41

    it "rejects malformed and unknown operation names before dispatch" do
        observed <- newIORef Nothing
        operation <- incrementOperation observed
        registry <- either
            (expectationFailure . show >=> const (fail "registry setup failed"))
            pure
            (createIntegrationAdminRegistry [operation])
        let arguments = rawJsonFromEncoding (Aeson.toEncoding Aeson.Null)
        callIntegrationAdmin registry "Example.increment" arguments
            `shouldReturn`
                Left
                    (IntegrationInvalidInput
                        "The integration admin operation is unknown.")
        callIntegrationAdmin registry "example.missing" arguments
            `shouldReturn`
                Left
                    (IntegrationInvalidInput
                        "The integration admin operation is unknown.")
        readIORef observed `shouldReturn` Nothing

incrementOperation
    :: IORef (Maybe Int)
    -> IO SomeIntegrationAdminOperation
incrementOperation observed = do
    name <- either
        (expectationFailure . show >=> const (fail "invalid operation name"))
        pure
        (integrationAdminOperationName "example.increment")
    let schema = rawJsonFromEncoding (Aeson.toEncoding Aeson.Null)
    pure . SomeIntegrationAdminOperation $ IntegrationAdminOperation
        { integrationAdminOperationDefinition = IntegrationAdminDefinition
            { integrationAdminDefinitionName = name
            , integrationAdminDefinitionTitle = "Increment"
            , integrationAdminDefinitionDescription = "Increment a number."
            , integrationAdminDefinitionInputSchema = schema
            , integrationAdminDefinitionOutputSchema = schema
            , integrationAdminDefinitionSensitiveInputFields = []
            }
        , integrationAdminOperationInput =
            jsonInputContract Aeson.Null $
                Json.object
                    (IncrementInput <$> Json.atKey "value" Json.int)
        , integrationAdminOperationOutput =
            jsonOutputContract Aeson.Null \(IncrementOutput value) ->
                Aeson.pairs ("value" Aeson..= value)
        , integrationAdminOperationHandler = \(IncrementInput value) -> do
            writeIORef observed (Just value)
            pure (Right (IncrementOutput (value + 1)))
        }
