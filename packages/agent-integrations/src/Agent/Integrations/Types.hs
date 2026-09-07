{-# LANGUAGE ExistentialQuantification #-}

-- | Typed contracts used by in-memory integrations.
--
-- Model input is necessarily JSON, but it is decoded at the registry boundary.
-- Integration handlers only receive their concrete Haskell input type and
-- return their concrete output type.
module Agent.Integrations.Types
    ( IntegrationId
    , integrationId
    , integrationIdText
    , IntegrationToolName
    , integrationToolName
    , integrationToolNameText
    , IntegrationEffect(..)
    , IntegrationError(..)
    , InputContract(..)
    , OutputContract(..)
    , jsonInputContract
    , jsonOutputContract
    , aesonInputContract
    , aesonOutputContract
    , IntegrationTool(..)
    , SomeIntegrationTool(..)
    , IntegrationModule(..)
    , decodeIntegrationInput
    , encodeIntegrationOutput
    ) where

import Agent.Json
    ( RawJson
    , rawJsonBytes
    , rawJsonFromEncoding
    )
import qualified Agent.Json.Decode as Json
import Data.Aeson (Encoding, FromJSON, ToJSON, Value)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text

newtype IntegrationId = IntegrationId Text
    deriving (Eq, Ord, Show)

integrationId :: Text -> Either Text IntegrationId
integrationId raw
    | validIdentifier checked = Right (IntegrationId checked)
    | otherwise = Left "integration id must be a non-empty lowercase identifier"
  where
    checked = Text.strip raw

integrationIdText :: IntegrationId -> Text
integrationIdText (IntegrationId value) = value

newtype IntegrationToolName = IntegrationToolName Text
    deriving (Eq, Ord, Show)

integrationToolName :: Text -> Either Text IntegrationToolName
integrationToolName raw
    | validIdentifier checked = Right (IntegrationToolName checked)
    | otherwise = Left "integration tool name must be a non-empty lowercase identifier"
  where
    checked = Text.strip raw

integrationToolNameText :: IntegrationToolName -> Text
integrationToolNameText (IntegrationToolName value) = value

validIdentifier :: Text -> Bool
validIdentifier value =
    not (Text.null value)
        && Text.all
            (\character ->
                character == '_'
                    || character == '-'
                    || character >= 'a' && character <= 'z'
                    || character >= '0' && character <= '9')
            value

-- | One source of truth for MCP annotations and host approval policy.
data IntegrationEffect
    = IntegrationReadOnly
    | IntegrationMutation
    | IntegrationFreshApproval
    deriving (Eq, Show)

-- | Sanitized integration failure. Constructors distinguish malformed caller
-- input from runtime failure without carrying provider exception text.
data IntegrationError
    = IntegrationInvalidInput !Text
    | IntegrationUnavailable !Text
    | IntegrationOperationFailed !Text
    deriving (Eq, Show)

data InputContract input = InputContract
    { inputContractSchema :: !RawJson
    , inputContractDecoder :: !(Json.Decoder input)
    }

data OutputContract output = OutputContract
    { outputContractSchema :: !RawJson
    , outputContractEncoder :: !(output -> Encoding)
    }

jsonInputContract :: Value -> Json.Decoder input -> InputContract input
jsonInputContract schema decoder = InputContract
    { inputContractSchema = rawJsonFromEncoding (Aeson.toEncoding schema)
    , inputContractDecoder = decoder
    }

jsonOutputContract :: Value -> (output -> Encoding) -> OutputContract output
jsonOutputContract schema encoder = OutputContract
    { outputContractSchema = rawJsonFromEncoding (Aeson.toEncoding schema)
    , outputContractEncoder = encoder
    }

aesonInputContract
    :: FromJSON input
    => Value
    -> InputContract input
aesonInputContract schema =
    jsonInputContract schema $
        Json.withOwnedRawJson \bytes ->
            case Aeson.eitherDecodeStrict' bytes of
                Left _ -> fail "invalid integration tool arguments"
                Right value -> pure value

aesonOutputContract
    :: ToJSON output
    => Value
    -> OutputContract output
aesonOutputContract schema =
    jsonOutputContract schema Aeson.toEncoding

data IntegrationTool input output = IntegrationTool
    { integrationToolNameValue :: !IntegrationToolName
    , integrationToolDescription :: !Text
    , integrationToolInput :: !(InputContract input)
    , integrationToolOutput :: !(OutputContract output)
    , integrationToolEffect :: !IntegrationEffect
    , integrationToolDestructive :: !Bool
    , integrationToolIdempotent :: !Bool
    , integrationToolOpenWorld :: !Bool
    , integrationToolMaximumOutputBytes :: !Int
    , integrationToolHandler
        :: !(input -> IO (Either IntegrationError output))
    }

data SomeIntegrationTool =
    forall input output. SomeIntegrationTool (IntegrationTool input output)

data IntegrationModule = IntegrationModule
    { integrationModuleId :: !IntegrationId
    , integrationModuleInstructions :: !Text
    , integrationModuleTools :: !(IO [SomeIntegrationTool])
    }

decodeIntegrationInput
    :: InputContract input
    -> RawJson
    -> Either IntegrationError input
decodeIntegrationInput contract raw =
    case Json.decodeEither contract.inputContractDecoder (rawJsonBytes raw) of
        Left _ ->
            Left
                (IntegrationInvalidInput
                    "The integration tool arguments are invalid.")
        Right value -> Right value

encodeIntegrationOutput :: OutputContract output -> output -> RawJson
encodeIntegrationOutput contract =
    rawJsonFromEncoding . contract.outputContractEncoder
