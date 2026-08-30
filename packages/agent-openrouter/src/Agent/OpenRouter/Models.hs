-- | Small client for OpenRouter's public model catalog endpoint.
--
-- The catalog is deliberately kept separate from the Responses transport:
-- listing models is useful before a model has been configured, and the
-- endpoint is available with or without an API key.
module Agent.OpenRouter.Models
    ( OpenRouterModel(..)
    , decodeModels
    , fetchOpenRouterModels
    , fetchOpenRouterModelsWith
    , fetchOpenRouterModelsWithCredential
    ) where

import qualified Agent.Json.Decode as Json
import Agent.OpenRouter.Credential (credentialFromEnv)
import Agent.OpenRouter.Options
    ( ClientOptions(..)
    , clientOptionsFromEnv
    )
import Agent.Provider
    ( Credential(..)
    , Provider(..)
    )
import Control.Exception.Safe (tryAny)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Client as HttpClient
import Network.HTTP.Simple
import Network.HTTP.Types (HeaderName)

-- | The subset of OpenRouter model metadata needed by the interactive
-- selector.  OpenRouter's catalog has grown additional fields over time, so
-- unknown fields are intentionally ignored by the decoder below.
data OpenRouterModel = OpenRouterModel
    { modelId :: !Text
    , modelDisplayName :: !Text
    , modelContextLength :: !(Maybe Int)
    , modelCreated :: !(Maybe Int)
    , modelSupportsTools :: !Bool
    }
    deriving (Eq, Show)

data ModelsResponse = ModelsResponse
    { models :: ![OpenRouterModel]
    }

modelsResponseDecoder :: Json.Decoder ModelsResponse
modelsResponseDecoder = Json.object $
    ModelsResponse
        <$> Json.atKey "data" (Json.list modelDecoder)

modelDecoder :: Json.Decoder OpenRouterModel
modelDecoder = Json.object do
    modelId <- Json.atKey "id" Json.text
    modelName <- Json.optionalKey "name" Json.text
    modelContextLength <- Json.optionalKey "context_length" Json.int
    modelCreated <- Json.optionalKey "created" Json.int
    supportedParameters <- fromMaybe []
        <$> Json.optionalKey "supported_parameters" (Json.list Json.text)
    pure OpenRouterModel
        { modelId
        , modelDisplayName = displayName modelId modelName
        , modelContextLength
        , modelCreated
        , modelSupportsTools = "tools" `elem` supportedParameters
        }

displayName :: Text -> Maybe Text -> Text
displayName modelId = \case
    Just value
        | not (Text.null (Text.strip value)) -> value
    _ -> modelId

-- | Decode a successful @GET /models@ response.
decodeModels :: LBS.ByteString -> Either Text [OpenRouterModel]
decodeModels body =
    case Json.decodeEither modelsResponseDecoder (LBS.toStrict body) of
        Left _ -> Left "OpenRouter returned an unreadable models response."
        Right response -> Right (sortModels response.models)

-- | Keep the API's order for models with equal timestamps. Models without a
-- timestamp are retained after dated entries, also in their original order.
sortModels :: [OpenRouterModel] -> [OpenRouterModel]
sortModels = sortOn modelSortKey

modelSortKey :: OpenRouterModel -> (Bool, Down Int)
modelSortKey model = case model.modelCreated of
    Just value -> (False, Down value)
    Nothing -> (True, Down 0)

-- | Fetch OpenRouter's public model catalog using environment-derived options
-- and the optional @OPENROUTER_API_KEY@.
fetchOpenRouterModels :: IO (Either Text [OpenRouterModel])
fetchOpenRouterModels = do
    options <- clientOptionsFromEnv
    credential <- credentialFromEnv
    fetchOpenRouterModelsWithCredentialMaybe options credential

-- | Fetch the catalog using explicit transport options and an optional API
-- key.  OpenRouter permits this endpoint without authentication; a key is
-- still useful when the caller wants the request associated with its account.
fetchOpenRouterModelsWith
    :: ClientOptions
    -> Maybe Text
    -> IO (Either Text [OpenRouterModel])
fetchOpenRouterModelsWith options apiKey =
    fetchOpenRouterModelsAt options apiKey

-- | Fetch the catalog using an already-selected OpenRouter credential.
fetchOpenRouterModelsWithCredential
    :: ClientOptions
    -> Credential
    -> IO (Either Text [OpenRouterModel])
fetchOpenRouterModelsWithCredential options credential
    | credential.provider /= OpenRouterProvider =
        pure $ Left "OpenRouter model discovery requires an OpenRouter credential."
    | otherwise =
        fetchOpenRouterModelsAt options (Just credential.accessToken)

fetchOpenRouterModelsWithCredentialMaybe
    :: ClientOptions
    -> Maybe Credential
    -> IO (Either Text [OpenRouterModel])
fetchOpenRouterModelsWithCredentialMaybe options credential =
    case credential of
        Nothing -> fetchOpenRouterModelsAt options Nothing
        Just value -> fetchOpenRouterModelsWithCredential options value

fetchOpenRouterModelsAt
    :: ClientOptions
    -> Maybe Text
    -> IO (Either Text [OpenRouterModel])
fetchOpenRouterModelsAt options apiKey = do
    result <- tryAny requestModels
    case result of
        Left _ ->
            pure $ Left
                "Could not load OpenRouter models. Check your connection and retry."
        Right response -> do
            let status = getResponseStatusCode response
            pure $
                if status >= 200 && status < 300
                    then decodeModels (getResponseBody response)
                    else Left
                        ( "OpenRouter models returned HTTP "
                            <> Text.pack (show status)
                        )
  where
    requestModels = do
        baseRequest <- parseRequest options.baseUrl
        let endpointPath =
                BS8.dropWhileEnd (== '/') (HttpClient.path baseRequest)
                    <> "/models"
            request =
                setRequestPath endpointPath baseRequest
        httpLBS
            $ configureRequest apiKey
            $ setRequestResponseTimeout
                (HttpClient.responseTimeoutMicro (5 * 1_000_000))
            $ request

    configureRequest key =
        optionalAuthorization key
            . optionalHeader "HTTP-Referer" options.httpReferer
            . optionalHeader "X-Title" options.appTitle
            . setRequestHeader "User-Agent" ["haskell-agent"]
            . setRequestHeader "Accept" ["application/json"]

optionalAuthorization :: Maybe Text -> Request -> Request
optionalAuthorization key request = case nonEmptyText key of
    Just value ->
        setRequestHeader
            "Authorization"
            ["Bearer " <> Text.encodeUtf8 value]
            request
    Nothing -> request

optionalHeader :: HeaderName -> Maybe Text -> Request -> Request
optionalHeader name value request = case nonEmptyText value of
    Just text -> setRequestHeader name [Text.encodeUtf8 text] request
    Nothing -> request

nonEmptyText :: Maybe Text -> Maybe Text
nonEmptyText (Just value)
    | not (Text.null (Text.strip value)) = Just value
nonEmptyText _ = Nothing
