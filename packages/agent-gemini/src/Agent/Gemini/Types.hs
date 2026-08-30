-- | Minimal native Gemini response types used by the streaming adapter.
module Agent.Gemini.Types
    ( GenerateContentResponse(..)
    , Candidate(..)
    , Content(..)
    , Part(..)
    , NativeFunctionCall(..)
    , UsageMetadata(..)
    ) where

import Control.Applicative ((<|>))
import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser)
import Data.Text (Text)

data NativeFunctionCall = NativeFunctionCall
    { functionCallId :: !(Maybe Text)
    , functionCallName :: !Text
    , functionCallArgs :: !Value
    } deriving (Eq, Show)

instance FromJSON NativeFunctionCall where
    parseJSON = withObject "Gemini FunctionCall" \object ->
        NativeFunctionCall
            <$> object .:? "id"
            <*> object .: "name"
            <*> object .:? "args" .!= Object KeyMap.empty

data Part = Part
    { partText :: !(Maybe Text)
    , partThought :: !Bool
    , partThoughtSignature :: !(Maybe Text)
    , partFunctionCall :: !(Maybe NativeFunctionCall)
    } deriving (Eq, Show)

instance FromJSON Part where
    parseJSON = withObject "Gemini Part" \object ->
        Part
            <$> object .:? "text"
            <*> object .:? "thought" .!= False
            <*> object .:? "thoughtSignature"
            <*> object .:? "functionCall"

data Content = Content
    { contentRole :: !(Maybe Text)
    , contentParts :: ![Part]
    } deriving (Eq, Show)

instance FromJSON Content where
    parseJSON = withObject "Gemini Content" \object ->
        Content
            <$> object .:? "role"
            <*> object .:? "parts" .!= []

data Candidate = Candidate
    { candidateContent :: !(Maybe Content)
    , candidateFinishReason :: !(Maybe Text)
    , candidateIndex :: !(Maybe Int)
    } deriving (Eq, Show)

instance FromJSON Candidate where
    parseJSON = withObject "Gemini Candidate" \object ->
        Candidate
            <$> object .:? "content"
            <*> object .:? "finishReason"
            <*> object .:? "index"

data UsageMetadata = UsageMetadata
    { promptTokenCount :: !(Maybe Int)
    , candidatesTokenCount :: !(Maybe Int)
    , cachedContentTokenCount :: !(Maybe Int)
    , thoughtsTokenCount :: !(Maybe Int)
    , totalTokenCount :: !(Maybe Int)
    } deriving (Eq, Show)

instance FromJSON UsageMetadata where
    parseJSON = withObject "Gemini UsageMetadata" \object ->
        UsageMetadata
            <$> object .:? "promptTokenCount"
            <*> object .:? "candidatesTokenCount"
            <*> object .:? "cachedContentTokenCount"
            <*> object .:? "thoughtsTokenCount"
            <*> object .:? "totalTokenCount"

data GenerateContentResponse = GenerateContentResponse
    { nativeResponseId :: !(Maybe Text)
    , nativeModelVersion :: !(Maybe Text)
    , nativeCandidates :: ![Candidate]
    , nativeUsageMetadata :: !(Maybe UsageMetadata)
    , nativePromptBlockReason :: !(Maybe Text)
    } deriving (Eq, Show)

instance FromJSON GenerateContentResponse where
    parseJSON = withObject "Gemini GenerateContentResponse" \object -> do
        traceId <- object .:? "traceId"
        case KeyMap.lookup "response" object of
            Just Null -> pure (emptyCodeAssistResponse traceId)
            Just responseValue -> do
                response <- parseJSON responseValue
                pure response
                    { nativeResponseId =
                        response.nativeResponseId <|> traceId
                    }
            Nothing
                | any (`KeyMap.member` object)
                    ["traceId", "consumedCredits", "remainingCredits"] ->
                    pure (emptyCodeAssistResponse traceId)
                | otherwise -> parseNativeResponse object

emptyCodeAssistResponse :: Maybe Text -> GenerateContentResponse
emptyCodeAssistResponse responseId = GenerateContentResponse
    { nativeResponseId = responseId
    , nativeModelVersion = Nothing
    , nativeCandidates = []
    , nativeUsageMetadata = Nothing
    , nativePromptBlockReason = Nothing
    }

parseNativeResponse :: Object -> Parser GenerateContentResponse
parseNativeResponse object = do
    if any (`KeyMap.member` object)
        [ "responseId"
        , "modelVersion"
        , "candidates"
        , "usageMetadata"
        , "promptFeedback"
        ]
        then pure ()
        else fail "Gemini stream object has no response fields"
    promptFeedback <- object .:? "promptFeedback" :: Parser (Maybe Value)
    blockReason <- case promptFeedback of
        Just (Object feedback) -> feedback .:? "blockReason"
        _ -> pure Nothing
    GenerateContentResponse
        <$> object .:? "responseId"
        <*> object .:? "modelVersion"
        <*> object .:? "candidates" .!= []
        <*> object .:? "usageMetadata"
        <*> pure blockReason
