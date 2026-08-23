-- | Pure helpers shared by stateless Responses request projections.
module Agent.Responses.Request
    ( forceStatelessStreaming
    , mapResponseTools
    , selectConfiguredModel
    , setResponseModel
    ) where

import Agent.Responses.Types
import qualified Data.Maybe as Maybe
import Data.Text (Text)

-- | Disable remote transcript state and require a streaming response.
forceStatelessStreaming
    :: ResponseCreateParams
    -> ResponseCreateParams
forceStatelessStreaming request = request
    { store = Just False
    , stream = Just True
    , previousResponseId = Nothing
    }

-- | Transform or discard tools without changing unrelated request fields.
mapResponseTools
    :: (ResponseTool -> Maybe ResponseTool)
    -> ResponseCreateParams
    -> ResponseCreateParams
mapResponseTools project ResponseCreateParams{..} =
    ResponseCreateParams
        { tools = Maybe.mapMaybe project <$> tools
        , ..
        }

-- | Set the request model without relying on ambiguous record updates.
setResponseModel
    :: Text
    -> ResponseCreateParams
    -> ResponseCreateParams
setResponseModel selected ResponseCreateParams{..} =
    ResponseCreateParams
        { model = Just selected
        , ..
        }

-- | Apply an exact override, preserve a provider-native model identifier, or
-- fall back to the configured default when no model was supplied.
selectConfiguredModel
    :: [(Text, Text)]
    -> (Text -> Bool)
    -> Text
    -> Maybe Text
    -> Text
selectConfiguredModel overrides isNative defaultModel = \case
    Nothing -> defaultModel
    Just model -> case lookup model overrides of
        Just target -> target
        Nothing
            | isNative model -> model
            | otherwise -> defaultModel
