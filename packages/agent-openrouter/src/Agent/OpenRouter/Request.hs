-- | Pure projection from canonical Responses API parameters to the dialect
-- accepted by OpenRouter's stateless Responses endpoint.
module Agent.OpenRouter.Request
    ( mapModel
    , buildRequest
    ) where

import Agent.Responses.Types
import Agent.OpenRouter.Options (ClientOptions(..))
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text

-- | Apply an exact model override, preserve an OpenRouter slug, or use the
-- configured default.
mapModel :: ClientOptions -> Text -> Text
mapModel options model = case lookup model options.modelOverrides of
    Just target -> target
    Nothing
        | "/" `Text.isInfixOf` model -> model
        | otherwise -> options.defaultModel

-- | Build the typed Responses request sent to OpenRouter.
--
-- OpenRouter is a drop-in Responses host except that it is stateless:
-- @store@ must be false and @previous_response_id@ is rejected. ChatGPT-only
-- computer-use tools are dropped; function tools and @web_search@ pass
-- through. Extra OpenRouter fields (provider routing, plugins) stay in
-- 'extraFields'.
buildRequest :: ClientOptions -> ResponseCreateParams -> ResponseCreateParams
buildRequest options request = request
    { model = Just (maybe options.defaultModel (mapModel options) request.model)
    , tools = Maybe.mapMaybe openRouterTool <$> request.tools
    , store = Just False
    , stream = Just True
    , previousResponseId = Nothing
    }

openRouterTool :: ResponseTool -> Maybe ResponseTool
openRouterTool tool = case tool of
    FunctionToolValue {} -> Just tool
    KnownResponseTool ToolWebSearch _ -> Just tool
    KnownResponseTool ToolComputer _ -> Nothing
    KnownResponseTool ToolComputerUsePreview _ -> Nothing
    _ -> Just tool
