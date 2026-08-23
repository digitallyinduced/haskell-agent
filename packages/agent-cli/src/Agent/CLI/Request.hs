-- | Construction of provider-neutral Responses requests.
module Agent.CLI.Request
    ( requestParams
    , setRequestInstructions
    ) where

import Agent.Responses.Types
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)

-- | Codex requires @store = false@. Continuation still uses
-- @previous_response_id@, with the local transcript available for recovery.
requestParams
    :: Text
    -> Text
    -> [ResponseTool]
    -> Text
    -> ResponseCreateParams
requestParams modelName instructionText toolSchemas effort =
    case defaultResponseCreateParams of
        ResponseCreateParams{..} ->
            ResponseCreateParams
                { model = Just modelName
                , instructions = Just instructionText
                , tools = Just toolSchemas
                , reasoning = Just ReasoningConfig
                    { context = Nothing
                    , effort = Just effort
                    , generateSummary = Nothing
                    , reasoningMode = Nothing
                    , summary = Nothing
                    , extraFields = KeyMap.empty
                    }
                , store = Just False
                , ..
                }

setRequestInstructions :: Text -> ResponseCreateParams -> ResponseCreateParams
setRequestInstructions instructionText params =
    case params of
        ResponseCreateParams{..} ->
            ResponseCreateParams
                { instructions = Just instructionText
                , ..
                }
