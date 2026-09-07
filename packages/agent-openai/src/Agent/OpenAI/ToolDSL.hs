{-# LANGUAGE ImplicitPrelude #-}

module Agent.OpenAI.ToolDSL
    ( buildTool
    , buildGrokTool
    , PropertySchema(..)
    , PropertyType(..)
    ) where

import Agent.Responses.Types (FunctionTool(..), ResponseTool(..))
import Agent.ToolDSL
    ( PropertySchema(..)
    , PropertyType(..)
    , parametersObjectLoose
    )
import Data.Text (Text)
import Agent.Json (rawJsonFromEncoding)
import qualified Data.Aeson as Aeson

-- | Build a non-strict OpenAI function tool. Optional properties remain
-- genuinely optional rather than required-but-nullable.
buildTool :: Text -> Text -> [PropertySchema] -> ResponseTool
buildTool name description properties = FunctionToolValue FunctionTool
    { name
    , description = Just description
    , parameters = Just
        (rawJsonFromEncoding
            (Aeson.toEncoding (parametersObjectLoose properties)))
    , strict = Just False
    , async = Nothing
    }

-- | grok-build function tool: optional fields stay optional, @strict@ omitted.
buildGrokTool :: Text -> Text -> [PropertySchema] -> ResponseTool
buildGrokTool name description properties = FunctionToolValue FunctionTool
    { name
    , description = Just description
    , parameters = Just
        (rawJsonFromEncoding
            (Aeson.toEncoding (parametersObjectLoose properties)))
    , strict = Nothing
    , async = Nothing
    }
