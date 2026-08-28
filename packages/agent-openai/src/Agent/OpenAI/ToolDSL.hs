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
    , parametersObject
    , parametersObjectLoose
    )
import Data.Text (Text)
import Agent.Json (rawJsonFromEncoding)
import qualified Data.Aeson as Aeson

-- | Build a strict Structured Outputs function tool.
--
-- OpenAI requires every declared property to occur in the @required@ array
-- when strict function calling is enabled. Application-optional properties are
-- therefore encoded as required-but-nullable. This is recursive, including
-- fields of nested objects.
buildTool :: Text -> Text -> [PropertySchema] -> ResponseTool
buildTool name description properties = FunctionToolValue FunctionTool
    { name
    , description = Just description
    , parameters = Just
        (rawJsonFromEncoding (Aeson.toEncoding (parametersObject properties)))
    , strict = Just True
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
    }
