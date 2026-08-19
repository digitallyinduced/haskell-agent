{-# LANGUAGE ImplicitPrelude #-}

module Agent.OpenAI.ToolDSL
    ( buildTool
    , PropertySchema(..)
    , PropertyType(..)
    ) where

import Agent.OpenAI.Responses.Types (FunctionTool(..), ResponseTool(..))
import Agent.ToolDSL (PropertySchema(..), PropertyType(..), parametersObject)
import Data.Text (Text)
import qualified Data.Aeson.KeyMap as KeyMap

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
    , parameters = Just (parametersObject properties)
    , strict = Just True
    , extraFields = KeyMap.empty
    }
