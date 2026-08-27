{-# LANGUAGE TemplateHaskell #-}

-- | Access to the model catalog shipped with this package.
--
-- The catalog is embedded at compile time (mirroring Codex's
-- @include_str!("../models.json")@), so resolution cannot depend on install
-- layout, data-directory overrides, or the current working directory.
module Agent.OpenAI.Models.Bundled
    ( bundledModelsText
    , loadBundledModels
    , loadBundledModelsOrThrow
    ) where

import Agent.OpenAI.Models.Types (ModelsResponse)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Language.Haskell.TH.Syntax
    ( lift
    , makeRelativeToProject
    , qAddDependentFile
    , runIO
    )

-- | The raw @models.json@ bundled with this package.
bundledModelsText :: Text
bundledModelsText =
    Text.pack
        $(do
            path <- makeRelativeToProject "data/models.json"
            qAddDependentFile path
            contents <- runIO (readFile path)
            lift contents
         )

loadBundledModels :: IO (Either Text ModelsResponse)
loadBundledModels =
    pure $ case
        Aeson.eitherDecodeStrict'
            (TextEncoding.encodeUtf8 bundledModelsText)
    of
        Left err -> Left (Text.pack err)
        Right response -> Right response

loadBundledModelsOrThrow :: IO ModelsResponse
loadBundledModelsOrThrow =
    loadBundledModels >>= either
        (ioError . userError . ("unable to load bundled models.json: " <>) . Text.unpack)
        pure
