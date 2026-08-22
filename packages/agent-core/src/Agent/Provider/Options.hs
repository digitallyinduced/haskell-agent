-- | Small environment and model-mapping helpers shared by provider clients.
module Agent.Provider.Options
    ( lookupNonEmptyEnv
    , lookupIntEnv
    , parseModelOverrides
    ) where

import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import System.Environment (lookupEnv)

-- | Read an environment variable, treating only the empty string as absent.
lookupNonEmptyEnv :: String -> IO (Maybe String)
lookupNonEmptyEnv name = nonEmpty <$> lookupEnv name
  where
    nonEmpty (Just value) | not (null value) = Just value
    nonEmpty _ = Nothing

-- | Read an integer environment variable. Invalid or empty values are absent.
lookupIntEnv :: String -> IO (Maybe Int)
lookupIntEnv name = do
    value <- lookupNonEmptyEnv name
    pure (value >>= parseInt)

-- | Parse an integer only when the entire input is consumed.
parseInt :: String -> Maybe Int
parseInt value = case reads value of
    [(number, "")] -> Just number
    _ -> Nothing

-- | Parse comma-separated exact model mappings in @source=target@ form.
--
-- Whitespace around each side is ignored. Empty or malformed entries are
-- skipped so one bad override does not discard the remaining valid entries.
parseModelOverrides :: Text -> [(Text, Text)]
parseModelOverrides raw =
    Maybe.mapMaybe parseEntry (Text.splitOn "," raw)
  where
    parseEntry entry = case Text.breakOn "=" entry of
        (source, target)
            | not (Text.null (Text.strip source))
            , Just stripped <- Text.stripPrefix "=" target
            , not (Text.null (Text.strip stripped)) ->
                Just (Text.strip source, Text.strip stripped)
        _ -> Nothing
