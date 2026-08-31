-- | Environment lookups used by the CLI.
module Agent.CLI.Environment
    ( lookupNonEmpty
    ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified System.Process.Environment.OsString as Environment
import qualified System.OsPath as OsPath

-- | Read a non-empty environment variable using the platform environment
-- representation, decoding its value as UTF-8.
lookupNonEmpty :: String -> IO (Maybe Text)
lookupNonEmpty name = do
    value <- Environment.getEnv (OsPath.unsafeEncodeUtf name)
    pure $ case value of
        Just raw
            | Right text <- OsPath.decodeUtf raw
            , not (null text) ->
                Just (Text.pack text)
        _ -> Nothing
