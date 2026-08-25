-- | File URI encoding shared by terminal and language-server integrations.
module Agent.CLI.FileUri
    ( fileUri
    , fileUriPath
    ) where

import Control.Monad (guard)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.URI
    ( escapeURIString
    , isUnreserved
    , parseURI
    , unEscapeString
    , uriPath
    , uriScheme
    )

-- | Encode a filesystem path as a file URI.
--
-- Slashes and URI-unreserved characters stay readable. Colons are also valid
-- within URI path segments and are kept for compatibility with terminal OSC 7
-- working-directory reports.
fileUri :: FilePath -> Text
fileUri path =
    Text.pack ("file://" <> escapeURIString uriPathCharacter path)
  where
    uriPathCharacter character =
        character == '/' || character == ':' || isUnreserved character

-- | Decode the path component of a file URI.
fileUriPath :: Text -> Maybe FilePath
fileUriPath raw = do
    uri <- parseURI (Text.unpack raw)
    guard (uriScheme uri == "file:")
    pure (unEscapeString (uriPath uri))
