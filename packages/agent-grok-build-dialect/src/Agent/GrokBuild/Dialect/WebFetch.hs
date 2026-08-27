-- | Grok Build's model-facing @web_fetch@ contract.
module Agent.GrokBuild.Dialect.WebFetch
    ( WebFetchRequest(..)
    , webFetchTool
    ) where

import Agent.GrokBuild.Dialect.Common (jsonTool)
import qualified Agent.Json.Decode as Json
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch (typedTool)
import Agent.Tools.Types (AppTool, ToolExecutionPolicy(..))
import Data.Text (Text)

data WebFetchRequest = WebFetchRequest
    { webFetchUrl :: !Text
    }
    deriving (Eq, Show)

webFetchRequestDecoder :: Json.Decoder WebFetchRequest
webFetchRequestDecoder = Json.object $
    WebFetchRequest <$> Json.atKey "url" Json.text

webFetchTool
    :: (WebFetchRequest -> IO (Either Text Text))
    -> AppTool
webFetchTool run =
    jsonTool
        "web_fetch"
        webFetchDescription
        [ PropertySchema "url" PropertyString True $
            Just "The URL to fetch content from."
        ]
        True
        ParallelSafe
        (typedTool "web_fetch" webFetchRequestDecoder run)

webFetchDescription :: Text
webFetchDescription =
    "Fetch the content of a specific URL and return it as markdown.\n\n\
    \IMPORTANT: web_fetch will fail for authenticated or private URLs. \
    \Use specialized integration tools for those instead.\n\n\
    \Usage notes:\n\
    \  - Public HTTP URLs are automatically upgraded to HTTPS\n\
    \  - Long pages are saved to session scratch storage and truncated inline"
