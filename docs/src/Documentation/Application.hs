{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Application (application, loadApplication, searchPages) where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Data.List (find, sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import Documentation.Content (pages)
import Documentation.Layout (plainText, renderPage, textPath)
import Documentation.Types
import IHP.HSX.QQ (hsx)
import Network.HTTP.Types
import Network.Wai
import System.FilePath ((</>))
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

-- Only explicitly registered assets are loaded. Request paths never become file paths.
loadApplication :: FilePath -> IO Application
loadApplication directory = do
    assets <- traverse loadAsset
        [("/documentation.css", "text/css; charset=utf-8", "documentation.css")
        ,("/documentation.js", "text/javascript; charset=utf-8", "documentation.js")
        ,("/favicon.svg", "image/svg+xml", "favicon.svg")
        ,("/terminal-overview.svg", "image/svg+xml", "terminal-overview.svg")
        ,("/terminal-overview.txt", "text/plain; charset=utf-8", "terminal-overview.txt")
        ,("/login-dashboard.svg", "image/svg+xml", "login-dashboard.svg")
        ,("/provider-chooser.svg", "image/svg+xml", "provider-chooser.svg")
        ,("/interactive-terminal-captures.txt", "text/plain; charset=utf-8", "interactive-terminal-captures.txt")
        ,("/agent-server-openapi.json", "application/json", "agent-server-openapi.json")]
    pure (application (Map.fromList assets))
  where
    loadAsset (path, contentType, filename) = do
        body <- LazyByteString.fromStrict <$> ByteString.readFile (directory </> filename)
        pure (path, (contentType, body))

application :: Map.Map ByteString.ByteString (ByteString.ByteString, LazyByteString.ByteString) -> Application
application assets request respond
    | requestMethod request `notElem` [methodGet, methodHead] =
        respond (responseLBS status405 [("Allow", "GET, HEAD")] "Method not allowed")
    | otherwise = respond $ case Map.lookup path assets of
        Just (contentType, body) -> response status200 contentType body
        Nothing
            | path == "/llms.txt" -> response status200 "text/plain; charset=utf-8" (encode llmsIndex)
            | path == "/search/" -> html status200 (searchPage query)
            | Just page <- find ((== path) . Text.encodeUtf8 . pagePath) pages -> html status200 page
            | Just page <- find ((== path) . Text.encodeUtf8 . textPath) pages ->
                response status200 "text/plain; charset=utf-8" (encode (pageTitle page <> "\n\n" <> plainText page <> "\n"))
            | Just page <- find ((== path <> "/") . Text.encodeUtf8 . pagePath) pages ->
                responseLBS status308 [(hLocation, Text.encodeUtf8 (pagePath page) <> rawQueryString request)] ""
            | otherwise -> html status404 notFoundPage
  where
    path = rawPathInfo request
    query = Text.take 200 $ Text.strip $ maybe "" (Text.decodeUtf8With Text.lenientDecode) (lookup "q" (queryString request) >>= id)
    encode = LazyByteString.fromStrict . Text.encodeUtf8
    html status page = response status "text/html; charset=utf-8" (renderHtml (renderPage pages page query))
    response status contentType body = responseLBS status
        [(hContentType, contentType), ("X-Content-Type-Options", "nosniff")
        ,("Content-Security-Policy", "default-src 'self'; object-src 'none'; base-uri 'none'; frame-ancestors 'none'; form-action 'self'")]
        (if requestMethod request == methodHead then "" else body)

searchIndex :: [(Page, Text)]
searchIndex = [(page, Text.toCaseFold (pageTitle page <> " " <> pageDescription page <> " " <> plainText page)) | page <- pages]

searchPages :: Text -> [Page]
searchPages query
    | null terms = []
    | otherwise = sortOn (Down . titleScore) [page | (page, body) <- searchIndex, all (`Text.isInfixOf` body) terms]
  where
    terms = Text.words (Text.toCaseFold (Text.take 200 query))
    titleScore page = length (filter (`Text.isInfixOf` Text.toCaseFold (pageTitle page)) terms)

searchPage :: Text -> Page
searchPage query = Page "/search/" "Search documentation" "Search all user guides locally." "Reference" body
  where
    results = searchPages query
    body :: Html
    body
        | Text.null query = [hsx|<p>Enter a phrase in the search field above.</p>|]
        | null results = [hsx|<p>No results for <strong>{query}</strong>. Try a different phrase.</p>|]
        | otherwise = [hsx|<p>Results for <strong>{query}</strong></p><ul class="search-results">{foldMap result results}</ul>|]
    result page = [hsx|<li><h2><a href={pagePath page}>{pageTitle page}</a></h2><p>{pageDescription page}</p></li>|]

notFoundPage :: Page
notFoundPage = Page "/404/" "Page not found" "This documentation page does not exist." "Documentation"
    [hsx|<p>Choose a guide from the navigation or <a href="/">return to the introduction</a>.</p>|]

llmsIndex :: Text
llmsIndex = "# Haskell Agent documentation\n\n" <> Text.unlines
    ["- [" <> pageTitle page <> "](" <> textPath page <> "): " <> pageDescription page | page <- pages]
