{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (forM_)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LazyText
import Documentation.Application (loadApplication, searchPages)
import Documentation.Content (pages)
import Documentation.Layout (plainText, renderPage, textPath)
import Documentation.Types
import Network.HTTP.Types
import Network.Wai (Application, defaultRequest, rawPathInfo, requestMethod)
import Network.Wai.Test
import System.Directory (doesDirectoryExist)
import Test.Hspec
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as Html
import Text.HTML.TagSoup

main :: IO ()
main = do
    localAssets <- doesDirectoryExist "public"
    application <- loadApplication (if localAssets then "public" else "docs/public")
    hspec (spec application)

spec :: Application -> Spec
spec application = do
    describe "Documentation routes" $ do
        it "registers unique canonical page routes including the reference guides" $ do
            map pagePath pages `shouldContain` ["/", "/getting-started/installation/"]
            map pagePath pages `shouldContain` ["/guides/agent-lifecycle/"]
            forM_ ["/reference/configuration/", "/reference/providers/", "/reference/tools/", "/reference/keybindings/", "/reference/environment/", "/reference/persisted-settings/", "/guides/documentation/"] $ \path ->
                map pagePath pages `shouldContain` [path]
            forM_ ["/customization/language-servers/", "/customization/web-access/", "/customization/learned-skills/", "/guides/telegram/", "/guides/voice/"] $ \path ->
                map pagePath pages `shouldContain` [path]
            forM_ ["/reference/server/", "/reference/runtime-daemon/", "/reference/native-integration/", "/guides/deployment/", "/guides/structured-memory/", "/guides/scheduled-work/", "/guides/browser-control/", "/reference/tool-execution/"] $ \path ->
                map pagePath pages `shouldContain` [path]
            map pagePath pages `shouldContain` ["/tutorials/fix-a-bug/", "/tutorials/investigate-a-repository/", "/tutorials/parallel-changes/", "/tutorials/local-model/", "/tutorials/connect-mcp/"]
            map pagePath pages `shouldSatisfy` (\paths -> nub paths == paths)
            map pagePath pages `shouldSatisfy` all (Text.isSuffixOf "/")
        forM_ pages $ \page -> do
            it ("serves " <> Text.unpack (pagePath page)) $ do
                result <- fetch application methodGet (Text.encodeUtf8 (pagePath page))
                simpleStatus result `shouldBe` status200
                lookup hContentType (simpleHeaders result) `shouldBe` Just "text/html; charset=utf-8"
                responseText result `shouldSatisfy` Text.isInfixOf (pageTitle page)
            it ("serves text for " <> Text.unpack (pagePath page)) $ do
                result <- fetch application methodGet (Text.encodeUtf8 (textPath page))
                simpleStatus result `shouldBe` status200
                lookup hContentType (simpleHeaders result) `shouldBe` Just "text/plain; charset=utf-8"
                responseText result `shouldSatisfy` Text.isPrefixOf (pageTitle page)
            it ("has valid links and unique identifiers in " <> Text.unpack (pagePath page)) $
                validateLinks page
            it ("renders section navigation without JavaScript in " <> Text.unpack (pagePath page)) $ do
                let bodyTags = parseTags (LazyText.toStrict (renderHtml (pageBody page)))
                    sectionIds = [identifier | TagOpen name attributes <- bodyTags, name `elem` ["h2", "h3"], Just identifier <- [lookup "id" attributes]]
                    contents = takeWhile (/= TagClose "aside") $ dropWhile (not . isContents) (pageTags page)
                    links = [value | TagOpen "a" attributes <- contents, ("href", value) <- attributes]
                links `shouldBe` map ("#" <>) sectionIds
                length sectionIds `shouldBe` length [() | TagOpen name _ <- bodyTags, name `elem` ["h2", "h3"]]
        it "returns an empty HEAD body for documents, text, search, assets and missing routes" $
            forM_ ["/", "/text/index/", "/search/?q=sessions", "/documentation.css", "/missing/"] $ \path -> do
                result <- fetch application methodHead path
                simpleBody result `shouldBe` ""
        it "rejects write methods" $ do
            result <- fetch application methodPost "/"
            simpleStatus result `shouldBe` status405
            lookup "Allow" (simpleHeaders result) `shouldBe` Just "GET, HEAD"
        it "redirects known pages to trailing slashes while preserving the query" $ do
            result <- fetch application methodGet "/guides/sessions?q=resume"
            simpleStatus result `shouldBe` status308
            lookup hLocation (simpleHeaders result) `shouldBe` Just "/guides/sessions/?q=resume"
        it "provides a useful 404 rather than an unrelated page" $ do
            result <- fetch application methodGet "/missing/"
            simpleStatus result `shouldBe` status404
            responseText result `shouldSatisfy` Text.isInfixOf "Page not found"
        it "indexes every text export exactly once" $ do
            result <- fetch application methodGet "/llms.txt"
            simpleStatus result `shouldBe` status200
            let body = responseText result
            forM_ pages $ \page -> Text.count ("(" <> textPath page <> ")") body `shouldBe` 1
    describe "Search" $ do
        it "renders adjacent-page navigation and feedback links" $ do
            result <- fetch application methodGet "/getting-started/installation/"
            let tags = parseTags (responseText result)
                links = [value | TagOpen "a" attributes <- tags, ("href", value) <- attributes]
            tags `shouldSatisfy` any (\tag -> case tag of
                TagOpen "nav" attributes -> lookup "aria-label" attributes == Just "Adjacent pages"
                _ -> False)
            links `shouldContain` ["https://github.com/digitallyinduced/haskell-agent/issues/new"]
        it "preserves command boundaries in plain-text exports" $ do
            let page = Page "/example/" "Example" "" "Reference" (Html.pre (Html.code "first command\nsecond command\n"))
            plainText page `shouldSatisfy` Text.isInfixOf "first command\nsecond command"
        it "matches case-insensitively and prioritizes titles" $ do
            map pagePath (searchPages "SESSIONS") `shouldContain` ["/guides/sessions/"]
            take 1 (map pagePath (searchPages "SESSIONS")) `shouldBe` ["/guides/sessions/"]
        it "requires all search terms" $
            map pagePath (searchPages "sessions improbablewordnotindocumentation") `shouldBe` []
        it "does not return every page for an empty query" $
            map pagePath (searchPages "   ") `shouldBe` []
        it "serves HTML search results without client-side JavaScript" $ do
            result <- fetch application methodGet "/search/?q=sessions"
            simpleStatus result `shouldBe` status200
            responseText result `shouldSatisfy` Text.isInfixOf "class=\"search-results\""
            let resultTags = dropWhile (/= TagOpen "ul" [("class", "search-results")]) (parseTags (responseText result))
                resultLinks = [value | TagOpen "a" attributes <- takeWhile (/= TagClose "ul") resultTags, ("href", value) <- attributes]
            resultLinks `shouldContain` ["/guides/sessions/"]
        it "escapes untrusted query text in content and attributes" $ do
            result <- fetch application methodGet "/search/?q=%3Cscript%3Ealert%281%29%3C%2Fscript%3E%22"
            responseText result `shouldSatisfy` (not . Text.isInfixOf "<script>alert(1)</script>")
            responseText result `shouldSatisfy` Text.isInfixOf "&lt;script&gt;"
        it "tolerates malformed UTF-8 in queries" $ do
            result <- fetch application methodGet "/search/?q=%FF"
            simpleStatus result `shouldBe` status200
    describe "Local assets and security" $ do
        forM_ [("/documentation.css", "text/css; charset=utf-8"), ("/documentation.js", "text/javascript; charset=utf-8"), ("/favicon.svg", "image/svg+xml"), ("/terminal-overview.svg", "image/svg+xml"), ("/terminal-overview.txt", "text/plain; charset=utf-8"), ("/login-dashboard.svg", "image/svg+xml"), ("/provider-chooser.svg", "image/svg+xml"), ("/interactive-terminal-captures.txt", "text/plain; charset=utf-8")] $ \(path, contentType) ->
            it ("serves the registered asset " <> show path) $ do
                result <- fetch application methodGet path
                simpleStatus result `shouldBe` status200
                lookup hContentType (simpleHeaders result) `shouldBe` Just contentType
                simpleBody result `shouldSatisfy` (not . LazyByteString.null)
        forM_ [("/HaskellAgentBridge.h", "text/plain; charset=utf-8"), ("/agent-server-openapi.json", "application/json")] $ \(path, contentType) ->
            it ("serves the interface contract " <> show path) $ do
                result <- fetch application methodGet path
                simpleStatus result `shouldBe` status200
                lookup hContentType (simpleHeaders result) `shouldBe` Just contentType
                simpleBody result `shouldSatisfy` (not . LazyByteString.null)
                headResult <- fetch application methodHead path
                simpleBody headResult `shouldBe` ""
        it "never interprets arbitrary request paths as filesystem paths" $
            forM_ ["/../flake.nix", "/%2e%2e/flake.nix", "/public/../package.nix", "/documentation.css/../package.nix"] $ \path -> do
                result <- fetch application methodGet path
                simpleStatus result `shouldBe` status404
        it "uses local-only browser resource policies and disables MIME sniffing" $ do
            result <- fetch application methodGet "/"
            lookup "X-Content-Type-Options" (simpleHeaders result) `shouldBe` Just "nosniff"
            lookup "Content-Security-Policy" (simpleHeaders result) `shouldSatisfy`
                maybe False (ByteString.isInfixOf "default-src 'self'")

fetch :: Application -> Method -> ByteString.ByteString -> IO SResponse
fetch application method path =
    -- setPath normalizes "/" to an empty rawPathInfo. Keep the actual wire path,
    -- including encoded traversal probes, while retaining its decoded query.
    runSession (request ((setPath defaultRequest path)
        { requestMethod = method, rawPathInfo = ByteString.takeWhile (/= 63) path })) application

responseText :: SResponse -> Text
responseText = Text.decodeUtf8 . LazyByteString.toStrict . simpleBody

isContents :: Tag Text -> Bool
isContents (TagOpen "aside" attributes) = lookup "id" attributes == Just "table-of-contents"
isContents _ = False

pageTags :: Page -> [Tag Text]
pageTags page = parseTags $ LazyText.toStrict $ renderHtml $ renderPage pages page ""

identifiers :: Page -> [Text]
identifiers page = [value | TagOpen _ attributes <- pageTags page, ("id", value) <- attributes]

validateLinks :: Page -> Expectation
validateLinks page = do
    let ids = identifiers page
        references = [value | TagOpen _ attributes <- pageTags page, (name, value) <- attributes, name `elem` ["href", "src", "action"]]
        sidebarTags = takeWhile (/= TagClose "nav") $ dropWhile (not . isSidebar) (pageTags page)
        sidebarLinks = [value | TagOpen "a" attributes <- sidebarTags, ("href", value) <- attributes]
        available = map pagePath pages <> map textPath pages <> ["/llms.txt", "/search/", "/documentation.css", "/documentation.js", "/favicon.svg", "/terminal-overview.svg", "/terminal-overview.txt", "/login-dashboard.svg", "/provider-chooser.svg", "/interactive-terminal-captures.txt"]
        interfaceAssets = ["/HaskellAgentBridge.h", "/agent-server-openapi.json"]
    nub ids `shouldBe` ids
    forM_ pages $ \target -> pagePath target `shouldSatisfy` (`elem` sidebarLinks)
    forM_ references $ \reference ->
        if Text.isPrefixOf "/" reference || Text.isPrefixOf "#" reference
            then do
                let (pathPart, fragment) = Text.breakOn "#" reference
                    targetPath = if Text.null pathPart then pagePath page else pathPart
                targetPath `shouldSatisfy` (`elem` (available <> interfaceAssets))
                if Text.null fragment
                    then pure ()
                    else case filter ((== targetPath) . pagePath) pages of
                        [target] -> Text.drop 1 fragment `shouldSatisfy` (`elem` identifiers target)
                        _ -> expectationFailure ("Fragment refers to non-page resource: " <> Text.unpack reference)
            else reference `shouldSatisfy` (\value -> "https://" `Text.isPrefixOf` value || "http://" `Text.isPrefixOf` value || "mailto:" `Text.isPrefixOf` value)
  where
    isSidebar (TagOpen "nav" attributes) = lookup "id" attributes == Just "sidebar"
    isSidebar _ = False
