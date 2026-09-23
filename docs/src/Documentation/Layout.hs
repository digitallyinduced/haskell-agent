{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Layout (renderPage, plainText, textPath) where

import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Documentation.Types
import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.HTML.TagSoup (Tag (..), parseTags, innerText)

plainText :: Page -> Text
plainText page = Text.strip $ Text.concat $ map readableTag $ parseTags $
    LazyText.toStrict (renderHtml (pageBody page))
  where
    readableTag (TagText value) = value
    readableTag (TagOpen "li" _) = "\n- "
    readableTag (TagOpen "br" _) = "\n"
    readableTag (TagClose name)
        | name `elem` ["p", "h2", "h3", "pre", "tr", "ul", "ol", "blockquote"] = "\n\n"
        | name `elem` ["td", "th"] = " | "
    readableTag _ = ""

textPath :: Page -> Text
textPath page = "/text" <> (if pagePath page == "/" then "/index/" else pagePath page)

renderPage :: [Page] -> Page -> Text -> Html
renderPage pages page query = [hsx|
    <!DOCTYPE html>
    <html lang="en" data-theme="auto">
        <head>
            <meta charset="utf-8"/>
            <meta name="viewport" content="width=device-width, initial-scale=1"/>
            <title>{pageTitle page} — Haskell Agent</title>
            <meta name="description" content={pageDescription page}/>
            <link rel="icon" href="/favicon.svg" type="image/svg+xml"/>
            <link rel="stylesheet" href="/documentation.css"/>
            <script src="/documentation.js" defer></script>
        </head>
        <body>
            <a class="skip-link" href="#main-content">Skip to content</a>
            <header class="site-header">
                <a class="site-title" href="/">Haskell Agent <span>Documentation</span></a>
                <form action="/search/" method="get" role="search">
                    <label class="visually-hidden" for="search-input">Search documentation</label>
                    <input id="search-input" type="search" name="q" value={query} placeholder="Search documentation…" maxlength="200"/>
                    <button type="submit">Search</button>
                </form>
                <label class="theme-control" for="theme-select">Theme
                    <select id="theme-select">
                        <option value="auto">Auto</option>
                        <option value="light">Light</option>
                        <option value="dark">Dark</option>
                    </select>
                </label>
                <button id="menu-toggle" type="button" aria-controls="sidebar" aria-expanded="false">Menu</button>
            </header>
            <div class="documentation-layout">
                <nav id="sidebar" aria-label="Documentation">
                    {foldMap renderGroup (nub (map pageGroup pages))}
                    <a href="/llms.txt">Documentation index for agents</a>
                </nav>
                <main id="main-content" tabindex="-1">
                    <article>
                        <header class="page-header">
                            <p class="eyebrow">{pageGroup page}</p>
                            <h1>{pageTitle page}</h1>
                            <p class="page-description">{pageDescription page}</p>
                            {exportLink}
                        </header>
                        {mobileContents}
                        {pageBody page}
                        {adjacentPages}
                    </article>
                    <footer>
                        <p>Haskell Agent</p>
                        <p><a href="https://github.com/digitallyinduced/haskell-agent/tree/master/docs/src/Documentation/Pages">Edit documentation</a> · <a href="https://github.com/digitallyinduced/haskell-agent/issues/new">Report a documentation issue</a></p>
                        <p>When reporting a problem, include the page address and the command you tried. Remove credentials and private project information.</p>
                    </footer>
                </main>
                <aside id="table-of-contents" aria-label="On this page">{desktopContents}</aside>
            </div>
        </body>
    </html>
|]
  where
    headings :: [(Text, Text, Text)]
    headings = collectHeadings (parseTags (LazyText.toStrict (renderHtml (pageBody page))))
    collectHeadings :: [Tag Text] -> [(Text, Text, Text)]
    collectHeadings (TagOpen name attributes : rest)
        | name `elem` ["h2", "h3"], Just identifier <- lookup "id" attributes =
            let (content, remaining) = break (== TagClose name) rest
            in (name, identifier, innerText content) : collectHeadings (drop 1 remaining)
    collectHeadings (_ : rest) = collectHeadings rest
    collectHeadings [] = []
    contentsList :: Html
    contentsList = [hsx|<ul>{foldMap contentsLink headings}</ul>|]
    contentsLink :: (Text, Text, Text) -> Html
    contentsLink (level, identifier, label) =
        let itemClass = if level == "h3" then "subsection" else "section" :: Text
        in [hsx|<li class={itemClass}><a href={"#" <> identifier}>{label}</a></li>|]
    desktopContents :: Html
    desktopContents
        | null headings = mempty
        | otherwise = [hsx|<h2>On this page</h2>{contentsList}|]
    mobileContents :: Html
    mobileContents
        | null headings = mempty
        | otherwise = [hsx|<details class="mobile-contents"><summary>On this page</summary>{contentsList}</details>|]
    adjacentPages :: Html
    adjacentPages = case break ((== pagePath page) . pagePath) pages of
        (before, _ : after) -> [hsx|
            <nav aria-label="Adjacent pages">
                <hr/>
                {foldMap (adjacentLink "Previous") (take 1 (reverse before))}
                {foldMap (adjacentLink "Next") (take 1 after)}
            </nav>
        |]
        _ -> mempty
    adjacentLink :: Text -> Page -> Html
    adjacentLink label target = [hsx|<p>{label}: <a href={pagePath target}>{pageTitle target}</a></p>|]
    exportLink :: Html
    exportLink
        | any ((== pagePath page) . pagePath) pages = [hsx|<a class="text-export" href={textPath page}>View as text</a>|]
        | otherwise = mempty
    renderGroup :: Text -> Html
    renderGroup group = [hsx|
        <section class="navigation-group">
            <h2>{group}</h2>
            <ul>{foldMap renderLink (filter ((== group) . pageGroup) pages)}</ul>
        </section>
    |]
    renderLink :: Page -> Html
    renderLink target =
        let current = if pagePath target == pagePath page then "page" else "false" :: Text
        in [hsx|<li><a href={pagePath target} aria-current={current}>{pageTitle target}</a></li>|]
