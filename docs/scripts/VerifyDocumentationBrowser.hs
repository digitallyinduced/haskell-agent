{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Run with: nix develop .#docs -c runghc docs/scripts/VerifyDocumentationBrowser.hs
-- Uses Chrome's DevTools protocol directly, with a fresh temporary profile and
-- the normal browser sandbox. It never opens a personal browser profile.
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (finally)
import Control.Monad (forM_, unless, void, when)
import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (Pair)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LazyByteString
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Client as HTTP
import qualified Network.WebSockets as WebSocket
import System.Directory
import System.Environment
import System.FilePath
import System.IO.Temp (withTempDirectory)
import System.Process
import System.Timeout (timeout)

data Browser = Browser WebSocket.Connection (IORef Int) (IORef [Value])

main :: IO ()
main = do
    address <- Text.pack . fromMaybe "http://127.0.0.1:4321" <$> lookupEnv "DOCUMENTATION_URL"
    executable <- getEnv "CHROME_EXECUTABLE"
    screenshots <- getEnv "DOCUMENTATION_SCREENSHOTS"
    temporaryRoot <- getEnv "TMPDIR"
    createDirectoryIfMissing True screenshots
    withTempDirectory temporaryRoot "documentation-browser-" $ \profile ->
        withCreateProcess (proc executable
            ["--headless=new", "--remote-debugging-port=0", "--remote-debugging-address=127.0.0.1",
             "--user-data-dir=" <> profile, "--no-first-run", "--no-default-browser-check",
             "--disable-background-networking", "--disable-component-update", "about:blank"])
            { std_out = NoStream } $ \_ _ _ process ->
            flip finally (terminateProcess process >> void (waitForProcess process)) $ do
                waitFor "Chrome debugging endpoint" (doesFileExist (profile </> "DevToolsActivePort"))
                endpoint <- lines <$> readFile (profile </> "DevToolsActivePort")
                port <- case endpoint of
                    value : _ -> pure (read value)
                    _ -> fail "Chrome did not publish a debugging port"
                manager <- HTTP.newManager HTTP.defaultManagerSettings
                request <- HTTP.parseRequest ("http://127.0.0.1:" <> show port <> "/json/new?about:blank")
                response <- HTTP.httpLbs request { HTTP.method = "PUT" } manager
                target <- either fail pure (eitherDecode (HTTP.responseBody response))
                identifier <- stringAt "id" target
                WebSocket.runClient "127.0.0.1" port ("/devtools/page/" <> Text.unpack identifier) $ \connection -> do
                    browser <- Browser connection <$> newIORef 0 <*> newIORef []
                    void $ command browser "Page.enable" []
                    void $ command browser "Runtime.enable" []
                    void $ command browser "Network.enable" []
                    verify browser address screenshots

command :: Browser -> Text -> [Pair] -> IO Value
command (Browser connection nextIdentifier errors) method parameters = do
    identifier <- atomicModifyIORef' nextIdentifier (\value -> (value + 1, value + 1))
    WebSocket.sendTextData connection (encode (object ["id" .= identifier, "method" .= method, "params" .= object parameters]))
    result <- timeout 15000000 (receive identifier)
    maybe (fail ("Timed out: " <> Text.unpack method)) pure result
  where
    receive identifier = do
        message <- WebSocket.receiveData connection :: IO LazyByteString.ByteString
        value <- either fail pure (eitherDecode message)
        when (field "method" value == String "Runtime.exceptionThrown") $
            modifyIORef' errors (value :)
        if field "id" value == toJSON identifier
            then if field "error" value /= Null
                then fail ("DevTools error: " <> show value)
                else pure (field "result" value)
            else receive identifier

field :: Text -> Value -> Value
field name (Object values) = fromMaybe Null (KeyMap.lookup (fromText name) values)
field _ _ = Null

stringAt :: Text -> Value -> IO Text
stringAt name value = case field name value of
    String result -> pure result
    other -> fail ("Expected text field " <> Text.unpack name <> ": " <> show other)

evaluate :: Browser -> Text -> IO Value
evaluate browser expression = do
    result <- command browser "Runtime.evaluate"
        ["expression" .= expression, "returnByValue" .= True, "awaitPromise" .= True]
    unless (field "exceptionDetails" result == Null) $
        fail ("Browser evaluation failed: " <> show result)
    pure (field "value" (field "result" result))

assertBrowser :: Browser -> Text -> IO ()
assertBrowser browser expression = do
    value <- evaluate browser expression
    unless (value == Bool True) $ fail ("Browser assertion failed: " <> Text.unpack expression <> ": " <> show value)

waitFor :: String -> IO Bool -> IO ()
waitFor description condition = do
    result <- timeout 15000000 loop
    unless (result == Just ()) $ fail ("Timed out waiting for " <> description)
  where
    loop = do
        ready <- condition
        unless ready (threadDelay 50000 >> loop)

waitBrowser :: Browser -> Text -> IO ()
waitBrowser browser expression = waitFor (Text.unpack expression) ((== Bool True) <$> evaluate browser expression)

literal :: Text -> Text
literal = Text.decodeUtf8 . LazyByteString.toStrict . encode

selector :: Text -> Text
selector value = "document.querySelector(" <> literal value <> ")"

visible :: Browser -> Text -> IO ()
visible browser value = waitBrowser browser $
    "(()=>{const e=" <> selector value <> ";if(!e)return false;const r=e.getBoundingClientRect();return r.width>0&&r.height>0&&getComputedStyle(e).visibility!=='hidden'})()"

click :: Browser -> Text -> IO ()
click browser value = do
    visible browser value
    position <- evaluate browser $ "(()=>{const e=" <> selector value <> ";e.scrollIntoView({block:'center'});const r=e.getBoundingClientRect();return {x:r.x+r.width/2,y:r.y+r.height/2}})()"
    forM_ ["mousePressed", "mouseReleased" :: Text] $ \event ->
        void $ command browser "Input.dispatchMouseEvent"
            ["type" .= event, "x" .= field "x" position, "y" .= field "y" position,
             "button" .= ("left" :: Text), "clickCount" .= (1 :: Int)]

key :: Browser -> Text -> Int -> IO ()
key browser name modifiers = forM_ ["keyDown", "keyUp" :: Text] $ \event ->
    void $ command browser "Input.dispatchKeyEvent"
        ["type" .= event, "key" .= name, "modifiers" .= modifiers,
         "text" .= (if event == "keyDown" && name == "Enter" then "\r" else "" :: Text),
         "windowsVirtualKeyCode" .= (if name == "Enter" then 13 else if name == "Escape" then 27 else 75 :: Int)]

navigate :: Browser -> Text -> IO ()
navigate browser url = do
    result <- command browser "Page.navigate" ["url" .= url]
    unless (field "errorText" result == Null) $ fail ("Navigation failed: " <> show result)
    waitBrowser browser ("location.href===" <> literal url <> "&&document.readyState==='complete'")
    -- Resource Timing records the actual navigation response, not a second fetch.
    assertBrowser browser "performance.getEntriesByType('navigation')[0].responseStatus===200"

viewport :: Browser -> Int -> Int -> IO ()
viewport browser width height = void $ command browser "Emulation.setDeviceMetricsOverride"
    ["width" .= width, "height" .= height, "deviceScaleFactor" .= (1 :: Int), "mobile" .= False]

screenshot :: Browser -> FilePath -> IO ()
screenshot browser path = do
    result <- command browser "Page.captureScreenshot" ["format" .= ("png" :: Text)]
    encoded <- stringAt "data" result
    content <- either fail pure (Base64.decode (Text.encodeUtf8 encoded))
    ByteString.writeFile path content

search :: Browser -> Text -> IO ()
search browser query = do
    click browser "#search-input"
    void $ command browser "Input.insertText" ["text" .= query]
    key browser "Enter" 0
    visible browser ".search-results a"

verify :: Browser -> Text -> FilePath -> IO ()
verify browser@(Browser _ _ errors) address screenshots = do
    viewport browser 1440 1000
    navigate browser (address <> "/")
    visible browser "h1"
    pathsValue <- evaluate browser "Array.from(document.querySelectorAll('#sidebar .navigation-group a'),a=>a.getAttribute('href'))"
    paths <- case fromJSON pathsValue of
        Success values -> pure (values :: [Text])
        Error message -> fail message
    unless (not (null paths)) $ fail "No documentation routes found"
    screenshot browser (screenshots </> "documentation-desktop.png")
    navigate browser (address <> "/getting-started/installation/")
    visible browser "h1"
    assertBrowser browser "document.querySelector('.text-export').getAttribute('href')==='/text/getting-started/installation/'"
    visible browser "#table-of-contents a"
    visible browser "code.language-sh .syntax-command"
    assertBrowser browser "(async()=>{const html=await(await fetch(location.href)).text();const original=new DOMParser().parseFromString(html,'text/html');window.originalDocumentationExamples=Array.from(original.querySelectorAll('pre > code'),e=>e.textContent);return JSON.stringify(Array.from(document.querySelectorAll('pre > code'),e=>e.textContent))===JSON.stringify(window.originalDocumentationExamples)})()"
    void $ evaluate browser "window.copiedDocumentation=null;Object.defineProperty(navigator.clipboard,'writeText',{value:async text=>{window.copiedDocumentation=text}})"
    click browser ".copy-button"
    waitBrowser browser "window.copiedDocumentation===window.originalDocumentationExamples[0]&&window.copiedDocumentation.includes('nix')"
    setTheme "dark"
    navigate browser (address <> "/getting-started/installation/")
    assertBrowser browser "document.documentElement.dataset.theme==='dark'"
    setTheme "light"
    screenshot browser (screenshots </> "documentation-installation-light.png")
    forM_ [2, 4] $ \modifier -> do
        click browser "h1"
        key browser "k" modifier
        assertBrowser browser "document.activeElement===document.querySelector('#search-input')"
    visible browser "nav[aria-label='Adjacent pages']"
    assertBrowser browser "Array.from(document.querySelectorAll('a')).some(a=>a.textContent==='Report a documentation issue'&&a.href==='https://github.com/digitallyinduced/haskell-agent/issues/new')"
    search browser "sessions"
    screenshot browser (screenshots </> "documentation-search.png")
    click browser ".search-results a"
    waitBrowser browser ("location.href===" <> literal (address <> "/guides/sessions/") <> "&&document.readyState==='complete'")
    visible browser "h1"
    viewport browser 390 844
    counts <- mapM (\route -> do
        navigate browser (address <> route)
        visible browser "h1"
        assertBrowser browser "document.documentElement.scrollWidth<=innerWidth"
        assertBrowser browser "Array.from(document.querySelectorAll('article img')).every(i=>i.alt.length>0)"
        void $ evaluate browser "document.querySelectorAll('article img').forEach(i=>i.loading='eager')"
        waitBrowser browser "Array.from(document.querySelectorAll('article img')).every(i=>i.complete&&i.naturalWidth>0)"
        assertBrowser browser "JSON.stringify(Array.from(document.querySelectorAll('article h2[id],article h3[id]'),e=>'#'+e.id))===JSON.stringify(Array.from(document.querySelectorAll('#table-of-contents a'),e=>e.getAttribute('href')))"
        count <- evaluate browser "Array.from(document.querySelectorAll('pre > code.language-json'),e=>JSON.parse(e.textContent)).length"
        case fromJSON count of
            Success value -> pure (value :: Int)
            Error message -> fail message) paths
    unless (sum counts > 0) $ fail "No JSON examples found"
    navigate browser (address <> "/reference/configuration/")
    click browser ".mobile-contents summary"
    visible browser ".mobile-contents a"
    screenshot browser (screenshots </> "documentation-configuration-mobile.png")
    navigate browser (address <> "/")
    screenshot browser (screenshots </> "documentation-mobile.png")
    assertBrowser browser "document.documentElement.scrollWidth<=innerWidth"
    click browser "#menu-toggle"
    assertBrowser browser "document.querySelector('#menu-toggle').getAttribute('aria-expanded')==='true'"
    key browser "Escape" 0
    assertBrowser browser "document.querySelector('#menu-toggle').getAttribute('aria-expanded')==='false'"
    click browser "#menu-toggle"
    click browser "#sidebar a[href='/guides/sessions/']"
    waitBrowser browser ("location.href===" <> literal (address <> "/guides/sessions/") <> "&&document.readyState==='complete'")
    assertBrowser browser "document.querySelector('#menu-toggle').getAttribute('aria-expanded')==='false'&&document.documentElement.scrollWidth<=innerWidth"
    -- DevTools evaluation remains available while page script execution is disabled.
    -- Interactions below use browser input events, not JavaScript event handlers.
    void $ command browser "Emulation.setScriptExecutionDisabled" ["value" .= True]
    navigate browser (address <> "/")
    assertBrowser browser "document.querySelector('.copy-button')===null"
    click browser ".mobile-contents summary"
    visible browser ".mobile-contents a"
    click browser "#sidebar a[href='/guides/sessions/']"
    waitBrowser browser ("location.href===" <> literal (address <> "/guides/sessions/") <> "&&document.readyState==='complete'")
    search browser "skills"
    recorded <- readIORef errors
    unless (null recorded) $ fail ("Browser page errors: " <> show recorded)
    putStrLn ("Browser checks passed: " <> show (length paths) <> " pages, " <> show (sum counts) <>
        " JSON examples, mobile overflow, section navigation, server search, themes, copy button, text export, and JavaScript-disabled use.")
  where
    setTheme theme = do
        void $ evaluate browser ("document.querySelector('#theme-select').value=" <> literal theme <> ";document.querySelector('#theme-select').dispatchEvent(new Event('change',{bubbles:true}))")
        assertBrowser browser ("document.documentElement.dataset.theme===" <> literal theme)
