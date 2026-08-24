-- | Disabled-by-default, policy-bound client-side URL fetching for Grok.
module Agent.CLI.WebFetch
    ( WebFetchRuntime
    , AllowedDomain(..)
    , newWebFetchRuntime
    , closeWebFetchRuntime
    , webFetchRuntimeTool
    , parseAllowedDomain
    , domainAllowed
    , htmlToMarkdown
    , isNonPublicIPv4
    , isNonPublicIPv6
    ) where

import Agent.CLI.Config (WebFetchConfig(..))
import Agent.GrokBuild.Dialect.WebFetch
    ( WebFetchRequest(..)
    , webFetchTool
    )
import Agent.OsPath (unsafeToFilePath)
import Agent.Tools.Types (AppTool, ToolEnv(..))
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , newMVar
    )
import Control.Exception.Safe (displayException, tryAny)
import Control.Monad (unless, when)
import Data.Bits ((.&.), shiftR)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isAlphaNum)
import Data.IORef (readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word16, Word8)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.Internal as HttpInternal
import qualified Network.HTTP.Client.TLS as HttpTls
import Network.HTTP.Types.Header
    ( HeaderName
    , ResponseHeaders
    , hAccept
    , hAcceptLanguage
    , hContentLength
    , hContentType
    , hLocation
    , hUserAgent
    )
import Network.HTTP.Types.Status (statusCode)
import Network.Socket
    ( AddrInfo(..)
    , HostAddress
    , SockAddr(..)
    , SocketType(Stream)
    , defaultHints
    , getAddrInfo
    , hostAddress6ToTuple
    , hostAddressToTuple
    )
import Network.URI
    ( URI(..)
    , URIAuth(..)
    , parseURI
    , parseURIReference
    , relativeTo
    , uriToString
    )
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    )
import qualified System.FilePath as FilePath
import System.Posix.Files (setFileMode)
import System.Timeout (timeout)
import Text.HTML.TagSoup (Tag(..), parseTags)
import Text.Read (readMaybe)

data AllowedDomain = AllowedDomain
    { allowedHost :: !Text
    , allowedPathPrefix :: !(Maybe Text)
    }
    deriving (Eq, Show)

data WebFetchRuntime = WebFetchRuntime
    { runtimeManager :: !Http.Manager
    , runtimeAllowedDomains :: ![AllowedDomain]
    , runtimeConfig :: !WebFetchConfig
    , runtimeToolEnv :: !ToolEnv
    , runtimeArtifactCounter :: !(MVar Int)
    }

data FetchedPage = FetchedPage
    { fetchedUrl :: !Text
    , fetchedStatus :: !Int
    , fetchedContentType :: !Text
    , fetchedBody :: !BS.ByteString
    }

data HopResult
    = HopRedirect !URI
    | HopContent !FetchedPage

-- | Build the optional runtime. Disabled configuration returns 'Nothing';
-- malformed allowlist entries fail before a tool can be advertised.
newWebFetchRuntime
    :: WebFetchConfig
    -> ToolEnv
    -> IO (Either Text (Maybe WebFetchRuntime))
newWebFetchRuntime config env
    | not config.webFetchEnabled = pure (Right Nothing)
    | otherwise =
        case traverse parseAllowedDomain config.webFetchAllowedDomains of
            Left err -> pure (Left err)
            Right allowed -> do
                result <- tryAny HttpTls.newTlsManager
                case result of
                    Left exception ->
                        pure . Left $
                            "Failed to initialize web_fetch HTTP client: "
                                <> Text.pack (displayException exception)
                    Right manager -> do
                        counter <- newMVar 0
                        pure . Right . Just $ WebFetchRuntime
                            { runtimeManager = manager
                            , runtimeAllowedDomains = allowed
                            , runtimeConfig = config
                            , runtimeToolEnv = env
                            , runtimeArtifactCounter = counter
                            }

closeWebFetchRuntime :: WebFetchRuntime -> IO ()
closeWebFetchRuntime _ =
    -- http-client managers are finalized automatically and this runtime owns
    -- no separate worker threads. Keeping an explicit close hook lets the CLI
    -- preserve one uniform owner-scoped cleanup path without using the
    -- deprecated closeManager API.
    pure ()

webFetchRuntimeTool :: WebFetchRuntime -> AppTool
webFetchRuntimeTool runtime =
    webFetchTool (runWebFetch runtime)

runWebFetch
    :: WebFetchRuntime
    -> WebFetchRequest
    -> IO (Either Text Text)
runWebFetch runtime request =
    case validateUri request.webFetchUrl of
        Left err -> pure (Left err)
        Right parsed -> do
            let initial = upgradePublicHttp parsed
                timeoutMicros =
                    runtime.runtimeConfig.webFetchTimeoutSeconds
                        * 1000000
            result <- tryAny $
                timeout timeoutMicros (fetchFollowing runtime initial 0)
            case result of
                Left exception ->
                    pure . Left $
                        "web_fetch failed: "
                            <> Text.pack (displayException exception)
                Right Nothing ->
                    pure . Left $
                        "web_fetch timed out after "
                            <> Text.pack
                                (show
                                    runtime.runtimeConfig.webFetchTimeoutSeconds)
                            <> " seconds"
                Right (Just fetched) ->
                    case fetched of
                        Left err -> pure (Left err)
                        Right page -> renderFetchedPage runtime page

fetchFollowing
    :: WebFetchRuntime
    -> URI
    -> Int
    -> IO (Either Text FetchedPage)
fetchFollowing runtime current redirects
    | redirects > maxRedirects =
        pure . Left $
            "web_fetch exceeded the redirect limit of "
                <> Text.pack (show maxRedirects)
    | otherwise =
        validatePolicy runtime current >>= \case
            Left err -> pure (Left err)
            Right pinnedAddress ->
                fetchOne runtime current pinnedAddress >>= \case
                    Left err -> pure (Left err)
                    Right (HopContent page) -> pure (Right page)
                    Right (HopRedirect next) ->
                        fetchFollowing
                            runtime
                            (upgradePublicHttp next)
                            (redirects + 1)

maxRedirects :: Int
maxRedirects = 10

fetchOne
    :: WebFetchRuntime
    -> URI
    -> HostAddress
    -> IO (Either Text HopResult)
fetchOne runtime uri pinnedAddress = do
    parsed <- tryAny $ Http.parseRequest (uriToString id uri "")
    case parsed of
        Left exception ->
            pure . Left $
                "Invalid web_fetch request: "
                    <> Text.pack (displayException exception)
        Right baseRequest -> do
            let configured = baseRequest
                    { Http.method = "GET"
                    , HttpInternal.hostAddress = Just pinnedAddress
                    , Http.redirectCount = 0
                    , Http.checkResponse = \_ _ -> pure ()
                    , Http.responseTimeout =
                        Http.responseTimeoutMicro
                            ( runtime.runtimeConfig.webFetchTimeoutSeconds
                                * 1000000
                            )
                    , Http.requestHeaders =
                        [ (hUserAgent, webFetchUserAgent)
                        , ( hAccept
                          , "text/markdown,text/html,application/xhtml+xml,\
                            \application/json,text/plain;q=0.9,*/*;q=0.5"
                          )
                        , (hAcceptLanguage, "en-US,en;q=0.9")
                        ]
                    }
            response <- tryAny $
                Http.withResponse configured runtime.runtimeManager \value -> do
                    let status = statusCode (Http.responseStatus value)
                    if isRedirectStatus status
                        then case
                            lookup hLocation (Http.responseHeaders value)
                        of
                            Nothing ->
                                pure
                                    (Left
                                        "web_fetch redirect response omitted Location")
                            Just location ->
                                pure (redirectTarget uri location)
                        else if status < 200 || status >= 300
                            then
                                pure . Left $
                                    "web_fetch returned HTTP status "
                                        <> Text.pack (show status)
                        else do
                            let maxBytes =
                                    runtime.runtimeConfig.webFetchMaxContentBytes
                                contentLength =
                                    lookup hContentLength
                                        (Http.responseHeaders value)
                                        >>= readMaybe . BS8.unpack
                            case contentLength of
                                Just size | size > maxBytes ->
                                    pure . Left $
                                        "web_fetch response exceeds maximum size of "
                                            <> Text.pack (show maxBytes)
                                            <> " bytes"
                                _ -> do
                                    readBodyLimited
                                        maxBytes
                                        (Http.responseBody value)
                                        >>= \case
                                            Left err -> pure (Left err)
                                            Right body ->
                                                pure . Right . HopContent $
                                                    FetchedPage
                                                        { fetchedUrl =
                                                            Text.pack
                                                                (uriToString
                                                                    id uri "")
                                                        , fetchedStatus =
                                                            status
                                                        , fetchedContentType =
                                                            headerText
                                                                hContentType
                                                                "text/html"
                                                                (Http.responseHeaders
                                                                    value)
                                                        , fetchedBody = body
                                                        }
            pure case response of
                Left exception ->
                    Left
                        ( "web_fetch HTTP request failed: "
                            <> Text.pack (displayException exception)
                        )
                Right result -> result

redirectTarget :: URI -> BS.ByteString -> Either Text HopResult
redirectTarget current location = do
    let raw = Text.unpack (Text.decodeUtf8With lenientDecode location)
    reference <-
        maybe
            (Left "web_fetch received an invalid redirect Location")
            Right
            (parseURIReference raw)
    let resolved = reference `relativeTo` current
    _ <- validateUri (Text.pack (uriToString id resolved ""))
    pure (HopRedirect resolved)

readBodyLimited
    :: Int
    -> Http.BodyReader
    -> IO (Either Text BS.ByteString)
readBodyLimited limitBytes reader = go 0 []
  where
    go total reversed = do
        chunk <- Http.brRead reader
        if BS.null chunk
            then pure (Right (BS.concat (reverse reversed)))
            else do
                let next = total + BS.length chunk
                if next > limitBytes
                    then
                        pure . Left $
                            "web_fetch response exceeds maximum size of "
                                <> Text.pack (show limitBytes)
                                <> " bytes"
                    else go next (chunk : reversed)

renderFetchedPage
    :: WebFetchRuntime
    -> FetchedPage
    -> IO (Either Text Text)
renderFetchedPage runtime page =
    case decodeContent page.fetchedContentType page.fetchedBody of
        Left err -> pure (Left err)
        Right (renderedType, content) -> do
            let bytes = BS.length (Text.encodeUtf8 content)
                cap = runtime.runtimeConfig.webFetchMaxInlineBytes
                header =
                    "URL: " <> page.fetchedUrl
                        <> "\nStatus: "
                        <> Text.pack (show page.fetchedStatus)
                        <> "\nContent-Type: "
                        <> renderedType
                        <> "\nBytes: "
                        <> Text.pack (show bytes)
                        <> "\n\n"
            if bytes <= cap
                then pure (Right (header <> content))
                else do
                    saveOverflow runtime renderedType content >>= \case
                        Left err ->
                            pure . Right $
                                header
                                    <> truncateUtf8 cap content
                                    <> "\n\n[web_fetch content truncated; "
                                    <> err
                                    <> "]"
                        Right path ->
                            pure . Right $
                                header
                                    <> truncateUtf8 cap content
                                    <> "\n\n[web_fetch content truncated: "
                                    <> Text.pack (show cap)
                                    <> " of "
                                    <> Text.pack (show bytes)
                                    <> " bytes shown. Full content saved to: "
                                    <> Text.pack path
                                    <> ". Use read_file with offsets and limits \
                                       \to inspect it.]"

decodeContent
    :: Text
    -> BS.ByteString
    -> Either Text (Text, Text)
decodeContent rawContentType body
    | isHtmlMime mime =
        Right
            ( "markdown"
            , htmlToMarkdown (Text.decodeUtf8With lenientDecode body)
            )
    | isTextMime mime =
        Right
            ( if Text.null mime then "text/plain" else mime
            , Text.decodeUtf8With lenientDecode body
            )
    | otherwise =
        Left
            ( "web_fetch does not support binary content type "
                <> if Text.null mime then "<unknown>" else mime
            )
  where
    mime =
        Text.toLower . Text.strip . headOr "" $
            Text.splitOn ";" rawContentType

isHtmlMime :: Text -> Bool
isHtmlMime mime =
    mime == "text/html" || mime == "application/xhtml+xml"

isTextMime :: Text -> Bool
isTextMime mime =
    Text.null mime
        || "text/" `Text.isPrefixOf` mime
        || mime
            `elem`
                [ "application/json"
                , "application/xml"
                , "application/javascript"
                , "application/x-javascript"
                , "application/yaml"
                , "application/x-yaml"
                , "application/toml"
                ]
        || "+json" `Text.isSuffixOf` mime
        || "+xml" `Text.isSuffixOf` mime

saveOverflow
    :: WebFetchRuntime
    -> Text
    -> Text
    -> IO (Either Text FilePath)
saveOverflow runtime contentType content = do
    sessionTmp <- readIORef runtime.runtimeToolEnv.toolSessionTmp
    case sessionTmp of
        Nothing ->
            pure (Left "session scratch storage is unavailable")
        Just root ->
            modifyMVar runtime.runtimeArtifactCounter \counter -> do
                result <- tryAny do
                    let directory =
                            unsafeToFilePath root
                                FilePath.</> "web_fetch"
                    createDirectoryIfMissing True directory
                    setFileMode directory 0o700
                    (next, path) <-
                        nextArtifactPath
                            directory
                            (artifactExtension contentType)
                            (counter + 1)
                    BS.writeFile path (Text.encodeUtf8 content)
                    setFileMode path 0o600
                    pure (next, path)
                pure case result of
                    Left exception ->
                        ( counter
                        , Left
                            ( "failed to save full content: "
                                <> Text.pack
                                    (displayException exception)
                            )
                        )
                    Right (next, path) -> (next, Right path)

nextArtifactPath
    :: FilePath
    -> String
    -> Int
    -> IO (Int, FilePath)
nextArtifactPath directory extension number = do
    let path =
            directory
                FilePath.</> show number
                FilePath.<.> extension
    exists <- doesFileExist path
    if exists
        then nextArtifactPath directory extension (number + 1)
        else pure (number, path)

artifactExtension :: Text -> String
artifactExtension contentType
    | contentType == "markdown" = "md"
    | "json" `Text.isInfixOf` contentType = "json"
    | otherwise = "txt"

truncateUtf8 :: Int -> Text -> Text
truncateUtf8 limitBytes =
    Text.decodeUtf8With lenientDecode
        . BS.take limitBytes
        . Text.encodeUtf8

validatePolicy
    :: WebFetchRuntime
    -> URI
    -> IO (Either Text HostAddress)
validatePolicy runtime uri =
    case uriAuthority uri of
        Nothing -> pure (Left "web_fetch URL has no host")
        Just authority
            | not
                (domainAllowed
                    runtime.runtimeAllowedDomains
                    (Text.pack authority.uriRegName)
                    (Text.pack uri.uriPath)) ->
                pure . Left $
                    "web_fetch domain is not allowed: "
                        <> Text.pack authority.uriRegName
            | otherwise -> checkSsrf uri

validateUri :: Text -> Either Text URI
validateUri raw
    | BS.length (Text.encodeUtf8 raw) > 2000 =
        Left "web_fetch URL exceeds 2000 bytes"
    | otherwise = do
        uri <-
            maybe
                (Left "web_fetch URL is invalid")
                Right
                (parseURI (Text.unpack raw))
        unless
            (uri.uriScheme `elem` ["http:", "https:"])
            (Left
                ( "web_fetch only supports HTTP and HTTPS URLs, not "
                    <> Text.pack uri.uriScheme
                ))
        authority <-
            maybe (Left "web_fetch URL has no host") Right uri.uriAuthority
        unless
            (null authority.uriUserInfo)
            (Left "web_fetch URL must not contain credentials")
        let allowedPort = case uri.uriScheme of
                "http:" ->
                    null authority.uriPort
                        || authority.uriPort == ":80"
                "https:" ->
                    null authority.uriPort
                        || authority.uriPort == ":443"
                _ -> False
        unless allowedPort $
            Left
                "web_fetch only permits the default HTTP/HTTPS ports (80 and 443)"
        unless
            (null uri.uriFragment)
            (Left "web_fetch URL must not contain a fragment")
        let host = Text.pack authority.uriRegName
        when
            ( Text.null (Text.strip host)
                || (not ("." `Text.isInfixOf` host)
                    && not (":" `Text.isInfixOf` host)
                    && Text.toLower host /= "localhost")
            )
            (Left ("web_fetch rejects single-label host: " <> host))
        pure uri

upgradePublicHttp :: URI -> URI
upgradePublicHttp uri
    | uri.uriScheme == "http:"
    , Just authority <- uri.uriAuthority
    , Text.toLower (Text.pack authority.uriRegName) /= "localhost" =
        uri
            { uriScheme = "https:"
            , uriAuthority =
                Just authority
                    { uriPort =
                        if authority.uriPort == ":80"
                            then ""
                            else authority.uriPort
                    }
            }
    | otherwise = uri

parseAllowedDomain :: Text -> Either Text AllowedDomain
parseAllowedDomain raw
    | Text.null stripped =
        Left "webFetch allowedDomains must not contain empty entries"
    | "://" `Text.isInfixOf` stripped
        || any (`Text.isInfixOf` stripped) ["?", "#", "@"] =
        Left
            ( "Invalid webFetch allowed domain "
                <> quote raw
                <> "; expected host or host/path"
            )
    | Text.null host =
        Left
            ( "Invalid webFetch allowed domain "
                <> quote raw
            )
    | Text.any (not . validHostCharacter) host =
        Left
            ( "Invalid webFetch allowed domain "
                <> quote raw
            )
    | otherwise =
        Right AllowedDomain
            { allowedHost = normalizeHost host
            , allowedPathPrefix =
                normalizePathPrefix <$> nonEmpty path
            }
  where
    stripped =
        Text.toLower
            . Text.dropWhileEnd (== '/')
            . Text.strip $
                raw
    (host, pathWithSlash) = Text.breakOn "/" stripped
    path = Text.dropWhile (== '/') pathWithSlash
    validHostCharacter character =
        isAlphaNum character
            || character `elem` (".-" :: String)
    nonEmpty value
        | Text.null value = Nothing
        | otherwise = Just value

domainAllowed :: [AllowedDomain] -> Text -> Text -> Bool
domainAllowed allowed host path =
    any matches allowed
  where
    normalizedHost = normalizeHost host
    normalizedPath =
        let value = Text.toLower path
        in if Text.null value then "/" else value
    matches entry
        | entry.allowedHost /= normalizedHost = False
        | otherwise = case entry.allowedPathPrefix of
            Nothing -> True
            Just prefix ->
                normalizedPath == prefix
                    || ( (prefix <> "/") `Text.isPrefixOf` normalizedPath
                       )

normalizeHost :: Text -> Text
normalizeHost =
    stripWww
        . Text.dropWhileEnd (== '.')
        . Text.toLower
        . Text.strip
  where
    stripWww value = fromMaybe value (Text.stripPrefix "www." value)

normalizePathPrefix :: Text -> Text
normalizePathPrefix value =
    "/" <> Text.dropWhile (== '/') (Text.dropWhileEnd (== '/') value)

checkSsrf :: URI -> IO (Either Text HostAddress)
checkSsrf uri =
    case uri.uriAuthority of
        Nothing -> pure (Left "web_fetch URL has no host")
        Just authority -> do
            let host = stripIpv6Brackets authority.uriRegName
                port =
                    case dropWhile (== ':') authority.uriPort of
                        "" -> if uri.uriScheme == "https:" then "443" else "80"
                        value -> value
                hints = defaultHints { addrSocketType = Stream }
            resolved <-
                tryAny (getAddrInfo (Just hints) (Just host) (Just port))
            pure case resolved of
                Left exception ->
                    Left
                        ( "web_fetch DNS resolution failed for "
                            <> Text.pack host
                            <> ": "
                            <> Text.pack (displayException exception)
                        )
                Right [] ->
                    Left
                        ( "web_fetch DNS resolution returned no addresses for "
                            <> Text.pack host
                        )
                Right addresses ->
                    case filter (isBlockedAddress . (.addrAddress)) addresses of
                        [] ->
                            case
                                [ address
                                | AddrInfo
                                    { addrAddress =
                                        SockAddrInet _ address
                                    } <- addresses
                                ]
                            of
                                address : _ -> Right address
                                [] ->
                                    Left
                                        ( "web_fetch requires a public IPv4 \
                                          \address so the validated DNS result \
                                          \can be pinned; IPv6-only hosts are \
                                          \not supported"
                                        )
                        blocked : _ ->
                            Left
                                ( "web_fetch blocked non-public address for "
                                    <> Text.pack host
                                    <> ": "
                                    <> Text.pack
                                        (show blocked.addrAddress)
                                )

stripIpv6Brackets :: String -> String
stripIpv6Brackets value =
    fromMaybe value do
        withoutOpen <- stripPrefix "[" value
        stripSuffix "]" withoutOpen
  where
    stripPrefix prefix input
        | prefix `isPrefixOfString` input =
            Just (drop (length prefix) input)
        | otherwise = Nothing
    stripSuffix suffix input
        | suffix `isSuffixOfString` input =
            Just (take (length input - length suffix) input)
        | otherwise = Nothing

isBlockedAddress :: SockAddr -> Bool
isBlockedAddress = \case
    SockAddrInet _ address ->
        isNonPublicIPv4 (hostAddressToTuple address)
    SockAddrInet6 _ _ address _ ->
        isNonPublicIPv6 (hostAddress6ToTuple address)
    SockAddrUnix _ -> True

isNonPublicIPv4 :: (Word8, Word8, Word8, Word8) -> Bool
isNonPublicIPv4 (a, b, c, d) =
    a == 0
        || a == 10
        || a == 127
        || (a == 169 && b == 254)
        || (a == 172 && b >= 16 && b <= 31)
        || (a == 192 && b == 168)
        || (a == 100 && b >= 64 && b <= 127)
        || (a == 192 && b == 0 && c == 0)
        || (a == 192 && b == 0 && c == 2)
        || (a == 198 && (b == 18 || b == 19))
        || (a == 198 && b == 51 && c == 100)
        || (a == 203 && b == 0 && c == 113)
        || a >= 224
        || (a == 255 && b == 255 && c == 255 && d == 255)

-- | Conservative public-address classifier. Currently globally routed IPv6
-- unicast lives in 2000::/3; special-purpose ranges inside it are removed.
isNonPublicIPv6
    :: (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16)
    -> Bool
isNonPublicIPv6 (a, b, c, d, e, f, g, h)
    | all (== 0) [a, b, c, d, e] && f == 0xffff =
        isNonPublicIPv4
            ( highByte g
            , lowByte g
            , highByte h
            , lowByte h
            )
    | otherwise =
        (a .&. 0xe000) /= 0x2000
            || (a == 0x2001 && (b .&. 0xfe00) == 0)
            || (a == 0x2001 && b == 0x0db8)
            || a == 0x2002
  where
    highByte value = fromIntegral (value `shiftR` 8)
    lowByte value = fromIntegral (value .&. 0xff)

isRedirectStatus :: Int -> Bool
isRedirectStatus status =
    status `elem` [301, 302, 303, 307, 308]

headerText
    :: HeaderName
    -> Text
    -> ResponseHeaders
    -> Text
headerText name fallback headers =
    maybe fallback (Text.decodeUtf8With lenientDecode) (lookup name headers)

webFetchUserAgent :: BS.ByteString
webFetchUserAgent =
    "Mozilla/5.0 (compatible; haskell-agent/1.0)"

headOr :: a -> [a] -> a
headOr fallback = \case
    [] -> fallback
    value : _ -> value

quote :: Text -> Text
quote value = "'" <> value <> "'"

isPrefixOfString :: String -> String -> Bool
isPrefixOfString prefix value =
    take (length prefix) value == prefix

isSuffixOfString :: String -> String -> Bool
isSuffixOfString suffix value =
    drop (length value - length suffix) value == suffix

-- | Strip active/irrelevant elements and preserve basic document structure as
-- markdown-ish text. The conversion is intentionally deterministic and
-- dependency-light; it is not an HTML renderer.
htmlToMarkdown :: Text -> Text
htmlToMarkdown =
    cleanRendered
        . Text.concat
        . reverse
        . (\state -> state.rendered)
        . foldl' step initialHtmlState
        . parseTags

data HtmlState = HtmlState
    { skippedDepth :: !Int
    , preDepth :: !Int
    , rendered :: ![Text]
    }

initialHtmlState :: HtmlState
initialHtmlState = HtmlState
    { skippedDepth = 0
    , preDepth = 0
    , rendered = []
    }

step :: HtmlState -> Tag Text -> HtmlState
step state = \case
    TagOpen rawName _
        | isSkippedTag name ->
            state { skippedDepth = state.skippedDepth + 1 }
        | state.skippedDepth > 0 -> state
        | name == "pre" ->
            append "\n\n```text\n" state
                { preDepth = state.preDepth + 1 }
        | Just level <- headingLevel name ->
            append ("\n\n" <> Text.replicate level "#" <> " ") state
        | name == "li" -> append "\n- " state
        | name `elem` blockOpenTags -> append "\n\n" state
        | name == "br" -> append "\n" state
        | otherwise -> state
      where
        name = Text.toLower rawName
    TagClose rawName
        | isSkippedTag name && state.skippedDepth > 0 ->
            state { skippedDepth = state.skippedDepth - 1 }
        | state.skippedDepth > 0 -> state
        | name == "pre" ->
            append "\n```\n" state
                { preDepth = max 0 (state.preDepth - 1) }
        | name `elem` blockCloseTags
            || headingLevel name /= Nothing ->
                append "\n\n" state
        | otherwise -> state
      where
        name = Text.toLower rawName
    TagText text
        | state.skippedDepth > 0 -> state
        | state.preDepth > 0 -> append text state
        | otherwise ->
            let normalized = Text.unwords (Text.words text)
            in if Text.null normalized
                then state
                else append (normalized <> " ") state
    _ -> state

append :: Text -> HtmlState -> HtmlState
append value state =
    state { rendered = value : state.rendered }

isSkippedTag :: Text -> Bool
isSkippedTag name =
    name
        `elem`
            [ "script"
            , "style"
            , "noscript"
            , "svg"
            , "iframe"
            , "object"
            , "embed"
            ]

headingLevel :: Text -> Maybe Int
headingLevel = \case
    "h1" -> Just 1
    "h2" -> Just 2
    "h3" -> Just 3
    "h4" -> Just 4
    "h5" -> Just 5
    "h6" -> Just 6
    _ -> Nothing

blockOpenTags :: [Text]
blockOpenTags =
    [ "p"
    , "div"
    , "section"
    , "article"
    , "main"
    , "header"
    , "footer"
    , "blockquote"
    , "table"
    , "tr"
    ]

blockCloseTags :: [Text]
blockCloseTags = blockOpenTags <> ["ul", "ol", "li"]

cleanRendered :: Text -> Text
cleanRendered text =
    Text.strip
        . Text.unlines
        . collapseBlankLines False
        . map Text.stripEnd
        . Text.lines $
            text
  where
    collapseBlankLines _ [] = []
    collapseBlankLines previousBlank (line : rest)
        | Text.null line =
            if previousBlank
                then collapseBlankLines True rest
                else "" : collapseBlankLines True rest
        | otherwise =
            line : collapseBlankLines False rest
