module Agent.CLI.BrowserTools
    ( BrowserCommand(..)
    , BrowserInvocation(..)
    , BrowserCommandHandler
    , browserTools
    ) where

import Agent.Json.Decode (Decoder, FieldsDecoder)
import qualified Agent.Json.Decode as Json
import Agent.ToolArgs
    ( Object
    , objectArgs
    , optBool
    , reqText
    )
import Agent.ToolDSL
    ( PropertySchema(..)
    , PropertyType(..)
    )
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolHandlerResult
    , typedRichToolWithCall
    )
import Agent.Tools.Types
    ( AppTool
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , jsonAppToolWithExecution
    )
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Network.URI
    ( URI(uriAuthority, uriScheme)
    , URIAuth(uriRegName, uriUserInfo)
    , parseURI
    )

data BrowserCommand
    = BrowserNavigate !Text
    | BrowserSnapshot
    | BrowserClick !Text
    | BrowserType !Text !Text !Bool
    | BrowserKey !Text
    | BrowserScroll !Double !Double
    | BrowserBack
    | BrowserForward
    | BrowserReload
    | BrowserScreenshot
    | BrowserListTabs
    | BrowserSwitchTab !Text
    | BrowserListDownloads
    deriving (Eq, Show)

data BrowserInvocation = BrowserInvocation
    { browserScopeId :: !Text
    , browserCallId :: !Text
    , browserCommand :: !BrowserCommand
    } deriving (Eq, Show)

type BrowserCommandHandler =
    BrowserInvocation -> IO (Either Text ToolHandlerResult)

browserTools :: Text -> BrowserCommandHandler -> [AppTool]
browserTools scopeId handler =
    [ argumentTool
        "browser_navigate"
        "Navigate the active agent-owned browser tab to an absolute HTTP or HTTPS URL. A navigation invalidates element refs."
        [ PropertySchema "url" PropertyString True $ Just
            "Absolute HTTP or HTTPS URL to open."
        ]
        AlwaysPrompt
        navigateArgsDecoder
        \(NavigateArgs url) -> BrowserNavigate url
    , noArgumentTool
        "browser_snapshot"
        "Read the active agent-owned tab, including frame-labelled visible text and interactive controls. Controls carry opaque refs valid only for the latest snapshot; take a new snapshot after navigation, tab changes, or actions."
        AlwaysReadOnly
        BrowserSnapshot
    , argumentTool
        "browser_click"
        "Click an interactive element using an opaque ref from the latest browser_snapshot. Stale refs are rejected."
        [ PropertySchema "ref" PropertyString True $ Just
            "Opaque element ref from the latest browser_snapshot."
        ]
        AlwaysPrompt
        clickArgsDecoder
        \(ClickArgs ref) -> BrowserClick ref
    , argumentTool
        "browser_type"
        "Replace the contents of an editable control identified by a ref from the latest browser_snapshot, optionally pressing Enter afterward."
        [ PropertySchema "ref" PropertyString True $ Just
            "Opaque editable-element ref from the latest browser_snapshot."
        , PropertySchema "text" PropertyString True $ Just
            "Text to enter."
        , PropertySchema "submit" PropertyBoolean False $ Just
            "Whether to press Enter after typing. Defaults to false."
        ]
        AlwaysPrompt
        typeArgsDecoder
        \(TypeArgs ref text submit) -> BrowserType ref text submit
    , argumentTool
        "browser_key"
        "Send a trusted keyboard key to the focused control in the active agent-owned browser tab."
        [ PropertySchema "key" PropertyString True $ Just
            "Key value such as Enter, Escape, Tab, ArrowDown, or a single character."
        ]
        AlwaysPrompt
        keyArgsDecoder
        \(KeyArgs key) -> BrowserKey key
    , argumentTool
        "browser_scroll"
        "Scroll the active agent-owned browser viewport by bounded horizontal and vertical CSS-pixel deltas."
        [ PropertySchema "delta_x" PropertyNumber True $ Just
            "Horizontal delta in CSS pixels; negative scrolls left."
        , PropertySchema "delta_y" PropertyNumber True $ Just
            "Vertical delta in CSS pixels; negative scrolls up."
        ]
        AlwaysReadOnly
        scrollArgsDecoder
        \(ScrollArgs deltaX deltaY) -> BrowserScroll deltaX deltaY
    , noArgumentTool
        "browser_back"
        "Navigate the active agent-owned browser tab back one page. This invalidates element refs."
        AlwaysPrompt
        BrowserBack
    , noArgumentTool
        "browser_forward"
        "Navigate the active agent-owned browser tab forward one page. This invalidates element refs."
        AlwaysPrompt
        BrowserForward
    , noArgumentTool
        "browser_reload"
        "Reload the active agent-owned browser tab. This invalidates element refs."
        AlwaysPrompt
        BrowserReload
    , noArgumentTool
        "browser_screenshot"
        "Capture the content viewport of the active agent-owned browser tab. The image is returned separately from the textual result and excludes browser chrome."
        AlwaysReadOnly
        BrowserScreenshot
    , noArgumentTool
        "browser_list_tabs"
        "List agent-owned browser tabs with opaque tab IDs, titles, URLs, and active state. Unrelated user tabs are never included."
        AlwaysReadOnly
        BrowserListTabs
    , argumentTool
        "browser_switch_tab"
        "Switch to an agent-owned browser tab. Switching invalidates element refs; take a new browser_snapshot afterward."
        [ PropertySchema "tab_id" PropertyString True $ Just
            "Opaque tab ID returned by browser_list_tabs."
        ]
        AlwaysReadOnly
        switchTabArgsDecoder
        \(SwitchTabArgs tabId) -> BrowserSwitchTab tabId
    , noArgumentTool
        "browser_list_downloads"
        "List downloads attributed to approved browser actions in the current browser session. Unrelated filesystem downloads are never included."
        AlwaysReadOnly
        BrowserListDownloads
    ]
  where
    runInvocation call command =
        handler BrowserInvocation
            { browserScopeId = scopeId
            , browserCallId = call.callId
            , browserCommand = command
            }

    argumentTool
        :: Text
        -> Text
        -> [PropertySchema]
        -> ApprovalRule
        -> Decoder args
        -> (args -> BrowserCommand)
        -> AppTool
    argumentTool name description properties approval decoder toCommand =
        jsonAppToolWithExecution
            name
            description
            properties
            approval
            TurnSequential
            (typedRichToolWithCall name decoder \call args ->
                runInvocation call (toCommand args))

    noArgumentTool
        :: Text
        -> Text
        -> ApprovalRule
        -> BrowserCommand
        -> AppTool
    noArgumentTool name description approval command =
        argumentTool
            name
            description
            []
            approval
            emptyArgsDecoder
            (const command)

data EmptyArgs = EmptyArgs

emptyArgsDecoder :: Decoder EmptyArgs
emptyArgsDecoder = objectArgs \_ -> pure EmptyArgs

newtype NavigateArgs = NavigateArgs Text

navigateArgsDecoder :: Decoder NavigateArgs
navigateArgsDecoder = objectArgs \input -> do
    url <- reqText input "url"
    NavigateArgs <$> validateOrFail (validateHttpUrl url)

newtype ClickArgs = ClickArgs Text

clickArgsDecoder :: Decoder ClickArgs
clickArgsDecoder = objectArgs \input -> do
    ref <- reqText input "ref"
    ClickArgs <$> validateOrFail
        (validateNonBlank "ref" maxRefBytes ref)

data TypeArgs = TypeArgs !Text !Text !Bool

typeArgsDecoder :: Decoder TypeArgs
typeArgsDecoder = objectArgs \input -> do
    rawRef <- reqText input "ref"
    ref <- validateOrFail
        (validateNonBlank "ref" maxRefBytes rawRef)
    rawText <- reqText input "text"
    text <- validateOrFail
        (validateByteLength "text" maxTextBytes rawText)
    submit <- maybe False id <$> optBool input "submit"
    pure (TypeArgs ref text submit)

newtype KeyArgs = KeyArgs Text

keyArgsDecoder :: Decoder KeyArgs
keyArgsDecoder = objectArgs \input -> do
    key <- reqText input "key"
    KeyArgs <$> validateOrFail
        (validateNonBlank "key" maxKeyBytes key)

data ScrollArgs = ScrollArgs !Double !Double

scrollArgsDecoder :: Decoder ScrollArgs
scrollArgsDecoder = objectArgs \input -> do
    deltaX <- requiredFiniteNumber input "delta_x"
    deltaY <- requiredFiniteNumber input "delta_y"
    if abs deltaX > maxScrollDelta || abs deltaY > maxScrollDelta
        then failText
            "delta_x and delta_y must be between -10000 and 10000"
        else if deltaX == 0 && deltaY == 0
            then failText
                "at least one of delta_x and delta_y must be nonzero"
            else pure (ScrollArgs deltaX deltaY)

newtype SwitchTabArgs = SwitchTabArgs Text

switchTabArgsDecoder :: Decoder SwitchTabArgs
switchTabArgsDecoder = objectArgs \input -> do
    tabId <- reqText input "tab_id"
    SwitchTabArgs <$> validateOrFail
        (validateNonBlank "tab_id" maxTabIdBytes tabId)

validateHttpUrl :: Text -> Either Text Text
validateHttpUrl value = do
    url <- validateNonBlank "url" maxUrlBytes value
    if Text.strip url /= url
        then invalid
        else case parseURI (Text.unpack url) of
            Just uri
                | Text.toCaseFold (Text.pack (uriScheme uri))
                    `elem` ["http:", "https:"]
                , Just authority <- uriAuthority uri
                , not (null (uriRegName authority)) ->
                    if null (uriUserInfo authority)
                        then pure url
                        else Left
                            "url must not contain embedded credentials"
            _ -> invalid
  where
    invalid = Left "url must be an absolute HTTP or HTTPS URL"

validateNonBlank :: Text -> Int -> Text -> Either Text Text
validateNonBlank name limit value
    | Text.null (Text.strip value) =
        Left (name <> " must not be empty")
    | otherwise = validateByteLength name limit value

validateByteLength :: Text -> Int -> Text -> Either Text Text
validateByteLength name limit value
    | BS.length (TextEncoding.encodeUtf8 value) > limit =
        Left
            (name <> " must not exceed " <> Text.pack (show limit)
                <> " UTF-8 bytes")
    | otherwise = pure value

requiredFiniteNumber :: Object -> Text -> FieldsDecoder Double
requiredFiniteNumber _ name = do
    value <- Json.atKey name Json.double
    if isNaN value || isInfinite value
        then failText (name <> " must be a finite number")
        else pure value

validateOrFail :: Either Text value -> FieldsDecoder value
validateOrFail = either failText pure

failText :: MonadFail parser => Text -> parser value
failText = fail . Text.unpack

maxUrlBytes, maxRefBytes, maxTextBytes, maxKeyBytes, maxTabIdBytes :: Int
maxUrlBytes = 8192
maxRefBytes = 4096
maxTextBytes = 64 * 1024
maxKeyBytes = 128
maxTabIdBytes = 512

maxScrollDelta :: Double
maxScrollDelta = 10000
