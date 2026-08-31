module Agent.CLI.BrowserTools
    ( BrowserCommand(..)
    , BrowserCommandHandler
    , browserTools
    ) where

import Agent.ToolArgs
    ( Object
    , objectArgs
    , optBool
    , reqText
    )
import Agent.Json.Decode (Decoder, FieldsDecoder)
import qualified Agent.Json.Decode as Json
import Agent.ToolDSL
    ( PropertySchema(..)
    , PropertyType(..)
    )
import Agent.ToolDispatch
    ( noArgsTool
    , typedTool
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
    deriving (Eq, Show)

type BrowserCommandHandler =
    BrowserCommand -> IO (Either Text Text)

browserTools :: BrowserCommandHandler -> [AppTool]
browserTools handler =
    [ navigateTool
    , snapshotTool
    , clickTool
    , typeTool
    , keyTool
    , scrollTool
    , noArgumentTool
        "browser_back"
        "Navigate the active browser view back one page."
        AlwaysReadOnly
        BrowserBack
    , noArgumentTool
        "browser_forward"
        "Navigate the active browser view forward one page."
        AlwaysReadOnly
        BrowserForward
    , noArgumentTool
        "browser_reload"
        "Reload the current page in the active browser view."
        AlwaysReadOnly
        BrowserReload
    ]
  where
    navigateTool = jsonAppToolWithExecution
        "browser_navigate"
        "Navigate the active browser view to an absolute HTTP or HTTPS URL."
        [ PropertySchema "url" PropertyString True $ Just
            "Absolute HTTP or HTTPS URL to open."
        ]
        AlwaysReadOnly
        TurnSequential
        (typedTool "browser_navigate" navigateArgsDecoder runNavigate)

    snapshotTool = noArgumentTool
        "browser_snapshot"
        "Read the active page's URL, title, visible text, and interactive controls. Controls include CSS selectors accepted by browser_click and browser_type."
        AlwaysReadOnly
        BrowserSnapshot

    clickTool = jsonAppToolWithExecution
        "browser_click"
        "Click an element in the active browser view using a CSS selector from browser_snapshot."
        [ PropertySchema "selector" PropertyString True $ Just
            "CSS selector identifying the element to click."
        ]
        AlwaysPrompt
        TurnSequential
        (typedTool "browser_click" clickArgsDecoder runClick)

    typeTool = jsonAppToolWithExecution
        "browser_type"
        "Replace the contents of a text control in the active browser view, optionally submitting its form."
        [ PropertySchema "selector" PropertyString True $ Just
            "CSS selector identifying an input, textarea, or content-editable element."
        , PropertySchema "text" PropertyString True $ Just
            "Text to enter."
        , PropertySchema "submit" PropertyBoolean False $ Just
            "Whether to submit the form after typing. Defaults to false."
        ]
        AlwaysPrompt
        TurnSequential
        (typedTool "browser_type" typeArgsDecoder runType)

    keyTool = jsonAppToolWithExecution
        "browser_key"
        "Send a keyboard key to the focused element in the active browser view."
        [ PropertySchema "key" PropertyString True $ Just
            "DOM KeyboardEvent.key value such as Enter, Escape, Tab, ArrowDown, or a single character."
        ]
        AlwaysPrompt
        TurnSequential
        (typedTool "browser_key" keyArgsDecoder runKey)

    scrollTool = jsonAppToolWithExecution
        "browser_scroll"
        "Scroll the active browser view by bounded horizontal and vertical CSS-pixel deltas."
        [ PropertySchema "delta_x" PropertyNumber True $ Just
            "Horizontal delta in CSS pixels; negative scrolls left."
        , PropertySchema "delta_y" PropertyNumber True $ Just
            "Vertical delta in CSS pixels; negative scrolls up."
        ]
        AlwaysReadOnly
        TurnSequential
        (typedTool "browser_scroll" scrollArgsDecoder runScroll)

    runNavigate :: NavigateArgs -> IO (Either Text Text)
    runNavigate (NavigateArgs url) =
        handler (BrowserNavigate url)

    runClick :: ClickArgs -> IO (Either Text Text)
    runClick (ClickArgs selector) =
        handler (BrowserClick selector)

    runType :: TypeArgs -> IO (Either Text Text)
    runType (TypeArgs selector text submit) =
        handler (BrowserType selector text submit)

    runKey :: KeyArgs -> IO (Either Text Text)
    runKey (KeyArgs key) =
        handler (BrowserKey key)

    runScroll :: ScrollArgs -> IO (Either Text Text)
    runScroll (ScrollArgs deltaX deltaY) =
        handler (BrowserScroll deltaX deltaY)

    noArgumentTool name description approval command =
        jsonAppToolWithExecution
            name
            description
            []
            approval
            TurnSequential
            (noArgsTool name (handler command))

newtype NavigateArgs = NavigateArgs Text

navigateArgsDecoder :: Decoder NavigateArgs
navigateArgsDecoder = objectArgs \input -> do
        url <- reqText input "url"
        NavigateArgs <$> validateOrFail (validateHttpUrl url)

newtype ClickArgs = ClickArgs Text

clickArgsDecoder :: Decoder ClickArgs
clickArgsDecoder = objectArgs \input -> do
        selector <- reqText input "selector"
        ClickArgs <$> validateOrFail
            (validateNonBlank "selector" maxSelectorBytes selector)

data TypeArgs = TypeArgs !Text !Text !Bool

typeArgsDecoder :: Decoder TypeArgs
typeArgsDecoder = objectArgs \input -> do
        rawSelector <- reqText input "selector"
        selector <- validateOrFail
            (validateNonBlank "selector" maxSelectorBytes rawSelector)
        rawText <- reqText input "text"
        text <- validateOrFail
            (validateByteLength "text" maxTextBytes rawText)
        submit <- maybe False id <$> optBool input "submit"
        pure (TypeArgs selector text submit)

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

maxUrlBytes, maxSelectorBytes, maxTextBytes, maxKeyBytes :: Int
maxUrlBytes = 8192
maxSelectorBytes = 4096
maxTextBytes = 64 * 1024
maxKeyBytes = 128

maxScrollDelta :: Double
maxScrollDelta = 10000
