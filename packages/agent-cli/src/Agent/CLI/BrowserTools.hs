module Agent.CLI.BrowserTools
    ( BrowserCommand(..)
    , BrowserCommandHandler
    , browserTools
    ) where

import Agent.ToolArgs
    ( objectArgs
    , optBool
    , reqText
    )
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
import Data.Aeson
    ( FromJSON(..)
    , Value(..)
    )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Object, Parser)
import Data.Maybe (isJust)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.URI
    ( URI(uriAuthority, uriScheme)
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
        (typedTool "browser_navigate" runNavigate)

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
        (typedTool "browser_click" runClick)

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
        (typedTool "browser_type" runType)

    keyTool = jsonAppToolWithExecution
        "browser_key"
        "Send a keyboard key to the focused element in the active browser view."
        [ PropertySchema "key" PropertyString True $ Just
            "DOM KeyboardEvent.key value such as Enter, Escape, Tab, ArrowDown, or a single character."
        ]
        AlwaysPrompt
        TurnSequential
        (typedTool "browser_key" runKey)

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
        (typedTool "browser_scroll" runScroll)

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

instance FromJSON NavigateArgs where
    parseJSON = objectArgs \input -> do
        url <- reqText input "url"
        NavigateArgs <$> validateHttpUrl url

newtype ClickArgs = ClickArgs Text

instance FromJSON ClickArgs where
    parseJSON = objectArgs \input -> do
        selector <- reqText input "selector"
        ClickArgs <$> validateNonBlank "selector" maxSelectorLength selector

data TypeArgs = TypeArgs !Text !Text !Bool

instance FromJSON TypeArgs where
    parseJSON = objectArgs \input -> do
        selector <- reqText input "selector"
            >>= validateNonBlank "selector" maxSelectorLength
        text <- reqText input "text"
            >>= validateLength "text" maxTextLength
        submit <- maybe False id <$> optBool input "submit"
        pure (TypeArgs selector text submit)

newtype KeyArgs = KeyArgs Text

instance FromJSON KeyArgs where
    parseJSON = objectArgs \input -> do
        key <- reqText input "key"
        KeyArgs <$> validateNonBlank "key" maxKeyLength key

data ScrollArgs = ScrollArgs !Double !Double

instance FromJSON ScrollArgs where
    parseJSON = objectArgs \input -> do
        deltaX <- requiredFiniteNumber input "delta_x"
        deltaY <- requiredFiniteNumber input "delta_y"
        if abs deltaX > maxScrollDelta || abs deltaY > maxScrollDelta
            then failParser
                "delta_x and delta_y must be between -100000 and 100000"
            else if deltaX == 0 && deltaY == 0
                then failParser
                    "at least one of delta_x and delta_y must be nonzero"
                else pure (ScrollArgs deltaX deltaY)

validateHttpUrl :: Text -> Parser Text
validateHttpUrl value = do
    url <- validateNonBlank "url" maxUrlLength value
    if Text.strip url /= url
        then invalid
        else case parseURI (Text.unpack url) of
            Just uri
                | uriScheme uri `elem` ["http:", "https:"]
                , isJust (uriAuthority uri) ->
                    pure url
            _ -> invalid
  where
    invalid = failParser "url must be an absolute HTTP or HTTPS URL"

validateNonBlank :: Text -> Int -> Text -> Parser Text
validateNonBlank name limit value
    | Text.null (Text.strip value) =
        failParser (name <> " must not be empty")
    | otherwise = validateLength name limit value

validateLength :: Text -> Int -> Text -> Parser Text
validateLength name limit value
    | Text.length value > limit =
        failParser
            (name <> " must not exceed " <> Text.pack (show limit)
                <> " characters")
    | otherwise = pure value

requiredFiniteNumber :: Object -> Text -> Parser Double
requiredFiniteNumber input name =
    case KeyMap.lookup (Key.fromText name) input of
        Just (Number scientific) ->
            let value = toRealFloat scientific
            in if isNaN value || isInfinite value
                then failParser (name <> " must be a finite number")
                else pure value
        Just _ -> failParser ("Expected number for key: " <> name)
        Nothing -> failParser ("Missing parameter: " <> name)

failParser :: Text -> Parser a
failParser = fail . Text.unpack

maxUrlLength, maxSelectorLength, maxTextLength, maxKeyLength :: Int
maxUrlLength = 8192
maxSelectorLength = 4096
maxTextLength = 256 * 1024
maxKeyLength = 128

maxScrollDelta :: Double
maxScrollDelta = 100000
