-- | Interactive handling of MCP elicitation requests: form-mode input
-- collected field by field, and URL-mode consent before opening a browser.
--
-- Both the line-oriented prompt and the fullscreen UI make clear which
-- server is asking, let the user review answers before sending, and offer
-- explicit decline and cancel choices.
module Agent.CLI.McpElicitation
    ( cliMcpElicitation
    , elicitationHost
    , renderElicitationHeader
    ) where

import Agent.CLI.CancelWatch (StdinControl, withStdinPaused)
import Agent.CLI.Input (readApprovalLine)
import Agent.CLI.Notification
    ( AttentionRequest(InputRequested)
    , notifyAttention
    )
import Agent.CLI.TUI.App
    ( FullscreenRuntime
    , requestFullscreenChoiceWithBody
    , requestFullscreenText
    )
import Agent.MCP
    ( McpElicitMode(..)
    , McpElicitRequest(..)
    , McpElicitResult(..)
    )
import Agent.MCP.Elicitation
    ( McpFieldKind(..)
    , McpFormField(..)
    , McpEnumOption(..)
    , describeFieldKind
    , encodeFormContent
    , fieldDefaultText
    , parseElicitForm
    , parseFieldAnswer
    )
import Control.Exception.Safe (tryAny, tryIO)
import Control.Monad (forM_)
import Data.Aeson (Value(..))
import Data.Char (isControl)
import Data.IORef (IORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Exit (ExitCode(..))
import System.IO
    ( hFlush
    , hIsTerminalDevice
    , stderr
    , stdin
    )
import System.Process (rawSystem)

-- | Elicitation hook for the CLI. Uses the fullscreen UI when one is active
-- and the terminal otherwise; without a terminal the request is cancelled.
cliMcpElicitation
    :: StdinControl
    -> IORef (Maybe FullscreenRuntime)
    -> McpElicitRequest
    -> IO McpElicitResult
cliMcpElicitation stdinControl runtimeRef request = do
    runtime <- readIORef runtimeRef
    case runtime of
        Just active -> fullscreenElicitation active request
        Nothing -> lineElicitation stdinControl request

-- | The host component of a URL, shown prominently so users can judge where
-- a URL-mode elicitation leads.
elicitationHost :: Text -> Text
elicitationHost url =
    let afterScheme = case Text.breakOn "://" url of
            (_, rest) | not (Text.null rest) -> Text.drop 3 rest
            _ -> url
    in Text.takeWhile (\character -> character /= '/' && character /= '?' && character /= '#') afterScheme

renderElicitationHeader :: McpElicitRequest -> Text
renderElicitationHeader request =
    "MCP server \"" <> sanitize request.elicitServerName <> "\" requests input"
        <> (if Text.null (Text.strip request.elicitMessage)
            then ""
            else "\n" <> sanitize (Text.strip request.elicitMessage))

sanitize :: Text -> Text
sanitize = Text.map (\character -> if isControl character && character /= '\n' then ' ' else character)

-- * Line mode

lineElicitation :: StdinControl -> McpElicitRequest -> IO McpElicitResult
lineElicitation stdinControl request =
    withStdinPaused stdinControl do
        tty <- hIsTerminalDevice stdin
        if not tty
            then pure McpElicitCancel
            else do
                notifyAttention stderr InputRequested
                Text.hPutStrLn stderr ""
                Text.hPutStrLn stderr (renderElicitationHeader request)
                case request.elicitMode of
                    McpElicitUrl url -> lineUrlConsent url
                    McpElicitForm schema ->
                        case parseElicitForm schema of
                            Left err -> do
                                Text.hPutStrLn stderr
                                    ("Cannot render the requested form: " <> err)
                                pure McpElicitCancel
                            Right fields -> lineForm fields

lineUrlConsent :: Text -> IO McpElicitResult
lineUrlConsent url = do
    Text.hPutStrLn stderr ("URL: " <> sanitize url)
    Text.hPutStrLn stderr ("Host: " <> elicitationHost url)
    Text.hPutStrLn stderr
        "The page opens in your browser; nothing you enter there is visible to the agent."
    answer <- readApprovalLine "Open it? [y]es / [n]o (decline) / [c]ancel: "
    case fmap (Text.toLower . Text.strip) answer of
        Just value
            | value `elem` ["y", "yes"] -> do
                opened <- openBrowser url
                if opened
                    then pure (McpElicitAccept Nothing)
                    else do
                        Text.hPutStrLn stderr
                            "Could not open a browser; open the URL manually, then continue."
                        pure (McpElicitAccept Nothing)
            | value `elem` ["n", "no", "d", "decline"] -> pure McpElicitDecline
        _ -> pure McpElicitCancel

lineForm :: [McpFormField] -> IO McpElicitResult
lineForm fields = do
    Text.hPutStrLn stderr
        "Answer each field (Enter keeps the default or skips an optional field; type !decline or !cancel to stop)."
    collect fields [] >>= \case
        Left result -> pure result
        Right answers -> do
            Text.hPutStrLn stderr "Answers to send:"
            forM_ answers \(name, value) ->
                Text.hPutStrLn stderr ("  " <> name <> ": " <> renderValue value)
            confirmation <- readApprovalLine "Send these answers? [y]es / [n]o (decline) / [c]ancel: "
            case fmap (Text.toLower . Text.strip) confirmation of
                Just value
                    | value `elem` ["y", "yes"] ->
                        pure (McpElicitAccept (Just (encodeFormContent answers)))
                    | value `elem` ["n", "no", "d", "decline"] -> pure McpElicitDecline
                _ -> pure McpElicitCancel
  where
    collect [] answers = pure (Right (reverse answers))
    collect (field : rest) answers = askField field (3 :: Int) >>= \case
        Left result -> pure (Left result)
        Right Nothing -> collect rest answers
        Right (Just value) -> collect rest ((field.fieldName, value) : answers)

    askField field attempts = do
        Text.hPutStrLn stderr ""
        Text.hPutStrLn stderr (fieldHeading field)
        forM_ field.fieldDescription \description ->
            Text.hPutStrLn stderr ("  " <> sanitize description)
        Text.hPutStrLn stderr ("  expects: " <> describeFieldKind field)
        case field.fieldKind of
            McpEnumField options _ _ _ ->
                forM_ (zip [1 :: Int ..] options) \(index, option) ->
                    Text.hPutStrLn stderr
                        ("    " <> Text.pack (show index) <> ") "
                            <> (if option.optionTitle == option.optionValue
                                then option.optionValue
                                else option.optionTitle <> " (" <> option.optionValue <> ")"))
            _ -> pure ()
        Text.hPutStr stderr
            ("  " <> field.fieldName
                <> maybe "" (\value -> " [" <> value <> "]") (fieldDefaultText field)
                <> "> ")
        hFlush stderr
        line <- tryIO Text.getLine
        case line of
            Left _ -> pure (Left McpElicitCancel)
            Right raw
                | Text.strip raw == "!cancel" -> pure (Left McpElicitCancel)
                | Text.strip raw == "!decline" -> pure (Left McpElicitDecline)
                | otherwise ->
                    case parseFieldAnswer field raw of
                        Right value -> pure (Right value)
                        Left err
                            | attempts <= 1 -> do
                                Text.hPutStrLn stderr ("  " <> err <> "; cancelling")
                                pure (Left McpElicitCancel)
                            | otherwise -> do
                                Text.hPutStrLn stderr ("  " <> err)
                                askField field (attempts - 1)

fieldHeading :: McpFormField -> Text
fieldHeading field =
    "* " <> sanitize (fromMaybe field.fieldName field.fieldTitle)
        <> (if field.fieldRequired then " (required)" else " (optional)")

renderValue :: Value -> Text
renderValue = \case
    String text -> text
    Number number -> Text.pack (show number)
    Bool flag -> if flag then "yes" else "no"
    Array values -> Text.intercalate ", " (map renderValue (foldr (:) [] values))
    Null -> "null"
    Object _ -> "{…}"

-- * Fullscreen mode

fullscreenElicitation :: FullscreenRuntime -> McpElicitRequest -> IO McpElicitResult
fullscreenElicitation runtime request =
    case request.elicitMode of
        McpElicitUrl url -> do
            choice <-
                requestFullscreenChoiceWithBody
                    runtime
                    "MCP server requests a URL visit"
                    (renderElicitationHeader request
                        <> "\n\nURL: " <> sanitize url
                        <> "\nHost: " <> elicitationHost url
                        <> "\n\nThe page opens in your browser; nothing you enter there is visible to the agent.")
                    1
                    [ ("Open in browser", "Consent and open the URL")
                    , ("Decline", "Tell the server you will not visit the URL")
                    , ("Cancel", "Dismiss without answering")
                    ]
            case choice of
                Just 0 -> do
                    _ <- openBrowser url
                    pure (McpElicitAccept Nothing)
                Just 1 -> pure McpElicitDecline
                _ -> pure McpElicitCancel
        McpElicitForm schema ->
            case parseElicitForm schema of
                Left _ -> pure McpElicitCancel
                Right fields -> collect fields [] >>= \case
                    Left result -> pure result
                    Right answers -> do
                        choice <-
                            requestFullscreenChoiceWithBody
                                runtime
                                ("Send answers to " <> sanitize request.elicitServerName <> "?")
                                (Text.unlines
                                    [ name <> ": " <> renderValue value
                                    | (name, value) <- answers
                                    ])
                                0
                                [ ("Send", "Return these answers to the server")
                                , ("Decline", "Refuse the request")
                                , ("Cancel", "Dismiss without answering")
                                ]
                        pure case choice of
                            Just 0 -> McpElicitAccept (Just (encodeFormContent answers))
                            Just 1 -> McpElicitDecline
                            _ -> McpElicitCancel
  where
    collect [] answers = pure (Right (reverse answers))
    collect (field : rest) answers = askField field Nothing (3 :: Int) >>= \case
        Left result -> pure (Left result)
        Right Nothing -> collect rest answers
        Right (Just value) -> collect rest ((field.fieldName, value) : answers)

    askField field problem attempts = do
        let body =
                Text.intercalate "\n" $
                    filter (not . Text.null)
                        [ renderElicitationHeader request
                        , ""
                        , fieldHeading field
                        , maybe "" sanitize field.fieldDescription
                        , "Expects: " <> describeFieldKind field
                        , maybe "" ("Problem: " <>) problem
                        ]
        answer <- case field.fieldKind of
            McpEnumField options False _ _ | not (null options) -> do
                let rows =
                        [ (option.optionTitle, if option.optionTitle == option.optionValue then "" else option.optionValue)
                        | option <- options
                        ]
                        <> [("Skip", "Leave this field empty") | not field.fieldRequired]
                        <> [("Cancel", "Dismiss the request")]
                choice <- requestFullscreenChoiceWithBody runtime (fieldHeading field) body 0 rows
                pure $ case choice of
                    Just index
                        | index < length options -> Just (options !! index).optionValue
                        | not field.fieldRequired && index == length options -> Just ""
                    _ -> Nothing
            McpBooleanField -> do
                choice <- requestFullscreenChoiceWithBody runtime (fieldHeading field) body 0
                    [("Yes", ""), ("No", ""), ("Cancel", "Dismiss the request")]
                pure $ case choice of
                    Just 0 -> Just "yes"
                    Just 1 -> Just "no"
                    _ -> Nothing
            _ -> requestFullscreenText runtime (fieldHeading field) body
                    (fromMaybe "" (fieldDefaultText field))
        case answer of
            Nothing -> pure (Left McpElicitCancel)
            Just text -> case parseFieldAnswer field text of
                Right value -> pure (Right value)
                Left err
                    | attempts <= 1 -> pure (Left McpElicitCancel)
                    | otherwise -> askField field (Just err) (attempts - 1)

openBrowser :: Text -> IO Bool
openBrowser url = do
    result <- tryAny (rawSystem "open" [Text.unpack url])
    case result of
        Right ExitSuccess -> pure True
        _ -> do
            fallback <- tryAny (rawSystem "xdg-open" [Text.unpack url])
            pure (case fallback of Right ExitSuccess -> True; _ -> False)
