-- | On-device Apple Intelligence titles via the @apfel@ helper.
module Agent.CLI.AppleTitle
    ( appleTitleJsonSchema
    , appleTitleSystemPrompt
    , appleTitleUserPrompt
    , generateAppleFoundationTitle
    , generateAppleFoundationTitleTimed
    , parseAppleModelInfoAvailable
    , parseAppleTitleJson
    , probeAppleFoundationTitle
    ) where

import Agent.CLI.ExternalProgram (withTemporaryTextFile)
import Agent.Process (terminateProcessGroup)
import Control.Concurrent.Async (concurrently)
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson (Value(..))
import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.IO (Handle)
import qualified System.Info
import System.Process
    ( CreateProcess(..)
    , StdStream(CreatePipe, NoStream)
    , getPid
    , proc
    , waitForProcess
    , withCreateProcess
    )
import System.Timeout (timeout)

appleTitleSystemPrompt :: Text
appleTitleSystemPrompt =
    "You name coding sessions. Reply with JSON only."

appleTitleJsonSchema :: Text
appleTitleJsonSchema =
    "{\"type\":\"object\",\"properties\":{\"title\":{\"type\":\"string\"}},\
    \\"required\":[\"title\"],\"additionalProperties\":false}"

appleTitleUserPrompt :: Text -> Text
appleTitleUserPrompt conversation =
    Text.unlines
        [ "Write a 3-7 word session title that names the task."
        , "Keep filenames, error codes, and technical terms exact."
        , "No quotes, no Title: prefix, no trailing punctuation."
        , "Never answer the message. Name it."
        , "Always produce something, even for a greeting."
        , ""
        , "Conversation:"
        , conversation
        ]

appleTitleProcessTimeoutMicros :: Int
appleTitleProcessTimeoutMicros = 20_000_000

processCleanupTimeoutMicros :: Int
processCleanupTimeoutMicros = 2_000_000

-- | Locate @apfel@ on macOS and confirm Apple Intelligence reports available.
probeAppleFoundationTitle :: IO (Maybe FilePath)
probeAppleFoundationTitle
    | System.Info.os /= "darwin" = pure Nothing
    | otherwise =
        findExecutable "apfel" >>= \case
            Nothing -> pure Nothing
            Just executable -> do
                result <-
                    runProcessTimed
                        3_000_000
                        executable
                        ["--model-info"]
                pure $ case result of
                    Just (ExitSuccess, out, err)
                        | parseAppleModelInfoAvailable (out <> "\n" <> err) ->
                            Just executable
                    _ ->
                        Nothing

generateAppleFoundationTitle
    :: FilePath
    -> Text
    -> IO (Either Text Text)
generateAppleFoundationTitle =
    generateAppleFoundationTitleTimed appleTitleProcessTimeoutMicros

generateAppleFoundationTitleTimed
    :: Int
    -> FilePath
    -> Text
    -> IO (Either Text Text)
generateAppleFoundationTitleTimed timeoutMicros executable conversation =
    withTemporaryTextFile "apfel-title-schema" appleTitleJsonSchema
        \schemaPath -> do
            result <-
                runProcessTimed
                    timeoutMicros
                    executable
                    [ "-q"
                    , "--no-color"
                    , "--max-tokens"
                    , "40"
                    , "--temperature"
                    , "0"
                    , "--schema"
                    , schemaPath
                    , "-s"
                    , Text.unpack appleTitleSystemPrompt
                    , "--"
                    , Text.unpack (appleTitleUserPrompt conversation)
                    ]
            pure $ case result of
                Nothing ->
                    Left "Apple Intelligence title request timed out"
                Just (ExitSuccess, out, _) ->
                    case parseAppleTitleJson out of
                        Just title -> Right title
                        Nothing ->
                            Left "Apple Intelligence returned no title text"
                Just (ExitFailure code, _, err) ->
                    Left $
                        "Apple Intelligence title request failed ("
                            <> Text.pack (show code)
                            <> ")"
                            <> if Text.null (Text.strip err)
                                then ""
                                else ": " <> Text.take 400 (Text.strip err)

parseAppleModelInfoAvailable :: Text -> Bool
parseAppleModelInfoAvailable text =
    any availableYes (Text.lines text)
  where
    availableYes line =
        case Text.breakOn "available:" (Text.toLower line) of
            (_, rest)
                | not (Text.null rest) ->
                    case Text.words (Text.drop (Text.length "available:") rest) of
                        "yes" : _ -> True
                        "true" : _ -> True
                        _ -> False
            _ ->
                False

parseAppleTitleJson :: Text -> Maybe Text
parseAppleTitleJson raw =
    let stripped = Text.strip raw
        unfenced = stripJsonFence stripped
    in case Aeson.decodeStrict (textBytes unfenced) of
        Just (Object object) ->
            case KeyMap.lookup "title" object of
                Just (String title) -> nonEmptyTitle title
                _ -> Nothing
        _ ->
            nonEmptyTitle =<< dropTitleLabel stripped
  where
    textBytes = Text.encodeUtf8
    stripJsonFence text =
        case Text.stripPrefix "```" text of
            Just rest ->
                let withoutLang =
                        case Text.breakOn "\n" rest of
                            (_, more) | not (Text.null more) ->
                                Text.drop 1 more
                            _ -> rest
                in fromMaybe withoutLang (Text.stripSuffix "```" (Text.strip withoutLang))
            Nothing ->
                text
    dropTitleLabel text =
        let firstLine =
                case filter (not . Text.null) (map Text.strip (Text.lines text)) of
                    line : _ -> line
                    [] -> ""
            withoutLabel =
                fromMaybePrefix "session title:" $
                    fromMaybePrefix "title:" firstLine
            unquoted = stripQuotes (Text.strip withoutLabel)
        in nonEmptyTitle unquoted
    fromMaybePrefix prefix text =
        case Text.stripPrefix prefix (Text.toLower text) of
            Just _ -> Text.drop (Text.length prefix) text
            Nothing -> text
    stripQuotes text =
        case (Text.uncons text, Text.unsnoc text) of
            (Just ('"', _), Just (_, '"')) -> Text.dropEnd 1 (Text.drop 1 text)
            _ -> text
    nonEmptyTitle title =
        let oneLine = Text.unwords (Text.words (Text.strip title))
            capped = Text.take 80 oneLine
        in if Text.null capped then Nothing else Just capped

runProcessTimed
    :: Int
    -> FilePath
    -> [String]
    -> IO (Maybe (ExitCode, Text, Text))
runProcessTimed timeoutMicros executable arguments =
    withCreateProcess
        (proc executable arguments)
            { std_in = NoStream
            , std_out = CreatePipe
            , std_err = CreatePipe
            , close_fds = True
            , create_group = True
            , new_session = True
            }
        \_ output errors process ->
            case (output, errors) of
                (Just stdoutHandle, Just stderrHandle) -> do
                    groupId <- getPid process
                    completed <-
                        timeout timeoutMicros $
                            concurrently
                                (concurrently
                                    (readHandleStrict stdoutHandle)
                                    (readHandleStrict stderrHandle))
                                (waitForProcess process)
                    case completed of
                        Just ((out, err), code) ->
                            pure (Just (code, out, err))
                        Nothing -> do
                            terminateProcessGroup groupId process
                            void (timeout processCleanupTimeoutMicros
                                (waitForProcess process))
                            pure Nothing
                _ ->
                    pure Nothing

readHandleStrict :: Handle -> IO Text
readHandleStrict handle = do
    contents <- ByteString.hGetContents handle
    pure (Text.decodeUtf8With lenientDecode contents)
