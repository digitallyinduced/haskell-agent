-- | On-device Apple Intelligence titles via a small bundled Swift helper.
module Agent.CLI.AppleTitle
    ( generateAppleFoundationTitle
    , generateAppleFoundationTitleTimed
    , parseAppleAvailableJson
    , parseAppleTitleJson
    , probeAppleFoundationTitle
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently, withAsync)
import Control.Exception.Safe (tryAny)
import Control.Monad (void)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson (Value(..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as TextIO
import System.Directory (doesFileExist, findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.IO (hClose)
import qualified System.Info
import System.Posix.Signals (signalProcess, sigKILL)
import System.Process
    ( CreateProcess(..)
    , ProcessHandle
    , StdStream(CreatePipe)
    , getPid
    , proc
    , terminateProcess
    , waitForProcess
    , withCreateProcess
    )

appleTitleHelperName :: String
appleTitleHelperName = "apple-session-title"

appleTitleHelperEnv :: String
appleTitleHelperEnv = "HASKELL_AGENT_APPLE_SESSION_TITLE"

appleTitleProcessTimeoutMicros :: Int
appleTitleProcessTimeoutMicros = 20_000_000

probeTimeoutMicros :: Int
probeTimeoutMicros = 3_000_000

-- | Locate the Swift helper on macOS and confirm Apple Intelligence is available.
probeAppleFoundationTitle :: IO (Maybe FilePath)
probeAppleFoundationTitle
    | System.Info.os /= "darwin" = pure Nothing
    | otherwise =
        resolveAppleSessionTitleHelper >>= \case
            Nothing -> pure Nothing
            Just executable -> do
                result <-
                    runProcessTimed
                        probeTimeoutMicros
                        executable
                        ["--available"]
                        Nothing
                pure $ case result of
                    Just (ExitSuccess, out, err)
                        | parseAppleAvailableJson (out <> "\n" <> err) ->
                            Just executable
                    _ ->
                        Nothing

-- | The Nix package and the macOS bundle install this binary. The CLI does not
-- compile it.
resolveAppleSessionTitleHelper :: IO (Maybe FilePath)
resolveAppleSessionTitleHelper = do
    envPath <- lookupEnv appleTitleHelperEnv
    case envPath of
        Just path | not (null path) -> do
            exists <- doesFileExist path
            if exists then pure (Just path) else findOnPath
        _ ->
            findOnPath
  where
    findOnPath = findExecutable appleTitleHelperName

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
generateAppleFoundationTitleTimed timeoutMicros executable conversation = do
    result <-
        runProcessTimed
            timeoutMicros
            executable
            []
            (Just conversation)
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

parseAppleAvailableJson :: Text -> Bool
parseAppleAvailableJson text =
    any availableTrue (jsonCandidates text)
  where
    availableTrue candidate =
        case Aeson.decodeStrict (Text.encodeUtf8 candidate) of
            Just (Object object) ->
                case KeyMap.lookup "available" object of
                    Just (Bool True) -> True
                    _ -> False
            _ ->
                False

parseAppleTitleJson :: Text -> Maybe Text
parseAppleTitleJson raw =
    let stripped = Text.strip raw
        unfenced = stripJsonFence stripped
    in case Aeson.decodeStrict (Text.encodeUtf8 unfenced) of
        Just (Object object) ->
            case KeyMap.lookup "title" object of
                Just (String title) -> nonEmptyTitle title
                _ -> Nothing
        _ ->
            nonEmptyTitle =<< dropTitleLabel stripped
  where
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

jsonCandidates :: Text -> [Text]
jsonCandidates text =
    let stripped = Text.strip text
        lines_ = filter (not . Text.null) (map Text.strip (Text.lines stripped))
    in stripped : lines_

runProcessTimed
    :: Int
    -> FilePath
    -> [String]
    -> Maybe Text
    -> IO (Maybe (ExitCode, Text, Text))
runProcessTimed timeoutMicros executable arguments stdinText =
    runCreateProcessTimed
        timeoutMicros
        (proc executable arguments)
            { std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            }
        stdinText

runCreateProcessTimed
    :: Int
    -> CreateProcess
    -> Maybe Text
    -> IO (Maybe (ExitCode, Text, Text))
runCreateProcessTimed timeoutMicros process stdinText =
    withCreateProcess process
        \input output errors child ->
            case (output, errors) of
                (Just stdoutHandle, Just stderrHandle) -> do
                    timedOut <- newIORef False
                    let feedStdin =
                            case input of
                                Just handle -> do
                                    mapM_ (TextIO.hPutStr handle) stdinText
                                    hClose handle
                                Nothing ->
                                    pure ()
                        collect = do
                            feedStdin
                            (out, err) <- concurrently
                                (TextIO.hGetContents stdoutHandle)
                                (TextIO.hGetContents stderrHandle)
                            code <- waitForProcess child
                            pure (code, out, err)
                    withAsync
                        (threadDelay timeoutMicros >> do
                            writeIORef timedOut True
                            killTimedOutChild child)
                        \_ -> do
                            result <- collect
                            didTimeout <- readIORef timedOut
                            pure $ if didTimeout then Nothing else Just result
                _ ->
                    pure Nothing

killTimedOutChild :: ProcessHandle -> IO ()
killTimedOutChild child = do
    _ <- tryAny (terminateProcess child)
    threadDelay 200_000
    getPid child >>= \case
        Just processId ->
            void $ tryAny (signalProcess sigKILL processId)
        Nothing ->
            pure ()
