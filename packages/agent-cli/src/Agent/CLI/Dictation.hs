-- | Terminal microphone recording and provider transcription.
module Agent.CLI.Dictation
    ( DictationBackend(..)
    , DictationControl(..)
    , DictationResult(..)
    , dictate
    , dictateForProvider
    , dictateWith
    , dictationBackendForProvider
    , insertDictation
    , transcribeAudio
    ) where

import Agent.CLI.Auth
    ( LoadedAuth(..)
    , loadAuth
    , loadOpenAiDictationAuth
    )
import Agent.CLI.Transcription (transcribeAudio)
import Agent.OpenAI.Transcription
    ( openAITranscriptionSampleRate
    , transcribePcmWithOpenAI
    )
import Agent.Provider
    ( Provider(..)
    , providerSlug
    )
import Agent.XAI.Transcription
    ( transcribePcmWithXAI
    )
import Control.Concurrent.Async
    ( wait
    , waitEitherCatch
    , withAsync
    )
import Control.Exception.Safe
    ( SomeException
    , bracket
    , displayException
    , finally
    , throwIO
    , try
    , tryAny
    )
import Control.Exception (AsyncException(UserInterrupt))
import Control.Monad (unless, void)
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory
    ( findExecutable
    )
import System.Exit (ExitCode(..))
import System.IO
    ( BufferMode(..)
    , hClose
    , hFlush
    , hGetBuffering
    , hGetChar
    , hPutStrLn
    , hSetBuffering
    , stderr
    , stdin
    )
import System.Process
    ( CreateProcess(..)
    , StdStream(..)
    , createProcess
    , proc
    , terminateProcess
    , waitForProcess
    )

data DictationControl = DictationControl
    { dictationWaitForStop :: IO ()
    , dictationOnTranscript :: Text -> IO ()
    }

data DictationResult
    = DictationTranscript !Text
    | DictationFailed !Text
    deriving (Eq, Show)

data DictationBackend
    = OpenAIDictation
    | XAIDictation
    deriving (Eq, Show)

-- | Select dictation from the active model provider. Providers without a
-- speech-to-text integration fail explicitly rather than spending credentials
-- from an unrelated provider.
dictationBackendForProvider :: Provider -> Either Text DictationBackend
dictationBackendForProvider = \case
    OpenAIProvider -> Right OpenAIDictation
    XAIProvider -> Right XAIDictation
    provider ->
        Left $
            "Dictation is not supported for "
                <> providerSlug provider
                <> " models"

-- | Legacy standalone entry point. The inline model-aware REPL and fullscreen
-- composer use 'dictateForProvider'; this preserves the original xAI default
-- for callers that have no active model context.
dictate :: IO Text
dictate = dictateForProvider XAIProvider

-- | Stream the default microphone using the active model provider.
dictateForProvider :: Provider -> IO Text
dictateForProvider provider = do
    Text.hPutStrLn stderr "● Starting dictation…"
    hFlush stderr
    result <-
        dictateWith provider
            DictationControl
                { dictationWaitForStop = do
                    Text.hPutStr stderr "● Listening… press Enter to stop"
                    hFlush stderr
                    waitForStopKey
                , dictationOnTranscript = renderLiveTranscript
                }
            `finally` clearLiveTranscript
    case result of
        DictationTranscript transcript -> pure transcript
        DictationFailed err -> fail (Text.unpack err)

-- | Record microphone audio until the caller signals stop. Providers that
-- stream partial transcripts deliver them through the control callback.
dictateWith :: Provider -> DictationControl -> IO DictationResult
dictateWith provider control =
    try run >>= \case
        Left (err :: SomeException) ->
            pure (DictationFailed (Text.pack (displayException err)))
        Right result ->
            pure result
  where
    run =
        case dictationBackendForProvider provider of
            Left err ->
                pure (DictationFailed err)
            Right backend -> do
                requireExecutable "ffmpeg"
                runBackend backend
    runBackend = \case
        OpenAIDictation ->
            loadOpenAiDictationAuth >>= \case
                Nothing ->
                    pure $ DictationFailed
                        "No OpenAI credential found for OpenAI dictation"
                Just loaded ->
                    finish =<<
                        transcribePcmWithOpenAI
                            loaded.loadedTokenProvider
                            (streamMicrophone
                                openAITranscriptionSampleRate
                                control.dictationWaitForStop)
                            control.dictationOnTranscript
        XAIDictation ->
            loadAuth (Just XAIProvider) >>= \case
                Left err ->
                    pure (DictationFailed err)
                Right loaded ->
                    finish =<<
                        transcribePcmWithXAI
                            loaded.loadedTokenProvider
                            (streamMicrophone
                                16_000
                                control.dictationWaitForStop)
                            control.dictationOnTranscript
    finish = \case
        Left err ->
            pure (DictationFailed (Text.pack (show err)))
        Right transcript ->
            pure (DictationTranscript (Text.strip transcript))

requireExecutable :: String -> IO ()
requireExecutable command =
    findExecutable command >>= \case
        Just _ -> pure ()
        Nothing ->
            fail $
                command
                    <> " is required for dictation but was not found on PATH"

streamMicrophone :: Int -> IO () -> (BS.ByteString -> IO ()) -> IO ()
streamMicrophone sampleRate waitForStop sendAudio =
    bracket start stop \(input, output, process) ->
        withAsync waitForStop \stopKey ->
            withAsync (pump output) \audioPump ->
                waitEitherCatch stopKey audioPump >>= \case
                    Left (Left err) -> throwIO err
                    Right (Left err) -> throwIO err
                    Right (Right ()) ->
                        fail "microphone audio stream ended unexpectedly"
                    Left (Right ()) -> do
                        hPutStrLn input "q"
                        hFlush input
                        hClose input
                        waitForProcess process >>= \case
                            ExitSuccess -> wait audioPump
                            ExitFailure code ->
                                fail $
                                    "microphone recording failed (ffmpeg exit "
                                        <> show code
                                        <> ")"
  where
    pump output = do
        bytes <- BS.hGetSome output (sampleRate * 2 `div` 10)
        unless (BS.null bytes) do
            sendAudio bytes
            pump output
    start = do
        (Just input, Just output, _, process) <-
            createProcess
                (proc "ffmpeg"
                    [ "-hide_banner"
                    , "-loglevel", "error"
                    , "-f", "avfoundation"
                    , "-i", ":default"
                    , "-ac", "1"
                    , "-ar", show sampleRate
                    , "-c:a", "pcm_s16le"
                    , "-f", "s16le"
                    , "pipe:1"
                    ])
                { std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = NoStream
                }
        pure (input, output, process)
    stop (input, output, process) = do
        void (tryAny (hClose input))
        void (tryAny (hClose output))
        void (tryAny (terminateProcess process))
        void (tryAny (waitForProcess process))

renderLiveTranscript :: Text -> IO ()
renderLiveTranscript text = do
    let preview =
            Text.takeEnd 100
                (Text.unwords (Text.lines (Text.strip text)))
    Text.hPutStr stderr ("\r\ESC[2K● " <> preview)
    hFlush stderr

clearLiveTranscript :: IO ()
clearLiveTranscript = do
    Text.hPutStr stderr "\r\ESC[2K"
    hFlush stderr

waitForStopKey :: IO ()
waitForStopKey = do
    buffering <- hGetBuffering stdin
    hSetBuffering stdin NoBuffering
    let restore = hSetBuffering stdin buffering
        loop =
            hGetChar stdin >>= \case
                '\n' -> pure ()
                '\r' -> pure ()
                '\ETX' -> throwIO UserInterrupt
                _ -> loop
    loop `finally` restore

-- | Insert a transcript at the current cursor with word-safe surrounding space.
insertDictation :: Text -> Int -> Text -> (Text, Int)
insertDictation draft cursor rawTranscript
    | Text.null transcript = (draft, clampedCursor)
    | otherwise =
        let before = Text.take clampedCursor draft
            after = Text.drop clampedCursor draft
            leading
                | needsSpaceAfter before transcript = " "
                | otherwise = ""
            trailing
                | needsSpaceBefore transcript after = " "
                | otherwise = ""
            inserted = leading <> transcript <> trailing
        in
            ( before <> inserted <> after
            , clampedCursor + Text.length inserted
            )
  where
    transcript = Text.strip rawTranscript
    clampedCursor = max 0 (min (Text.length draft) cursor)
    needsSpaceAfter left right =
        maybe False (not . isSpace) (lastChar left)
            && maybe False (not . startsWithPunctuation) (firstChar right)
    needsSpaceBefore left right =
        maybe False (not . isSpace) (lastChar left)
            && maybe False (not . isSpace) (firstChar right)
    firstChar = fmap fst . Text.uncons
    lastChar = fmap snd . Text.unsnoc
    startsWithPunctuation char =
        char `elem` (".,;:!?)]}" :: String)
