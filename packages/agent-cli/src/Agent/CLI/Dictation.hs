-- | Terminal microphone recording and provider transcription.
module Agent.CLI.Dictation
    ( DictationAuthError(..)
    , DictationBackend(..)
    , DictationControl(..)
    , DictationResult(..)
    , DictationTarget(..)
    , dictate
    , dictateForProvider
    , dictateForTarget
    , dictateWith
    , dictateWithTarget
    , dictationTargetForSession
    , dictationBackendsForProvider
    , dictationBackendUnavailable
    , insertDictation
    , loadDictationBackendAuth
    , selectDictationBackend
    , transcribeAudio
    ) where

import Agent.CLI.Auth
    ( LoadedAuth(..)
    , authErrorNeedsOnboarding
    , loadAuth
    , loadOpenAiDictationAuth
    )
import Agent.CLI.Transcription (transcribeAudio)
import Agent.CLI.GatewayClient
    ( GatewayModelAccess
    , transcribeGatewayPcm
    )
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
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (mapMaybe)
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

-- | Dictation is either scoped to the active direct provider or routed
-- entirely through the organization gateway. Keeping these constructors
-- distinct prevents a gateway session from falling back to local credentials.
data DictationTarget
    = DirectDictation !Provider
    | GatewayDictation !GatewayModelAccess

-- | Select the only dictation transport allowed by the current session
-- boundary. A connected gateway is authoritative regardless of the model's
-- underlying provider.
dictationTargetForSession
    :: Provider
    -> Maybe GatewayModelAccess
    -> DictationTarget
dictationTargetForSession provider =
    maybe (DirectDictation provider) GatewayDictation

-- | Select the dictation backends allowed for the active model provider, in
-- preference order. Providers with their own speech-to-text integration only
-- ever transcribe with their own credentials. Claude has no transcription
-- API, so Claude sessions borrow whichever OpenAI or xAI account is
-- configured locally; OpenAI is tried first because ChatGPT subscriptions
-- stream partial transcripts. Remaining providers fail explicitly rather than
-- spending credentials from an unrelated provider.
dictationBackendsForProvider
    :: Provider
    -> Either Text (NonEmpty DictationBackend)
dictationBackendsForProvider = \case
    OpenAIProvider -> Right (OpenAIDictation :| [])
    XAIProvider -> Right (XAIDictation :| [])
    ClaudeCodeProvider -> Right (OpenAIDictation :| [XAIDictation])
    provider ->
        Left $
            "Dictation is not supported for "
                <> providerSlug provider
                <> " models"

-- | Why a backend cannot supply a credential. Distinguishing a plain absence
-- from a broken configuration lets borrowed-backend errors stay short while
-- still surfacing anything the user should fix.
data DictationAuthError
    = DictationCredentialMissing !Text
    -- ^ Nothing is configured for this backend.
    | DictationCredentialInvalid !Text
    -- ^ Something is configured but could not be loaded.
    deriving (Eq, Show)

dictationAuthErrorText :: DictationAuthError -> Text
dictationAuthErrorText = \case
    DictationCredentialMissing err -> err
    DictationCredentialInvalid err -> err

-- | Load the direct credential a backend transcribes with, without recording
-- any audio.
loadDictationBackendAuth
    :: DictationBackend
    -> IO (Either DictationAuthError LoadedAuth)
loadDictationBackendAuth = \case
    OpenAIDictation ->
        loadOpenAiDictationAuth >>= \case
            Nothing ->
                pure $ Left $ DictationCredentialMissing
                    "No OpenAI credential found for OpenAI dictation"
            Just loaded ->
                pure (Right loaded)
    XAIDictation ->
        loadAuth (Just XAIProvider) >>= \case
            Left err
                | authErrorNeedsOnboarding err ->
                    pure (Left (DictationCredentialMissing err))
                | otherwise ->
                    pure (Left (DictationCredentialInvalid err))
            Right loaded
                | loaded.loadedProvider /= XAIProvider ->
                    pure $ Left $ DictationCredentialInvalid
                        "xAI dictation requires direct xAI credentials"
                | otherwise ->
                    pure (Right loaded)

-- | Pick the first allowed backend that has a usable credential. Only the
-- credential lookup participates in the fallback; once a backend is chosen,
-- its transcription errors surface directly instead of silently retrying
-- with another account.
selectDictationBackend
    :: Provider
    -> (DictationBackend -> IO (Either DictationAuthError LoadedAuth))
    -> IO (Either Text (DictationBackend, LoadedAuth))
selectDictationBackend provider loadBackendAuth =
    case dictationBackendsForProvider provider of
        Left err ->
            pure (Left err)
        Right backends ->
            go [] (NonEmpty.toList backends)
  where
    go failures = \case
        [] ->
            pure $ Left $
                dictationBackendUnavailable provider (reverse failures)
        backend : remaining ->
            loadBackendAuth backend >>= \case
                Right loaded ->
                    pure (Right (backend, loaded))
                Left err ->
                    go ((backend, err) : failures) remaining

-- | Explain why no allowed backend could transcribe. A provider with a single
-- native backend reports that backend's error verbatim. Borrowed backends
-- summarize which accounts would work and only repeat lookup errors that
-- describe a broken configuration, so a Claude user is not shown every
-- provider's generic sign-in hint.
dictationBackendUnavailable
    :: Provider
    -> [(DictationBackend, DictationAuthError)]
    -> Text
dictationBackendUnavailable provider failures =
    case failures of
        [(_, err)] -> dictationAuthErrorText err
        _ ->
            "Dictation for "
                <> providerSlug provider
                <> " models requires an OpenAI or xAI account; connect one \
                   \with /login"
                <> case mapMaybe detail failures of
                    [] -> ""
                    details ->
                        " (" <> Text.intercalate "; " details <> ")"
  where
    detail (backend, err) = case err of
        DictationCredentialMissing _ -> Nothing
        DictationCredentialInvalid message ->
            Just (backendLabel backend <> ": " <> message)
    backendLabel = \case
        OpenAIDictation -> "openai"
        XAIDictation -> "xai"

-- | Legacy standalone entry point. The inline model-aware REPL and fullscreen
-- composer use 'dictateForProvider'; this preserves the original xAI default
-- for callers that have no active model context.
dictate :: IO Text
dictate = dictateForProvider XAIProvider

-- | Stream the default microphone using the active model provider.
dictateForProvider :: Provider -> IO Text
dictateForProvider = dictateForTarget . DirectDictation

dictateForTarget :: DictationTarget -> IO Text
dictateForTarget target = do
    Text.hPutStrLn stderr "● Starting dictation…"
    hFlush stderr
    result <-
        dictateWithTarget target
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
dictateWith provider =
    dictateWithTarget (DirectDictation provider)

dictateWithTarget
    :: DictationTarget
    -> DictationControl
    -> IO DictationResult
dictateWithTarget target control =
    try run >>= \case
        Left (err :: SomeException) ->
            pure (DictationFailed (Text.pack (displayException err)))
        Right result ->
            pure result
  where
    run = do
        requireExecutable "ffmpeg"
        case target of
            DirectDictation provider ->
                selectDictationBackend provider loadDictationBackendAuth
                    >>= \case
                        Left err ->
                            pure (DictationFailed err)
                        Right (backend, loaded) ->
                            runBackend backend loaded
            GatewayDictation gateway ->
                transcribeGatewayPcm
                    gateway
                    (streamMicrophone
                        openAITranscriptionSampleRate
                        control.dictationWaitForStop)
                    control.dictationOnTranscript >>= \case
                        Left err -> pure (DictationFailed err)
                        Right transcript ->
                            pure
                                (DictationTranscript
                                    (Text.strip transcript))
    runBackend backend loaded = case backend of
        OpenAIDictation ->
            finish =<<
                transcribePcmWithOpenAI
                    loaded.loadedTokenProvider
                    (streamMicrophone
                        openAITranscriptionSampleRate
                        control.dictationWaitForStop)
                    control.dictationOnTranscript
        XAIDictation ->
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
