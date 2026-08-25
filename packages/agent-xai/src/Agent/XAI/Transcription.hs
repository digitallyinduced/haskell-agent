-- | Streaming speech-to-text through xAI's dedicated WebSocket endpoint.
module Agent.XAI.Transcription
    ( TranscriptEvent(..)
    , decodeTranscriptEvent
    , transcribeAudioWithXAI
    , transcribePcmWithXAI
    ) where

import Agent.Error (ApiError(..))
import Agent.XAI.Options
    ( defaultGrokClientVersion
    , grokClientIdentifier
    , grokUserAgent
    )
import Agent.Provider
    ( Credential(..)
    , Provider(XAIProvider)
    , TokenProvider
    , runWithTokenProvider
    )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , newEmptyMVar
    , newMVar
    , readMVar
    , takeMVar
    , tryPutMVar
    )
import Control.Exception.Safe
    ( SomeException
    , bracket
    , displayException
    , finally
    , fromException
    , throwIO
    , tryAny
    )
import Control.Monad (unless, void)
import Data.Aeson (FromJSON(..), eitherDecode, withObject, (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.WebSockets as WS
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.IO (hClose)
import System.Process
    ( CreateProcess(..)
    , StdStream(..)
    , createProcess
    , proc
    , terminateProcess
    , waitForProcess
    )
import qualified System.Timeout as Timeout
import qualified Wuss

data TranscriptEvent
    = TranscriptCreated
    | TranscriptPartial
        { transcriptText :: !Text
        , transcriptIsFinal :: !Bool
        , transcriptSpeechFinal :: !Bool
        }
    | TranscriptDone
        { transcriptText :: !Text
        }
    | TranscriptError
        { transcriptMessage :: !Text
        }
    | TranscriptUnknown
    deriving (Eq, Show)

instance FromJSON TranscriptEvent where
    parseJSON = withObject "xAI STT event" \values ->
        values .: "type" >>= \case
            ("transcript.created" :: Text) ->
                pure TranscriptCreated
            "transcript.partial" ->
                TranscriptPartial
                    <$> values .:? "text" Aeson..!= ""
                    <*> values .:? "is_final" Aeson..!= False
                    <*> values .:? "speech_final" Aeson..!= False
            "transcript.done" ->
                TranscriptDone <$> values .:? "text" Aeson..!= ""
            "error" ->
                TranscriptError <$> values .:? "message" Aeson..!= "xAI STT error"
            _ ->
                pure TranscriptUnknown

decodeTranscriptEvent :: LBS.ByteString -> Either String TranscriptEvent
decodeTranscriptEvent = eitherDecode

data TranscriptState = TranscriptState
    { completed :: ![Text]
    , partial :: !Text
    , doneText :: !(Maybe Text)
    , failure :: !(Maybe Text)
    }

emptyTranscriptState :: TranscriptState
emptyTranscriptState = TranscriptState
    { completed = []
    , partial = ""
    , doneText = Nothing
    , failure = Nothing
    }

-- | Convert an audio file to 16 kHz mono PCM16 and stream it through
-- @wss://api.x.ai/v1/stt@. Credentials may be an xAI API key or the refreshed
-- OAuth bearer supplied by the CLI's Grok login.
transcribeAudioWithXAI
    :: TokenProvider
    -> FilePath
    -> IO (Either ApiError Text)
transcribeAudioWithXAI provider audioPath =
    transcribePcmWithXAI
        provider
        (streamAudioFile audioPath)
        (const (pure ()))

-- | Stream live 16 kHz mono signed PCM16 to xAI. The producer receives a
-- function that sends one binary audio chunk. Transcript events are delivered
-- as they arrive, allowing callers to render partial text while recording.
transcribePcmWithXAI
    :: TokenProvider
    -> ((BS.ByteString -> IO ()) -> IO ())
    -> (Text -> IO ())
    -> IO (Either ApiError Text)
transcribePcmWithXAI provider produceAudio onTranscript =
    runWithTokenProvider provider \credential ->
        if credential.provider /= XAIProvider
            then pure $ Left $ CredentialError
                "xAI dictation requires a Grok/xAI credential"
            else do
                result <- tryAny
                    (transcribe credential produceAudio onTranscript)
                pure $ case result of
                    Left err -> Left (transcriptionException err)
                    Right transcript ->
                        Right transcript

transcriptionException :: SomeException -> ApiError
transcriptionException err =
    case fromException err of
        Just (WS.RequestRejected _ response)
            | WS.responseCode response `elem` [401, 403] ->
                HttpError
                    (WS.responseCode response)
                    "xAI transcription authentication was rejected"
        _ ->
            ConnectionError
                ("xAI transcription failed: "
                    <> Text.pack (displayException err))

transcribe
    :: Credential
    -> ((BS.ByteString -> IO ()) -> IO ())
    -> (Text -> IO ())
    -> IO Text
transcribe credential produceAudio onTranscript = do
    language <- sttLanguage
    let path =
            "/v1/stt?sample_rate=16000&encoding=pcm"
                <> "&interim_results=true&language="
                <> Text.unpack language
                <> "&endpointing=400"
    Wuss.runSecureClientWith
        "api.x.ai"
        443
        path
        WS.defaultConnectionOptions
        [ ("Authorization"
          , "Bearer " <> Text.encodeUtf8 credential.accessToken)
        , ("x-grok-client-identifier", Text.encodeUtf8 grokClientIdentifier)
        , ("User-Agent"
          , Text.encodeUtf8 (grokUserAgent defaultGrokClientVersion))
        ]
        \connection -> do
            awaitCreated connection
            state <- newMVar emptyTranscriptState
            finished <- newEmptyMVar
            withAsync
                (receiveTranscripts connection state finished onTranscript)
                \receiver -> do
                (do
                    produceAudio (WS.sendBinaryData connection)
                    WS.sendTextData connection
                        ("{\"type\":\"audio.done\"}" :: Text)
                    waitForTranscript state finished)
                    `finally` do
                        void (tryAny (WS.sendClose connection ("done" :: Text)))
                        cancel receiver

sttLanguage :: IO Text
sttLanguage = do
    configured <- lookupEnv "XAI_STT_LANGUAGE"
    pure $ case configured of
        Just value
            | let language = Text.pack value
            , not (Text.null language)
            , Text.all validLanguageChar language ->
                language
        _ -> "en"
  where
    validLanguageChar char =
        char >= 'a' && char <= 'z'
            || char >= 'A' && char <= 'Z'
            || char >= '0' && char <= '9'
            || char == '-'
            || char == '_'

awaitCreated :: WS.Connection -> IO ()
awaitCreated connection = do
    next <- Timeout.timeout (10 * 1_000_000) loop
    case next of
        Nothing ->
            fail "timed out waiting for xAI transcript.created"
        Just () ->
            pure ()
  where
    loop = do
        bytes <- WS.receiveData connection
        case decodeTranscriptEvent bytes of
            Right TranscriptCreated -> pure ()
            Right TranscriptError{transcriptMessage} ->
                fail (Text.unpack transcriptMessage)
            _ -> loop

receiveTranscripts
    :: WS.Connection
    -> MVar TranscriptState
    -> MVar (Either SomeException ())
    -> (Text -> IO ())
    -> IO ()
receiveTranscripts connection state finished onTranscript =
    tryAny loop >>= void . tryPutMVar finished
  where
    loop = do
        bytes <- WS.receiveData connection
        case decodeTranscriptEvent bytes of
            Left _ -> loop
            Right event -> do
                current <- modifyMVar state \previous ->
                    let next = applyTranscriptEvent event previous
                    in pure (next, next)
                case event of
                    TranscriptPartial{} ->
                        void (tryAny (onTranscript (renderTranscript current)))
                    TranscriptDone{} ->
                        void (tryAny (onTranscript (renderTranscript current)))
                    _ ->
                        pure ()
                case event of
                    TranscriptDone{} -> pure ()
                    TranscriptError{} -> pure ()
                    _ -> loop

applyTranscriptEvent :: TranscriptEvent -> TranscriptState -> TranscriptState
applyTranscriptEvent event state =
    case event of
        TranscriptCreated ->
            state
        TranscriptPartial{transcriptText, transcriptSpeechFinal}
            | transcriptSpeechFinal ->
                state
                    { completed = appendNonEmpty state.completed transcriptText
                    , partial = ""
                    }
            | otherwise ->
                state { partial = transcriptText }
        TranscriptDone{transcriptText} ->
            state
                { doneText =
                    if Text.null (Text.strip transcriptText)
                        then Nothing
                        else Just transcriptText
                , partial = ""
                }
        TranscriptError{transcriptMessage} ->
            state { failure = Just transcriptMessage }
        TranscriptUnknown ->
            state
  where
    appendNonEmpty values text
        | Text.null (Text.strip text) = values
        | otherwise = values <> [text]

waitForTranscript
    :: MVar TranscriptState
    -> MVar (Either SomeException ())
    -> IO Text
waitForTranscript state finished = do
    completedInTime <- Timeout.timeout (30 * 1_000_000) (takeMVar finished)
    case completedInTime of
        Nothing ->
            fail "timed out waiting for xAI transcription"
        Just (Left err) ->
            throwIO err
        Just (Right ()) -> do
            current <- readMVar state
            case current.failure of
                Just message ->
                    fail (Text.unpack message)
                Nothing ->
                    let transcript = renderTranscript current
                    in if Text.null transcript
                        then fail "xAI transcription produced no text"
                        else pure transcript

renderTranscript :: TranscriptState -> Text
renderTranscript state =
    Text.strip $ case state.doneText of
        Just final -> final
        Nothing ->
            Text.intercalate " "
                (filter (not . Text.null)
                    (state.completed <> [state.partial]))

streamAudioFile :: FilePath -> (BS.ByteString -> IO ()) -> IO ()
streamAudioFile audioPath sendAudio =
    bracket start stop \(output, process) -> do
        let loop = do
                -- 100 ms of 16 kHz mono signed PCM16.
                bytes <- BS.hGetSome output 3200
                unless (BS.null bytes) do
                    sendAudio bytes
                    threadDelay 100000
                    loop
        loop
        waitForProcess process >>= \case
            ExitSuccess -> pure ()
            ExitFailure code ->
                fail $
                    "ffmpeg audio conversion failed (exit "
                        <> show code
                        <> ")"
  where
    start = do
        (_, Just output, _, process) <-
            createProcess
                (proc "ffmpeg"
                    [ "-hide_banner"
                    , "-loglevel", "error"
                    , "-i", audioPath
                    , "-f", "s16le"
                    , "-acodec", "pcm_s16le"
                    , "-ac", "1"
                    , "-ar", "16000"
                    , "pipe:1"
                    ])
                { std_out = CreatePipe
                , std_err = Inherit
                }
        pure (output, process)
    stop (output, process) = do
        void (tryAny (hClose output))
        void (tryAny (terminateProcess process))
        void (tryAny (waitForProcess process))
