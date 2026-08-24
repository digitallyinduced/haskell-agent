-- | Codex-backed voice-message transcription for the Telegram gateway.
module Agent.Telegram.Voice
    ( transcribeWithCodex
    ) where

import Control.Applicative ((<|>))
import Control.Exception.Safe (bracket, tryAny)
import Control.Monad (void)
import Data.Aeson
    ( Result(..)
    , Value(..)
    , eitherDecodeStrict'
    , encode
    , fromJSON
    , object
    , (.=)
    )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import System.IO
    ( Handle
    , hClose
    , hFlush
    )
import System.Process
    ( CreateProcess(..)
    , StdStream(..)
    , createProcess
    , proc
    , terminateProcess
    , waitForProcess
    )
import qualified System.Timeout as Timeout
import Text.Read (readMaybe)

transcribeWithCodex :: FilePath -> FilePath -> IO Text
transcribeWithCodex cwd audioPath = do
    result <- Timeout.timeout (5 * 60 * 1_000_000) $
        bracket start stop \(input, output, _) -> do
            send input $ object
                [ "method" .= ("initialize" :: Text)
                , "id" .= (1 :: Int)
                , "params" .= object
                    [ "clientInfo" .= object
                        [ "name" .= ("haskell_agent_telegram" :: Text)
                        , "title" .= ("Haskell Agent Telegram" :: Text)
                        , "version" .= ("0.1.0" :: Text)
                        ]
                    ]
                ]
            _ <- awaitResult output 1
            send input $ object
                [ "method" .= ("initialized" :: Text)
                , "params" .= object []
                ]
            send input $ object
                [ "method" .= ("thread/start" :: Text)
                , "id" .= (2 :: Int)
                , "params" .= object
                    [ "ephemeral" .= True
                    , "cwd" .= cwd
                    , "approvalPolicy" .= ("never" :: Text)
                    , "sandbox" .= ("read-only" :: Text)
                    ]
                ]
            threadResponse <- awaitResult output 2
            threadId <- maybe
                (fail "Codex thread/start response did not contain a thread ID")
                pure
                (lookupText ["result", "thread", "id"] threadResponse)
            send input $ object
                [ "method" .= ("turn/start" :: Text)
                , "id" .= (3 :: Int)
                , "params" .= object
                    [ "threadId" .= threadId
                    , "input" .=
                        [ object
                            [ "type" .= ("text" :: Text)
                            , "text" .=
                                ("Transcribe the attached voice message exactly. \
                                \Return only the transcription, without commentary."
                                    :: Text)
                            ]
                        , object
                            [ "type" .= ("localAudio" :: Text)
                            , "path" .= audioPath
                            ]
                        ]
                    ]
                ]
            _ <- awaitResult output 3
            awaitCodexTranscript output Nothing
    maybe (fail "Codex voice transcription timed out") pure result
  where
    start = do
        (Just input, Just output, _, process) <-
            createProcess (proc "codex" ["app-server", "--stdio"])
                { std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = Inherit
                }
        pure (input, output, process)
    stop (input, output, process) = do
        void (tryAny (hClose input))
        void (tryAny (hClose output))
        void (tryAny (terminateProcess process))
        void (tryAny (waitForProcess process))
    send handle value = do
        LBS8.hPutStrLn handle (encode value)
        hFlush handle

awaitResult :: Handle -> Int -> IO Value
awaitResult output expectedId = do
    value <- readCodexValue output
    case lookupInteger ["id"] value of
        Just actualId
            | actualId == fromIntegral expectedId ->
                case lookupText ["error", "message"] value of
                    Just message -> fail (Text.unpack message)
                    Nothing -> pure value
        _ -> awaitResult output expectedId

awaitCodexTranscript :: Handle -> Maybe Text -> IO Text
awaitCodexTranscript output latest = do
    value <- readCodexValue output
    let latest' = case
            ( lookupText ["method"] value
            , lookupText ["params", "item", "type"] value
            , lookupText ["params", "item", "text"] value
            ) of
                (Just "item/completed", Just "agentMessage", Just text) ->
                    Just text
                _ -> latest
    case lookupText ["method"] value of
        Just "turn/completed" -> case
            lookupText ["params", "turn", "error", "message"] value of
                Just message -> fail (Text.unpack message)
                Nothing ->
                    maybe
                        (fail "Codex completed without a transcription")
                        pure
                        ( latest'
                            <|> lookupText
                                ["params", "turn", "items", "0", "text"]
                                value
                        )
        _ -> awaitCodexTranscript output latest'

readCodexValue :: Handle -> IO Value
readCodexValue output = do
    line <- BS8.hGetLine output
    case eitherDecodeStrict' line of
        Left _ -> readCodexValue output
        Right value -> pure value

lookupText :: [Text] -> Value -> Maybe Text
lookupText path value = case lookupValue path value of
    Just (String text) -> Just text
    _ -> Nothing

lookupInteger :: [Text] -> Value -> Maybe Integer
lookupInteger path value = case lookupValue path value of
    Just number -> case fromJSON number of
        Success integer -> Just integer
        Error _ -> Nothing
    _ -> Nothing

lookupValue :: [Text] -> Value -> Maybe Value
lookupValue [] value = Just value
lookupValue (field : fields) (Object values) =
    KeyMap.lookup (Key.fromText field) values >>= lookupValue fields
lookupValue (index : fields) (Array values) = do
    position <- readMaybe (Text.unpack index)
    value <- values Vector.!? position
    lookupValue fields value
lookupValue _ _ = Nothing
