module Agent.CLI.ExternalSession.JSONL
    ( JsonlControl(..)
    , consumeJsonl
    , decodeBoundedJsonValue
    , foldJsonl
    , readJsonFileValue
    , readJsonlValues
    ) where

import Agent.CLI.ExternalSession.Content (JsonlCounters(..), oneLine)
import Agent.CLI.ExternalSession.Types
    ( ExternalSessionEnv(..)
    , ExternalSessionError(..)
    )
import Control.Concurrent.Async (wait, withAsync)
import Control.Exception.Safe
    ( IOException
    , bracket
    , throwIO
    , tryIO
    )
import Control.Monad (unless, when)
import Data.Aeson (Value, eitherDecodeStrict')
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word8)
import System.Exit (ExitCode(..))
import System.FilePath (takeFileName)
import System.IO
    ( Handle
    , hClose
    )
import qualified System.IO
import qualified System.IO.Error
import System.Process
    ( CreateProcess(..)
    , StdStream(CreatePipe, NoStream)
    , proc
    , terminateProcess
    , waitForProcess
    , withCreateProcess
    )

data JsonlControl = JsonlContinue | JsonlStop
    deriving (Eq, Show)

maxJsonRecordBytes :: Int
maxJsonRecordBytes = 8 * 1024 * 1024

maxJsonDepth :: Int
maxJsonDepth = 128

consumeJsonl
    :: ExternalSessionEnv
    -> FilePath
    -> Maybe Int
    -> (Value -> IO JsonlControl)
    -> IO JsonlCounters
consumeJsonl env path limit consume =
    snd <$> foldJsonl env path limit () consumeWithoutState
  where
    consumeWithoutState () value = do
        control <- consume value
        pure ((), control)

foldJsonl
    :: ExternalSessionEnv
    -> FilePath
    -> Maybe Int
    -> state
    -> (state -> Value -> IO (state, JsonlControl))
    -> IO (state, JsonlCounters)
foldJsonl env path limit initialState consume
    | ".jsonl.zst" `Text.isSuffixOf` Text.pack (takeFileName path) =
        consumeCompressed
    | otherwise = do
        opened <- tryIO (openBinaryFileRead path)
        case opened of
            Left (exception :: IOException) ->
                throwIO $
                    ExternalSessionReadFailure
                        ("failed to read " <> Text.pack path <> ": "
                            <> Text.pack (show exception))
            Right handle ->
                bracket
                    (pure handle)
                    hClose
                    \openedHandle ->
                        (\(state, counters, _) -> (state, counters))
                            <$> consumeHandle
                                openedHandle
                                limit
                                initialState
                                consume
  where
    consumeCompressed =
        withCreateProcess
            (proc env.externalZstdExecutable ["-dc", "--", path])
                { std_in = NoStream
                , std_out = CreatePipe
                , std_err = CreatePipe
                , create_group = True
                }
            \_ maybeOutput maybeErrors process ->
                case (maybeOutput, maybeErrors) of
                    (Just output, Just errors) ->
                        withAsync (drainStderr errors) \errorsTask -> do
                            (state, counters, stopped) <-
                                consumeHandle output limit initialState consume
                            when stopped $ do
                                _ <- tryIO (hClose output)
                                _ <- tryIO (terminateProcess process)
                                pure ()
                            exitCode <- waitForProcess process
                            details <- wait errorsTask
                            unless (stopped || exitCode == ExitSuccess) $
                                throwIO $
                                    ExternalSessionReadFailure
                                        ( "zstd failed to decompress "
                                            <> Text.pack path
                                            <> ": "
                                            <> if Text.null details
                                                then "unknown error"
                                                else oneLine 300 details
                                        )
                            pure (state, counters)
                    _ ->
                        throwIO $
                            ExternalSessionReadFailure
                                ("failed to capture zstd output for "
                                    <> Text.pack path)

readJsonlValues
    :: ExternalSessionEnv
    -> FilePath
    -> Maybe Int
    -> IO ([Value], JsonlCounters)
readJsonlValues env path limit = do
    (values, counters) <-
        foldJsonl env path limit [] \collected value ->
            pure (value : collected, JsonlContinue)
    pure (reverse values, counters)

readJsonFileValue :: FilePath -> IO (Maybe Value)
readJsonFileValue path =
    tryIO (BS.readFile path) >>= \case
        Left (_ :: IOException) -> pure Nothing
        Right bytes
            | BS.length bytes > maxJsonRecordBytes -> pure Nothing
            | otherwise -> pure (decodeBoundedJsonValue bytes)

decodeBoundedJsonValue :: BS.ByteString -> Maybe Value
decodeBoundedJsonValue bytes
    | BS.length bytes > maxJsonRecordBytes = Nothing
    | jsonDepthExceeds maxJsonDepth bytes = Nothing
    | otherwise = either (const Nothing) Just (eitherDecodeStrict' bytes)

consumeHandle
    :: Handle
    -> Maybe Int
    -> state
    -> (state -> Value -> IO (state, JsonlControl))
    -> IO (state, JsonlCounters, Bool)
consumeHandle handle limit initialState consume =
    go initialState emptyCounters 0
  where
    go state counters accepted
        | maybe False (accepted >=) limit =
            pure (state, counters, True)
        | otherwise = do
            lineResult <- tryIO (BS8.hGetLine handle)
            case lineResult of
                Left exception
                    | isEndOfFileException exception ->
                        pure (state, counters, False)
                    | otherwise -> throwIO exception
                Right line
                    | BS.all isJsonWhitespace line ->
                        go state counters accepted
                    | BS.length line > maxJsonRecordBytes ->
                        go state counters
                            { oversizedRecords =
                                counters.oversizedRecords + 1
                            }
                            accepted
                    | jsonDepthExceeds maxJsonDepth line ->
                        go state counters
                            { deeplyNestedRecords =
                                counters.deeplyNestedRecords + 1
                            }
                            accepted
                    | otherwise ->
                        case eitherDecodeStrict' line of
                            Left _ ->
                                go state counters
                                    { malformedRecords =
                                        counters.malformedRecords + 1
                                    }
                                    accepted
                            Right value -> do
                                (nextState, control) <- consume state value
                                nextState `seq` case control of
                                    JsonlStop ->
                                        pure (nextState, counters, True)
                                    JsonlContinue ->
                                        go nextState counters (accepted + 1)

emptyCounters :: JsonlCounters
emptyCounters = JsonlCounters 0 0 0

isJsonWhitespace :: Word8 -> Bool
isJsonWhitespace byte =
    byte == 0x20 || byte == 0x09 || byte == 0x0d

data DepthState = DepthState
    { depthValue :: !Int
    , depthInString :: !Bool
    , depthEscaped :: !Bool
    , depthExceeded :: !Bool
    }

jsonDepthExceeds :: Int -> BS.ByteString -> Bool
jsonDepthExceeds limit =
    (.depthExceeded)
        . BS.foldl' step (DepthState 0 False False False)
  where
    step state byte
        | state.depthExceeded = state
        | state.depthInString =
            if state.depthEscaped
                then state { depthEscaped = False }
                else case byte of
                    0x5c -> state { depthEscaped = True }
                    0x22 -> state { depthInString = False }
                    _ -> state
        | byte == 0x22 = state { depthInString = True }
        | byte == 0x7b || byte == 0x5b =
            let next = state.depthValue + 1
            in state
                { depthValue = next
                , depthExceeded = next > limit
                }
        | byte == 0x7d || byte == 0x5d =
            state { depthValue = max 0 (state.depthValue - 1) }
        | otherwise = state

drainStderr :: Handle -> IO Text
drainStderr handle = go [] 0
  where
    retainedLimit = 4096
    go chunks retained = do
        bytes <- BS.hGetSome handle 4096
        if BS.null bytes
            then
                pure $
                    decodeUtf8With lenientDecode
                        (BS.concat (reverse chunks))
            else
                let remaining = max 0 (retainedLimit - retained)
                    kept = BS.take remaining bytes
                    nextChunks =
                        if BS.null kept then chunks else kept : chunks
                in go nextChunks (retained + BS.length kept)

openBinaryFileRead :: FilePath -> IO Handle
openBinaryFileRead path =
    System.IO.openBinaryFile path System.IO.ReadMode

isEndOfFileException :: IOException -> Bool
isEndOfFileException = System.IO.Error.isEOFError
