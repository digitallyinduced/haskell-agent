{-# LANGUAGE OverloadedRecordDot #-}

-- | Session-scoped storage for oversized tool output.
module Agent.Tools.OutputArtifact
    ( OutputArtifact(..)
    , OutputArtifactWriter
    , artifactTools
    , finalizeToolOutput
    , boundedPreview
    , OutputArtifactMetadata(..)
    , outputArtifactMetadata
    , openOutputArtifact
    , appendOutputArtifact
    , finishOutputArtifact
    , abortOutputArtifact
    , renderOutputArtifactNotice
    , writeOutputArtifact
    , writeOutputArtifactDetailed
    , readOutputArtifact
    ) where

import Agent.Json.Decode (Decoder)
import Agent.OsPath (unsafeToFilePath)
import Agent.ToolArgs (objectArgs, optBool, optInt, reqText)
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch (ToolCall(..), typedTool, typedToolWithCall)
import Agent.Tools.Types
    ( AppTool
    , ToolEnv(..)
    , ToolExecutionPolicy(..)
    , jsonTool
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newMVar
    )
import Control.Exception.Safe
    ( SomeException
    , tryAny
    )
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.IORef (readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Encoding.Error as EncodingError
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    , doesDirectoryExist
    , pathIsSymbolicLink
    , removeFile
    )
import System.FilePath ((</>), takeFileName)
import System.IO
    ( Handle
    , hClose
    , openBinaryTempFile
    )
import System.Posix.Files (setFileMode)

artifactDirectoryName :: FilePath
artifactDirectoryName = "tool-output-artifacts"

artifactPrefix :: String
artifactPrefix = "output-"

data OutputArtifact = OutputArtifact
    { artifactHandle :: !Text
    , artifactObservedBytes :: !Int
    , artifactStoredBytes :: !Int
    , artifactTruncated :: !Bool
    } deriving (Eq, Show)

-- | Cheap metadata for a persisted artifact.  Metadata is derived from the
-- bytes on disk rather than trusting model-provided handles.
data OutputArtifactMetadata = OutputArtifactMetadata
    { metadataHandle :: !Text
    , metadataBytes :: !Int
    , metadataCharacters :: !Int
    } deriving (Eq, Show)

data WriterState = WriterState
    { writerHandle :: !(Maybe Handle)
    , writerObserved :: !Int
    , writerStored :: !Int
    , writerFailure :: !(Maybe Text)
    }

data OutputArtifactWriter = OutputArtifactWriter
    { outputWriterPath :: !FilePath
    , outputWriterName :: !Text
    , outputWriterCap :: !Int
    , outputWriterState :: !(MVar WriterState)
    }

artifactTools
    :: ToolEnv
    -> Maybe (ToolCall -> Text -> Text -> IO (Either Text Text))
    -> [AppTool]
artifactTools env analysis =
    [ jsonTool "read_tool_output"
        "Read a bounded line range from an oversized tool-output artifact."
        [ PropertySchema "handle" PropertyString True Nothing
        , PropertySchema "offset" PropertyNumber False
            (Just "1-based line offset; defaults to 1.")
        , PropertySchema "limit" PropertyNumber False
            (Just "Maximum 1000 lines; defaults to 200.")
        ]
        True ParallelSafe
        (typedTool "read_tool_output" readArgsDecoder (readToolOutput env))
    , jsonTool "search_tool_output"
        "Search an oversized tool-output artifact for a literal string and return bounded matching lines."
        [ PropertySchema "handle" PropertyString True Nothing
        , PropertySchema "pattern" PropertyString True Nothing
        , PropertySchema "case_insensitive" PropertyBoolean False Nothing
        , PropertySchema "head_limit" PropertyNumber False
            (Just "Maximum 200 matching lines; defaults to 50.")
        ]
        True ParallelSafe
        (typedTool "search_tool_output" searchArgsDecoder (searchToolOutput env))
    ]
    <> maybe [] (\spawn ->
        [ jsonTool "analyze_tool_output"
            "Spawn a tracked gpt-5.6-luna child to analyze an oversized tool-output artifact. Use wait_agent for its report."
            [ PropertySchema "handle" PropertyString True Nothing
            , PropertySchema "instruction" PropertyString True Nothing
            ]
            True TurnSequential
            (typedToolWithCall "analyze_tool_output" analyzeArgsDecoder
                (\call (AnalyzeArgs handle instruction) ->
                    artifactExists env handle >>= \case
                        Left err -> pure (Left err)
                        Right () -> spawn call handle instruction))
        ]) analysis

data ReadArgs = ReadArgs
    { handle :: Text
    , offset :: Maybe Int
    , limit :: Maybe Int
    }

readArgsDecoder :: Decoder ReadArgs
readArgsDecoder = objectArgs \o ->
        ReadArgs <$> reqText o "handle" <*> optInt o "offset" <*> optInt o "limit"

data SearchArgs = SearchArgs
    { handle :: Text
    , pattern :: Text
    , caseInsensitive :: Bool
    , headLimit :: Maybe Int
    }

searchArgsDecoder :: Decoder SearchArgs
searchArgsDecoder = objectArgs \o ->
        SearchArgs
            <$> reqText o "handle"
            <*> reqText o "pattern"
            <*> (fromMaybe False <$> optBool o "case_insensitive")
            <*> optInt o "head_limit"

data AnalyzeArgs = AnalyzeArgs Text Text

analyzeArgsDecoder :: Decoder AnalyzeArgs
analyzeArgsDecoder = objectArgs \o ->
        AnalyzeArgs <$> reqText o "handle" <*> reqText o "instruction"

readToolOutput :: ToolEnv -> ReadArgs -> IO (Either Text Text)
readToolOutput env args =
    readOutputArtifact env args.handle >>= \case
        Left err -> pure (Left err)
        Right content -> do
            let start = max 1 (fromMaybe 1 args.offset)
                count = min 1000 (max 1 (fromMaybe 200 args.limit))
                selected = take count (drop (start - 1) (Text.lines content))
                end = start + length selected - 1
                body = Text.intercalate "\n" selected
            pure . Right . boundResult $
                "artifact " <> args.handle <> " lines "
                    <> Text.pack (show start) <> "-"
                    <> Text.pack (show end)
                    <> ":\n" <> body

searchToolOutput :: ToolEnv -> SearchArgs -> IO (Either Text Text)
searchToolOutput env args =
    readOutputArtifact env args.handle >>= \case
        Left err -> pure (Left err)
        Right content -> do
            let needle = foldCase args.caseInsensitive args.pattern
                matches =
                    [ Text.pack (show n) <> ":" <> line
                    | (n, line) <- zip [1 :: Int ..] (Text.lines content)
                    , needle `Text.isInfixOf` foldCase args.caseInsensitive line
                    ]
                cap = min 200 (max 1 (fromMaybe 50 args.headLimit))
                shown = take cap matches
                suffix
                    | length matches > cap =
                        "\n[search truncated: "
                            <> Text.pack (show (length matches - cap))
                            <> " matches omitted]"
                    | otherwise = ""
            pure . Right . boundResult $
                if null shown
                    then "No matches in artifact " <> args.handle
                    else Text.intercalate "\n" shown <> suffix
  where
    foldCase True = Text.toCaseFold
    foldCase False = id

-- | Replace an oversized provider-facing result with a compact artifact marker.
finalizeToolOutput :: ToolEnv -> ToolCall -> Text -> IO Text
finalizeToolOutput env call output
    | BS.length encoded <= max 0 env.toolOutputInlineCap = pure output
    | otherwise =
        writeOutputArtifactDetailed env encoded >>= \case
            Left err ->
                pure $
                    "[tool output exceeded inline limit; artifact unavailable: "
                        <> err
                        <> "]\n[BEGIN UNTRUSTED TOOL OUTPUT PREVIEW]\n"
                        <> boundedPreview env.toolOutputPreviewCap output
                        <> "\n[END UNTRUSTED TOOL OUTPUT PREVIEW]"
            Right artifact ->
                pure $
                    renderOutputArtifactNotice call.name artifact
                        <> "\n[BEGIN UNTRUSTED TOOL OUTPUT PREVIEW]\n"
                        <> boundedPreview env.toolOutputPreviewCap output
                        <> "\n[END UNTRUSTED TOOL OUTPUT PREVIEW]"
  where
    encoded = Encoding.encodeUtf8 output

openOutputArtifact :: ToolEnv -> IO (Either Text OutputArtifactWriter)
openOutputArtifact env =
    readIORef env.toolSessionTmp >>= \case
        Nothing -> pure (Left "session scratch storage is unavailable")
        Just root -> do
            let rootPath = unsafeToFilePath root
                directory = rootPath </> artifactDirectoryName
            tryAny (do
                rootExists <- doesDirectoryExist rootPath
                rootLink <- if rootExists then pathIsSymbolicLink rootPath else pure False
                if rootLink
                    then ioError (userError "session scratch directory is a symlink")
                    else pure ()
                directoryExists <- doesDirectoryExist directory
                directoryLink <-
                    if directoryExists then pathIsSymbolicLink directory else pure False
                if directoryLink
                    then ioError (userError "artifact directory is a symlink")
                    else pure ()
                createDirectoryIfMissing True directory
                setFileMode directory 0o700
                (path, handle) <- openBinaryTempFile directory artifactPrefix
                state <- newMVar WriterState
                    { writerHandle = Just handle
                    , writerObserved = 0
                    , writerStored = 0
                    , writerFailure = Nothing
                    }
                pure OutputArtifactWriter
                    { outputWriterPath = path
                    , outputWriterName = Text.pack (takeFileName path)
                    , outputWriterCap = max 0 env.toolOutputArtifactCap
                    , outputWriterState = state
                    }) >>= \case
                Left exception ->
                    pure (Left ("failed to create tool-output artifact: "
                        <> exceptionText exception))
                Right writer -> pure (Right writer)

appendOutputArtifact
    :: OutputArtifactWriter
    -> BS.ByteString
    -> IO (Either Text ())
appendOutputArtifact writer bytes =
    modifyMVar writer.outputWriterState \state ->
        case (state.writerHandle, state.writerFailure) of
            (_, Just err) ->
                pure (state
                    { writerObserved = state.writerObserved + BS.length bytes
                    }, Left err)
            (Nothing, Nothing) ->
                pure (state, Left "tool-output artifact is already finalized")
            (Just handle, Nothing) -> do
                let remaining =
                        max 0 (writer.outputWriterCap - state.writerStored)
                    storedChunk = BS.take remaining bytes
                    next = state
                        { writerObserved = state.writerObserved + BS.length bytes
                        , writerStored =
                            state.writerStored + BS.length storedChunk
                        }
                tryAny (unless (BS.null storedChunk) (BS.hPut handle storedChunk))
                    >>= \case
                        Left exception -> do
                            let err = "failed to write tool-output artifact: "
                                    <> exceptionText exception
                            pure (next { writerFailure = Just err }, Left err)
                        Right () -> pure (next, Right ())

finishOutputArtifact :: OutputArtifactWriter -> IO OutputArtifact
finishOutputArtifact writer =
    modifyMVar writer.outputWriterState \state -> do
        case state.writerHandle of
            Nothing -> pure ()
            Just handle -> do
                _ <- tryAny (hClose handle)
                _ <- tryAny (setFileMode writer.outputWriterPath 0o600)
                pure ()
        let artifact = OutputArtifact
                { artifactHandle = writer.outputWriterName
                , artifactObservedBytes = state.writerObserved
                , artifactStoredBytes = state.writerStored
                , artifactTruncated =
                    state.writerObserved > state.writerStored
                        || maybe False (const True) state.writerFailure
                }
        pure (state { writerHandle = Nothing }, artifact)

abortOutputArtifact :: OutputArtifactWriter -> IO ()
abortOutputArtifact writer = do
    modifyMVar_ writer.outputWriterState \state -> do
        mapM_ (\handle -> do
            _ <- tryAny (hClose handle)
            pure ()) state.writerHandle
        pure state { writerHandle = Nothing }
    _ <- tryAny (removeFile writer.outputWriterPath)
    pure ()

writeOutputArtifact :: ToolEnv -> Text -> IO (Either Text Text)
writeOutputArtifact env content =
    fmap (fmap (.artifactHandle))
        (writeOutputArtifactDetailed env (Encoding.encodeUtf8 content))

writeOutputArtifactDetailed
    :: ToolEnv
    -> BS.ByteString
    -> IO (Either Text OutputArtifact)
writeOutputArtifactDetailed env bytes =
    openOutputArtifact env >>= \case
        Left err -> pure (Left err)
        Right writer ->
            appendOutputArtifact writer bytes >>= \case
                Left err -> abortOutputArtifact writer >> pure (Left err)
                Right () -> Right <$> finishOutputArtifact writer

readOutputArtifact :: ToolEnv -> Text -> IO (Either Text Text)
readOutputArtifact env rawHandle =
    resolveArtifactPath env rawHandle >>= \case
        Left err -> pure (Left err)
        Right path ->
            tryAny
                (Encoding.decodeUtf8With EncodingError.lenientDecode
                    <$> BS.readFile path) >>= \case
                Left exception ->
                    pure (Left ("failed to read artifact: "
                        <> exceptionText exception))
                Right content -> pure (Right content)

outputArtifactMetadata
    :: ToolEnv
    -> Text
    -> IO (Either Text OutputArtifactMetadata)
outputArtifactMetadata env handle =
    readOutputArtifact env handle >>= \case
        Left err -> pure (Left err)
        Right content ->
            pure $ Right OutputArtifactMetadata
                { metadataHandle = handle
                , metadataBytes = BS.length (Encoding.encodeUtf8 content)
                , metadataCharacters = Text.length content
                }

artifactExists :: ToolEnv -> Text -> IO (Either Text ())
artifactExists env handle =
    resolveArtifactPath env handle >>= \case
        Left err -> pure (Left err)
        Right _ -> pure (Right ())

resolveArtifactPath :: ToolEnv -> Text -> IO (Either Text FilePath)
resolveArtifactPath env rawHandle
    | not (validHandle rawHandle) =
        pure (Left "invalid tool-output artifact handle")
    | otherwise =
        readIORef env.toolSessionTmp >>= \case
            Nothing -> pure (Left "session scratch storage is unavailable")
            Just root -> do
                let path =
                        unsafeToFilePath root
                            </> artifactDirectoryName
                            </> Text.unpack rawHandle
                    rootPath = unsafeToFilePath root
                    directory = rootPath </> artifactDirectoryName
                rootExists <- doesDirectoryExist rootPath
                rootSymbolic <-
                    if rootExists then pathIsSymbolicLink rootPath else pure False
                directoryExists <- doesDirectoryExist directory
                directorySymbolic <-
                    if directoryExists then pathIsSymbolicLink directory else pure False
                exists <- doesFileExist path
                symbolic <- if exists then pathIsSymbolicLink path else pure False
                pure $
                    if rootSymbolic || directorySymbolic
                        then Left "tool-output artifact symlinks are not allowed"
                        else if not exists
                        then Left "tool-output artifact was not found"
                        else if symbolic
                            then Left "tool-output artifact symlinks are not allowed"
                            else Right path

validHandle :: Text -> Bool
validHandle handle =
    let unpacked = Text.unpack handle
        prefix = Text.pack artifactPrefix
    in prefix `Text.isPrefixOf` handle
        && Text.length handle > Text.length prefix
        && takeFileName unpacked == unpacked
        && not (Text.isInfixOf ".." handle)
        && Text.all validCharacter handle
  where
    validCharacter c =
        c == '-' || c == '_' || c == '.'
            || c >= '0' && c <= '9'
            || c >= 'a' && c <= 'z'
            || c >= 'A' && c <= 'Z'

renderOutputArtifactNotice :: Text -> OutputArtifact -> Text
renderOutputArtifactNotice source artifact =
    "[tool output from " <> source
        <> " stored as artifact " <> artifact.artifactHandle
        <> "; observed " <> showText artifact.artifactObservedBytes
        <> " bytes, stored " <> showText artifact.artifactStoredBytes
        <> " bytes"
        <> (if artifact.artifactTruncated
                then " (artifact storage cap reached)"
                else "")
        <> ". Full stored output is excluded from model context. "
        <> "Use read_tool_output/search_tool_output, or analyze_tool_output "
        <> "when available for bounded Luna analysis.]"

-- | Return a bounded head/tail preview.  The bound is in UTF-8 bytes (the
-- same unit used by the inline and artifact caps). Partial UTF-8 code points
-- at the head/tail boundaries are omitted.
boundedPreview :: Int -> Text -> Text
boundedPreview cap text
    | cap <= 0 = ""
    | BS.length encoded <= cap = text
    | cap <= BS.length markerBytes =
        fit (decode (BS.take cap encoded))
    | otherwise =
        let budget = cap - BS.length markerBytes
            leftBytes = budget `div` 2
            rightBytes = budget - leftBytes
        in fit
            (decode (BS.take leftBytes encoded)
                <> marker
                <> decode (BS.drop (BS.length encoded - rightBytes) encoded))
  where
    encoded = Encoding.encodeUtf8 text
    marker = "\n… [middle omitted] …\n"
    markerBytes = Encoding.encodeUtf8 marker
    decode = Encoding.decodeUtf8With EncodingError.ignore
    fit value
        | BS.length (Encoding.encodeUtf8 value) <= cap = value
        | Text.null value = ""
        | otherwise = fit (Text.dropEnd 1 value)

boundResult :: Text -> Text
boundResult result
    | BS.length encoded <= resultCap = result
    | otherwise =
        Encoding.decodeUtf8With EncodingError.ignore
            (BS.take (resultCap - BS.length suffixBytes) encoded)
            <> suffix
  where
    encoded = Encoding.encodeUtf8 result
    resultCap = 50 * 1024
    suffix = "\n[artifact tool result truncated]"
    suffixBytes = Encoding.encodeUtf8 suffix

showText :: Show a => a -> Text
showText = Text.pack . show

exceptionText :: SomeException -> Text
exceptionText = Text.pack . show
