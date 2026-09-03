-- | Copying, exporting, and browsing conversation transcripts.
module Agent.CLI.Runtime.Repl.Transcript
    ( TranscriptAction(..)
    , handleTranscriptAction
    ) where

import Agent.CLI.Artifact ( fencedCodeBlock, lastDiffBlock )
import Agent.CLI.Command ( CopyRequest(..) )
import Agent.CLI.ExternalProgram
    ( resolveExternalProgram
    , runExternalProgramOnFile
    , withTemporaryTextFile
    )
import Agent.CLI.Runtime.Repl.Context
    ( ReplHandlerContext(..)
    , continueRepl
    , displayReplError
    , displayReplInfo
    , requestReplChoice
    , requestReplText
    , withReplSuspended
    )
import Agent.CLI.Runtime.Types ( RunResult )
import Agent.CLI.Session
    ( Persistence(..)
    , PersistenceState(..)
    , SessionHandle(sessionMeta)
    , SessionMeta(metaId)
    , SessionTurn
    , loadSession
    , sessionsRoot
    )
import Agent.CLI.Session.Selection ( currentSessionId )
import Agent.CLI.SessionEnv ( SessionEnv(..) )
import Agent.CLI.Style
    ( glyphOk, roleError, roleSuccess )
import Agent.CLI.Terminal ( copyTerminalClipboard, resolveColor )
import Agent.CLI.Transcript
    ( assistantResponseBodies, foldTranscriptTurns )
import qualified Agent.CLI.Transcript as Transcript
import Agent.CLI.TranscriptExport
    ( defaultExportFileName
    , resolveExportPath
    , saveCopyText
    , saveTranscriptNoClobber
    )
import qualified Agent.CLI.TranscriptExport as TranscriptExport
import Agent.OsPath ( toText )
import Agent.TUI.Model ( UiBlock )
import Control.Exception.Safe ( tryAny )
import Control.Monad ( forM_ )
import Data.IORef ( readIORef )
import Data.Text ( Text )
import qualified Data.Text as Text
    ( null, pack, strip )
import qualified Data.Text.IO as Text ( hPutStrLn, putStrLn )
import System.IO ( stderr, stdout )

data TranscriptAction
    = ExportTranscript (Maybe Text)
    | CopyResponse CopyRequest
    | CopyCodeBlock Int
    | CopyDiffBlock
    | CopyWorktreePath
    | CopySessionId
    | ShowTranscript
    | FindTranscript (Maybe Text)

handleTranscriptAction
    :: ReplHandlerContext
    -> TranscriptAction
    -> IO RunResult
handleTranscriptAction context action = do
    case action of
        ExportTranscript maybePath ->
            exportTranscript context maybePath
        CopyResponse request ->
            copyResponse context request
        CopyCodeBlock index ->
            copyCodeBlock context index
        CopyDiffBlock ->
            copyDiffBlock context
        CopyWorktreePath ->
            copyWorktreePath context
        CopySessionId ->
            copySessionId context
        ShowTranscript ->
            showTranscript context
        FindTranscript maybeQuery ->
            findTranscript context maybeQuery
    continueRepl context

exportTranscript
    :: ReplHandlerContext
    -> Maybe Text
    -> IO ()
exportTranscript context maybePath =
    loadActiveTranscript context.handlerSessionEnv >>= \case
        Left err ->
            displayReplError context err $
                Text.hPutStrLn stderr
                    (roleError context.handlerStdoutColor err)
        Right (sessionId, turns) -> do
            let markdown =
                    TranscriptExport.renderTranscriptMarkdown turns
                defaultPath = defaultExportFileName sessionId
            case maybePath of
                Just path ->
                    saveTranscriptExport context markdown path
                Nothing ->
                    requestReplChoice
                        context
                        "Export conversation"
                        "Copy the visible conversation or save it as Markdown."
                        0
                        [ ( "Copy Markdown to clipboard"
                          , "Copy the current visible transcript"
                          )
                        , ( "Save Markdown to a file"
                          , "Create a new file without overwriting"
                          )
                        ] >>= \case
                            Nothing -> pure ()
                            Just 0 ->
                                copyCommand
                                    context
                                    "conversation Markdown"
                                    "conversation is unavailable"
                                    (Just markdown)
                            Just _ ->
                                requestReplText
                                    context
                                    "Export path"
                                    "Relative paths use the current working directory. Existing files are never replaced."
                                    defaultPath >>= mapM_
                                        (\entered ->
                                            forM_
                                                (nonBlank entered)
                                                (saveTranscriptExport
                                                    context
                                                    markdown))

loadActiveTranscript
    :: SessionEnv
    -> IO (Either Text (Text, [SessionTurn]))
loadActiveTranscript
        SessionEnv
            { sessionDatabasePool = databasePool
            , sessionHome = home
            , sessionPersist = persist
            } =
    case persist of
        PersistenceDisabled ->
            pure
                (Left
                    "transcript export requires a persisted session")
        PersistenceEnabled slotRef ->
            readIORef slotRef >>= \case
                PersistencePending{} ->
                    pure
                        (Left
                            "transcript export requires an active persisted session")
                PersistenceActive handle ->
                    loadSession
                        databasePool
                        (sessionsRoot home)
                        handle.sessionMeta.metaId >>= \case
                            Left err -> pure (Left err)
                            Right (_, turns) ->
                                pure
                                    (Right
                                        ( handle.sessionMeta.metaId
                                        , turns
                                        ))

saveTranscriptExport
    :: ReplHandlerContext
    -> Text
    -> Text
    -> IO ()
saveTranscriptExport context markdown rawPath =
    resolveExportPath
        context.handlerSessionEnv.sessionCwd
        rawPath >>= \case
            Left err ->
                displayReplError context err $
                    Text.hPutStrLn stderr
                        (roleError context.handlerStdoutColor err)
            Right path ->
                saveTranscriptNoClobber path markdown >>= \case
                    Left err -> do
                        let message =
                                "could not export to "
                                    <> toText path
                                    <> ": "
                                    <> err
                        displayReplError context message $
                            Text.hPutStrLn stderr
                                (roleError
                                    context.handlerStdoutColor
                                    message)
                    Right () -> do
                        let message =
                                "exported conversation to " <> toText path
                        displayReplInfo context message $
                            Text.hPutStrLn stderr
                                (roleSuccess
                                    context.handlerStdoutColor
                                    (glyphOk <> message))

copyResponse
    :: ReplHandlerContext
    -> CopyRequest
    -> IO ()
copyResponse context request =
    loadAssistantResponses context.handlerSessionEnv >>= \case
        Left err ->
            displayReplError context err do
                color <- resolveColor stderr
                Text.hPutStrLn stderr (roleError color err)
        Right responses ->
            case listAt
                (request.copyResponseIndex - 1)
                responses of
                Nothing -> do
                    let available = length responses
                        responseNoun
                            | available == 1 = "response is"
                            | otherwise = "responses are"
                        message
                            | available == 0 =
                                "no assistant response to copy"
                            | otherwise =
                                "only "
                                    <> Text.pack (show available)
                                    <> " assistant "
                                    <> responseNoun
                                    <> " available to copy"
                    displayReplError context message do
                        color <- resolveColor stderr
                        Text.hPutStrLn stderr
                            (roleError color message)
                Just answer ->
                    copyAssistantResponse context request answer

loadAssistantResponses
    :: SessionEnv
    -> IO (Either Text [Text])
loadAssistantResponses env@SessionEnv{sessionLastAssistant = lastAssistantRef} =
    loadPersistedTranscript env >>= \case
        Left err -> pure (Left err)
        Right persisted -> do
            latest <- readIORef lastAssistantRef
            let responses =
                    maybe
                        []
                        (assistantResponseBodies . snd)
                        persisted
            pure $
                Right $
                    if null responses
                        then maybe [] pure latest
                        else responses

copyAssistantResponse
    :: ReplHandlerContext
    -> CopyRequest
    -> Text
    -> IO ()
copyAssistantResponse context request answer = do
    let index = request.copyResponseIndex
        label
            | index == 1 = "last response"
            | otherwise =
                "response " <> Text.pack (show index)
    case request.copyDestination of
        Nothing ->
            copyCommand
                context
                label
                "no assistant response to copy"
                (Just answer)
        Just rawPath ->
            resolveExportPath
                context.handlerSessionEnv.sessionCwd
                rawPath >>= \case
                    Left err ->
                        displayReplError context err do
                            color <- resolveColor stderr
                            Text.hPutStrLn stderr
                                (roleError color err)
                    Right path ->
                        saveCopyText path answer >>= \case
                            Left err ->
                                displayReplError context err do
                                    color <- resolveColor stderr
                                    Text.hPutStrLn stderr
                                        (roleError color err)
                            Right () -> do
                                let message =
                                        "copied "
                                            <> label
                                            <> " to "
                                            <> toText path
                                displayReplInfo context message do
                                    color <- resolveColor stderr
                                    Text.hPutStrLn stderr
                                        (roleSuccess color
                                            (glyphOk <> message))

copyCodeBlock
    :: ReplHandlerContext
    -> Int
    -> IO ()
copyCodeBlock context index = do
    answer <-
        readIORef context.handlerSessionEnv.sessionLastAssistant
    let label = "code block " <> Text.pack (show index)
    copyCommand
        context
        label
        (label <> " was not found")
        (answer >>= fencedCodeBlock index)

copyDiffBlock :: ReplHandlerContext -> IO ()
copyDiffBlock context = do
    answer <-
        readIORef context.handlerSessionEnv.sessionLastAssistant
    copyCommand
        context
        "diff block"
        "no diff block was found"
        (answer >>= lastDiffBlock)

copyWorktreePath :: ReplHandlerContext -> IO ()
copyWorktreePath context =
    copyCommand
        context
        "worktree path"
        "worktree path is unavailable"
        (Just (toText context.handlerSessionEnv.sessionCwd))

copySessionId :: ReplHandlerContext -> IO ()
copySessionId context = do
    sessionId <-
        currentSessionId context.handlerSessionEnv.sessionPersist
    copyCommand
        context
        "session id"
        "this session has no persisted id yet"
        sessionId

showTranscript :: ReplHandlerContext -> IO ()
showTranscript context =
    loadPersistedTranscript context.handlerSessionEnv >>= \case
        Left err ->
            displayReplError context err $
                Text.hPutStrLn stderr
                    (roleError context.handlerStdoutColor err)
        Right Nothing ->
            displayNoTranscript context
        Right (Just (meta, blocks)) ->
            withReplSuspended
                context
                (openPager
                    (Transcript.renderTranscriptMarkdown
                        meta
                        blocks)) >>= \case
                            Left err ->
                                displayReplError context err $
                                    Text.hPutStrLn stderr
                                        (roleError
                                            context.handlerStdoutColor
                                            err)
                            Right () -> pure ()

findTranscript
    :: ReplHandlerContext
    -> Maybe Text
    -> IO ()
findTranscript context maybeQuery =
    loadPersistedTranscript context.handlerSessionEnv >>= \case
        Left err ->
            displayReplError context err $
                Text.hPutStrLn stderr
                    (roleError context.handlerStdoutColor err)
        Right Nothing ->
            displayNoTranscript context
        Right (Just (meta, blocks)) -> do
            let query = maybe "" id maybeQuery
                matches =
                    Transcript.searchTranscriptBlocks
                        query
                        blocks
            if null matches
                then do
                    let message =
                            "No transcript blocks matched “"
                                <> query
                                <> "”."
                    displayReplInfo context message
                        (Text.putStrLn message)
                else
                    withReplSuspended
                        context
                        (openPager
                            (Transcript.renderTranscriptMarkdown
                                meta
                                matches)) >>= \case
                                    Left err ->
                                        displayReplError context err $
                                            Text.hPutStrLn stderr
                                                (roleError
                                                    context.handlerStdoutColor
                                                    err)
                                    Right () -> pure ()

displayNoTranscript :: ReplHandlerContext -> IO ()
displayNoTranscript context =
    displayReplInfo context message (Text.putStrLn message)
  where
    message = "No conversation transcript is available yet."

copyCommand
    :: ReplHandlerContext
    -> Text
    -> Text
    -> Maybe Text
    -> IO ()
copyCommand context label missing payload =
    case payload of
        Nothing ->
            displayReplError context missing do
                color <- resolveColor stderr
                Text.hPutStrLn stderr (roleError color missing)
        Just value -> do
            copied <-
                copyTerminalClipboard
                    context.handlerSessionEnv.sessionTerminal
                    stdout
                    value
            if copied
                then
                    let message = "copied " <> label
                    in displayReplInfo context message do
                        color <- resolveColor stderr
                        Text.hPutStrLn stderr
                            (roleSuccess color (glyphOk <> message))
                else
                    displayReplError
                        context
                        "terminal clipboard is unavailable" do
                            color <- resolveColor stderr
                            Text.hPutStrLn stderr
                                (roleError color
                                    "terminal clipboard is unavailable")

loadPersistedTranscript
    :: SessionEnv
    -> IO (Either Text (Maybe (SessionMeta, [UiBlock])))
loadPersistedTranscript
        SessionEnv
            { sessionDatabasePool = databasePool
            , sessionHome = home
            , sessionPersist = persist
            } =
    case persist of
        PersistenceDisabled -> pure (Right Nothing)
        PersistenceEnabled slotRef ->
            readIORef slotRef >>= \case
                PersistencePending{} -> pure (Right Nothing)
                PersistenceActive handle ->
                    loadSession
                        databasePool
                        (sessionsRoot home)
                        handle.sessionMeta.metaId
                        >>= pure . fmap
                            (\(meta, turns) ->
                                let blocks =
                                        foldTranscriptTurns
                                            (zip [0 ..] turns)
                                in if null blocks
                                    then Nothing
                                    else Just (meta, blocks))

openPager :: Text -> IO (Either Text ())
openPager markdown = do
    outcome <- tryAny do
        resolveExternalProgram
            [("PAGER", "$PAGER")]
            "less -R" >>= \case
                Left err -> pure (Left err)
                Right program ->
                    withTemporaryTextFile
                        "agent-transcript-"
                        markdown
                        (runExternalProgramOnFile program)
    pure $ case outcome of
        Left exception ->
            Left
                ( "could not open transcript: "
                    <> Text.pack (show exception)
                )
        Right result -> result

nonBlank :: Text -> Maybe Text
nonBlank value
    | Text.null stripped = Nothing
    | otherwise = Just stripped
  where
    stripped = Text.strip value

listAt :: Int -> [a] -> Maybe a
listAt index values
    | index < 0 = Nothing
    | otherwise = case drop index values of
        value : _ -> Just value
        [] -> Nothing
