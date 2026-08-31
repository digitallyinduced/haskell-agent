-- | Non-interactive session and storage administration commands.
module Agent.CLI.SessionAdmin
    ( accountSummariesJSON
    , managedPostgresConfigForHome
    , loadSessionPageJSON
    , runImportSession
    , runListSessions
    , runShowSession
    , runStorageAdmin
    , runWaitSession
    , sessionSummaryJSON
    , sessionSummaryWithStatusJSON
    , sessionToolEvent
    ) where

import Agent.CLI.Database.Storage
    ( postgresStorageCommandEnv
    , runStorageCommand
    )
import Agent.CLI.ComputerUse
    ( summarizeComputerCall
    , summarizeComputerToolCall
    )
import Agent.CLI.Login
    ( AccountBilling(..)
    , LoginAccount(..)
    , discoverLoginAccounts
    , loginAccountSelectionId
    )
import Agent.CLI.Options
    ( SessionOutputFormat(..)
    , SessionPageRequest(..)
    , StorageCommand
    )
import Agent.CLI.Session
    ( SessionMeta(..)
    , SessionTurn(..)
    , SessionTurnPage(..)
    , importSessionTransfer
    , listArchivedSessionIds
    , listSessions
    , loadSession
    , loadSessionMeta
    , loadRecentSessionHistoryTurns
    , loadRecentSessionTurns
    , loadSessionHistoryTurnsBefore
    , loadSessionTurnsBefore
    , sessionDirForId
    , sessionsRoot
    )
import Agent.CLI.SessionLock
    ( sessionActivityLockPath
    , sessionLockIsActive
    , sessionLockPath
    )
import Agent.OsPath (unsafeToFilePath)
import Agent.Provider (providerSlug)
import Agent.Responses.LoopBackend (responseItemToToolCall)
import Agent.Responses.Types
    ( ComputerCall(..)
    , ComputerCallOutput(..)
    , CustomToolCall(..)
    , CustomToolCallOutput(..)
    , FunctionCall(..)
    , FunctionCallOutput(..)
    , ResponseItem(..)
    , computerFunctionName
    , computerFunctionNamespace
    , legacyComputerFunctionName
    )
import Agent.Store.Postgres
    ( ManagedPostgresConfig
    , Store
    , managedPostgresConfigFromEnv
    , trustedPool
    , withStore
    )
import Agent.Store.Postgres.Connection (StorePool)
import Agent.Store.Types (renderStoreError)
import Control.Concurrent (threadDelay)
import Control.Monad
    ( unless
    , when
    )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Int (Int64)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as Text
import Data.Time.Format
    ( defaultTimeLocale
    , formatTime
    )
import qualified System.Directory as Directory
import System.Directory.OsPath
    ( doesDirectoryExist
    , getHomeDirectory
    )
import System.Exit (die)
import System.OsPath
    ( OsPath
    , decodeFS
    , unsafeEncodeUtf
    , (</>)
    )

runStorageAdmin :: StorageCommand -> IO ()
runStorageAdmin command = do
    home <- getHomeDirectory
    config <- managedPostgresConfigForHome home
    runStorageCommand (postgresStorageCommandEnv config) command >>= \case
        Left err -> die (Text.unpack err)
        Right message -> Text.putStrLn message

managedPostgresConfigForHome :: OsPath -> IO ManagedPostgresConfig
managedPostgresConfigForHome home = do
    stateDirectory <-
        decodeFS (home </> unsafeEncodeUtf ".haskell-agent")
    managedPostgresConfigFromEnv stateDirectory

runListSessions :: SessionOutputFormat -> IO ()
runListSessions outputFormat = do
    home <- getHomeDirectory
    withStoreForHome home \store -> do
        let root = sessionsRoot home
            pool = trustedPool store
        sessions <- listSessions pool root
        archivedIds <- listArchivedSessionIds pool
            >>= either (fail . Text.unpack) pure
        let archived = Set.fromList archivedIds
        case outputFormat of
            SessionJSON -> do
                summaries <- mapM
                    (\session -> sessionSummaryWithStatusJSON
                        root
                        (Set.member session.metaId archived)
                        session)
                    sessions
                LBS8.putStrLn (Aeson.encode summaries)
            SessionHuman ->
                if null sessions
                    then putStrLn "No sessions in ~/.haskell-agent/sessions"
                    else mapM_ printSessionSummary sessions

runShowSession
    :: Text
    -> SessionOutputFormat
    -> Maybe SessionPageRequest
    -> IO ()
runShowSession sessionId outputFormat pageRequest = do
    home <- getHomeDirectory
    withStoreForHome home \store -> do
        let pool = trustedPool store
            root = sessionsRoot home
        case pageRequest of
            Nothing ->
                loadSession pool root sessionId >>= \case
                    Left err -> die (Text.unpack err)
                    Right (meta, turns) ->
                        renderFullSession outputFormat meta turns
            Just request -> do
                meta <- loadSessionMeta pool root sessionId
                    >>= either (die . Text.unpack) pure
                page <- loadPage pool root request
                    >>= either (die . Text.unpack) pure
                renderSessionPage meta page
  where
    loadPage pool root = \case
        SessionRecent limit ->
            loadRecentSessionTurns pool root sessionId limit
        SessionBefore cursor limit ->
            loadSessionTurnsBefore pool root sessionId cursor limit

renderFullSession
    :: SessionOutputFormat
    -> SessionMeta
    -> [SessionTurn]
    -> IO ()
renderFullSession outputFormat meta turns =
    case outputFormat of
        SessionJSON ->
            LBS8.putStrLn
                (Aeson.encode (Aeson.object
                    [ "meta" Aeson..= sessionSummaryJSON meta
                    , "turns" Aeson..= map sessionTurnJSON turns
                    ]))
        SessionHuman -> do
            printSessionSummary meta
            putStrLn ""
            if null turns
                then putStrLn "(empty transcript)"
                else mapM_ printTurn turns

renderSessionPage :: SessionMeta -> SessionTurnPage -> IO ()
renderSessionPage meta page =
    LBS8.putStrLn (Aeson.encode (sessionPageJSON meta page))

loadSessionPageJSON
    :: StorePool
    -> OsPath
    -> Text
    -> Maybe Int64
    -> Int
    -> IO (Either Text Aeson.Value)
loadSessionPageJSON pool root sessionId before limit = do
    metaResult <- loadSessionMeta pool root sessionId
    pageResult <- case before of
        Nothing -> loadRecentSessionHistoryTurns pool root sessionId limit
        Just cursor ->
            loadSessionHistoryTurnsBefore pool root sessionId cursor limit
    pure do
        meta <- metaResult
        page <- pageResult
        pure (sessionPageJSON meta page)

sessionPageJSON :: SessionMeta -> SessionTurnPage -> Aeson.Value
sessionPageJSON meta page =
    Aeson.object
        [ "meta" Aeson..= sessionSummaryJSON meta
        , "turns" Aeson..= map indexedSessionTurnJSON page.pageTurns
        , "page" Aeson..= Aeson.object
            [ "generationStart" Aeson..= page.pageGenerationStart
            , "totalTurns" Aeson..= page.pageTotalTurns
            , "hasOlder" Aeson..= page.pageHasOlder
            , "hasNewer" Aeson..= page.pageHasNewer
            ]
        ]

sessionSummaryJSON :: SessionMeta -> Aeson.Value
sessionSummaryJSON = sessionSummaryJSONWithState False False False

sessionSummaryWithStatusJSON
    :: OsPath
    -> Bool
    -> SessionMeta
    -> IO Aeson.Value
sessionSummaryWithStatusJSON root archived meta = do
    (running, locked) <- case sessionDirForId root meta.metaId of
        Left _ -> pure (False, False)
        Right sessionDir -> do
            running <- probeLock (sessionActivityLockPath sessionDir)
            locked <- probeLock (sessionLockPath sessionDir)
            pure (running, locked)
    pure (sessionSummaryJSONWithState running locked archived meta)
  where
    probeLock lockPath = do
        lockExists <- Directory.doesFileExist lockPath
        if lockExists
            then sessionLockIsActive lockPath
            else pure False

sessionSummaryJSONWithState
    :: Bool -> Bool -> Bool -> SessionMeta -> Aeson.Value
sessionSummaryJSONWithState running locked archived meta =
    Aeson.object
        [ "id" Aeson..= meta.metaId
        , "title" Aeson..= meta.metaTitle
        , "updatedAt" Aeson..= meta.metaUpdatedAt
        , "provider" Aeson..= providerSlug meta.metaProvider
        , "model" Aeson..= meta.metaModel
        , "effort" Aeson..= meta.metaEffort
        , "cwd" Aeson..= unsafeToFilePath meta.metaCwd
        , "isRunning" Aeson..= running
        , "isLocked" Aeson..= locked
        , "isArchived" Aeson..= archived
        ]

accountSummariesJSON :: IO [Aeson.Value]
accountSummariesJSON = map accountJSON <$> discoverLoginAccounts
  where
    accountJSON account = Aeson.object
        [ "provider" Aeson..= providerSlug account.loginProvider
        , "billing" Aeson..= case account.loginBilling of
            SubscriptionBilling _ -> ("subscription" :: Text)
            ApiCreditsBilling -> "api"
        , "selectionID" Aeson..= loginAccountSelectionId account
        , "accountID" Aeson..= account.loginAccountId
        , "label" Aeson..= account.loginLabel
        , "detail" Aeson..= account.loginSource
        , "managedID" Aeson..= account.loginManagedId
        , "source" Aeson..= account.loginSource
        , "enabled" Aeson..= account.loginEnabled
        , "canManage" Aeson..= maybe False (const True) account.loginManagedId
        ]

-- Keep native transcript hydration independent from provider response items.
-- Those can contain large tool payloads that this view does not render.
sessionTurnJSON :: SessionTurn -> Aeson.Value
sessionTurnJSON turn =
    Aeson.object
        [ "userText" Aeson..= turn.turnUserText
        , "assistantText" Aeson..= turn.turnAssistantText
        , "error" Aeson..= turn.turnError
        , "toolEvents" Aeson..= sessionToolEvents turn
        ]

indexedSessionTurnJSON :: (Int64, SessionTurn) -> Aeson.Value
indexedSessionTurnJSON (index, turn) =
    Aeson.object
        [ "index" Aeson..= index
        , "userText" Aeson..= turn.turnUserText
        , "assistantText" Aeson..= turn.turnAssistantText
        , "error" Aeson..= turn.turnError
        , "toolEvents" Aeson..= sessionToolEvents turn
        ]

sessionToolEvents :: SessionTurn -> [Aeson.Value]
sessionToolEvents turn =
    mapMaybe sessionToolEvent turn.turnItems

sessionToolEvent :: ResponseItem -> Maybe Aeson.Value
sessionToolEvent = \case
    item@(FunctionCallItem call)
        | ( call.name == computerFunctionName
                && call.namespace `elem` [Nothing, Just "functions"]
          )
            || ( call.name == legacyComputerFunctionName
                && call.namespace == Just computerFunctionNamespace
               ) ->
            let summary = fromMaybe "Computer action"
                    (responseItemToToolCall item
                        >>= summarizeComputerToolCall)
            in Just $
                toolStartedJSON
                    call.callId
                    "computer"
                    (TextEncoding.decodeUtf8 . LBS.toStrict . Aeson.encode $
                        Aeson.object ["summary" Aeson..= summary])
    FunctionCallItem call ->
        Just (toolStartedJSON call.callId call.name call.arguments)
    CustomToolCallItem call ->
        Just (toolStartedJSON call.callId call.name call.input)
    FunctionCallOutputItem result ->
        Just
            (toolFinishedJSON
                result.callId
                (if containsInputImage result.output
                    then "Screenshot captured"
                    else renderToolValue result.output))
    CustomToolCallOutputItem result ->
        Just (toolFinishedJSON result.callId (renderToolValue result.output))
    ComputerCallItem call ->
        Just $
            toolStartedJSON
                call.computerCallId
                "computer"
                (TextEncoding.decodeUtf8 . LBS.toStrict . Aeson.encode $
                    Aeson.object
                        [ "summary" Aeson..= summarizeComputerCall call
                        ])
    ComputerCallOutputItem result ->
        Just
            (toolFinishedJSON
                result.computerOutputCallId
                "Screenshot captured")
    _ -> Nothing

containsInputImage :: Aeson.Value -> Bool
containsInputImage = \case
    Aeson.Object object ->
        KeyMap.lookup "type" object == Just (Aeson.String "input_image")
            || any containsInputImage object
    Aeson.Array values -> any containsInputImage values
    _ -> False

toolStartedJSON :: Text -> Text -> Text -> Aeson.Value
toolStartedJSON callId name arguments =
    let (visible, truncated) = boundedSessionToolText arguments
    in Aeson.object
        [ "type" Aeson..= ("tool_started" :: Text)
        , "callId" Aeson..= callId
        , "name" Aeson..= name
        , "arguments" Aeson..= visible
        , "argumentsEncrypted" Aeson..= False
        , "truncated" Aeson..= truncated
        ]

toolFinishedJSON :: Text -> Text -> Aeson.Value
toolFinishedJSON callId output =
    let (visible, truncated) = boundedSessionToolText output
    in Aeson.object
        [ "type" Aeson..= ("tool_finished" :: Text)
        , "callId" Aeson..= callId
        , "output" Aeson..= visible
        , "truncated" Aeson..= truncated
        ]

renderToolValue :: Aeson.Value -> Text
renderToolValue = \case
    Aeson.String value -> value
    value ->
        TextEncoding.decodeUtf8 (LBS.toStrict (Aeson.encode value))

boundedSessionToolText :: Text -> (Text, Bool)
boundedSessionToolText value =
    let (visible, remainder) = Text.splitAt 8192 value
    in (visible, not (Text.null remainder))

withStoreForHome :: OsPath -> (Store -> IO a) -> IO a
withStoreForHome home action = do
    config <- managedPostgresConfigForHome home
    withStore config action >>= \case
        Left err -> die (Text.unpack (renderStoreError err))
        Right value -> pure value

printSessionSummary :: SessionMeta -> IO ()
printSessionSummary meta =
    putStrLn $ Text.unpack $ Text.intercalate "  "
        [ meta.metaId
        , Text.pack
            (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" meta.metaUpdatedAt)
        , providerSlug meta.metaProvider
        , meta.metaModel
        , meta.metaTitle
        ]

printTurn :: SessionTurn -> IO ()
printTurn turn = do
    Text.putStrLn ("user> " <> turn.turnUserText)
    case turn.turnAssistantText of
        Just text | not (Text.null (Text.strip text)) ->
            Text.putStrLn ("assistant> " <> text)
        _ -> pure ()
    case turn.turnError of
        Just err | not (Text.null (Text.strip err)) ->
            Text.putStrLn ("error> " <> err)
        _ -> pure ()
    putStrLn ""

runWaitSession :: Text -> IO ()
runWaitSession sessionId = do
    home <- getHomeDirectory
    dir <- either (die . Text.unpack) pure
        (sessionDirForId (sessionsRoot home) sessionId)
    exists <- doesDirectoryExist dir
    unless exists (die ("session not found: " <> Text.unpack sessionId))
    let wait = do
            active <- sessionLockIsActive (sessionLockPath dir)
            when active (threadDelay 100000 >> wait)
    wait

runImportSession :: Maybe OsPath -> IO ()
runImportSession cwd = do
    bytes <- LBS.getContents
    transfer <- case Aeson.eitherDecode bytes of
        Left err -> die ("invalid transferred session: " <> err)
        Right value -> pure value
    home <- getHomeDirectory
    withStoreForHome home \store ->
        importSessionTransfer
            (trustedPool store)
            (sessionsRoot home)
            cwd
            transfer >>= \case
                Left err -> die (Text.unpack err)
                Right sessionId -> Text.putStrLn sessionId
