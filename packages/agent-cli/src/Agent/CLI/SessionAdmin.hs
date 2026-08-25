-- | Non-interactive session and storage administration commands.
module Agent.CLI.SessionAdmin
    ( managedPostgresConfigForHome
    , runImportSession
    , runListSessions
    , runShowSession
    , runStorageAdmin
    , runWaitSession
    ) where

import Agent.CLI.Database.Storage
    ( postgresStorageCommandEnv
    , runStorageCommand
    )
import Agent.CLI.Options (StorageCommand)
import Agent.CLI.Session
    ( SessionMeta(..)
    , SessionTurn(..)
    , importSessionTransfer
    , listSessions
    , loadSession
    , sessionDirForId
    , sessionsRoot
    )
import Agent.CLI.SessionLock
    ( sessionLockIsActive
    , sessionLockPath
    )
import Agent.Provider (providerSlug)
import Agent.Store.Postgres
    ( ManagedPostgresConfig
    , Store
    , managedPostgresConfigFromEnv
    , trustedPool
    , withStore
    )
import Agent.Store.Types (renderStoreError)
import Control.Concurrent (threadDelay)
import Control.Monad
    ( unless
    , when
    )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Format
    ( defaultTimeLocale
    , formatTime
    )
import System.Directory.OsPath
    ( doesDirectoryExist
    , getHomeDirectory
    )
import System.Exit (die)
import System.IO (stderr)
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

runListSessions :: IO ()
runListSessions = do
    home <- getHomeDirectory
    withStoreForHome home \store -> do
        (sessions, warnings) <- listSessions (trustedPool store) (sessionsRoot home)
        mapM_ (\warning -> Text.hPutStrLn stderr ("warning: " <> warning)) warnings
        if null sessions
            then putStrLn "No sessions in ~/.haskell-agent/sessions"
            else mapM_ printSessionSummary sessions

runShowSession :: Text -> IO ()
runShowSession sessionId = do
    home <- getHomeDirectory
    withStoreForHome home \store ->
        loadSession (trustedPool store) (sessionsRoot home) sessionId >>= \case
            Left err -> die (Text.unpack err)
            Right (meta, turns) -> do
                printSessionSummary meta
                putStrLn ""
                if null turns
                    then putStrLn "(empty transcript)"
                    else mapM_ printTurn turns

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
