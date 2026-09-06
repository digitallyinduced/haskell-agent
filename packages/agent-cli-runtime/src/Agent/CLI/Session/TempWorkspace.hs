-- | Private scratch-space allocation and cleanup for persisted sessions.
module Agent.CLI.Session.TempWorkspace
    ( defaultSessionTempKeepCount
    , SessionTempCleanupReport(..)
    , SessionTempLease
    , sessionsRoot
    , sessionTempsRoot
    , sessionMaterializationMetaPath
    , removeSessionMaterializationMeta
    , isValidSessionId
    , sessionDirForId
    , sessionTempDirForId
    , allocateSessionTemp
    , acquireSessionTempLease
    , releaseSessionTempLease
    , cleanupStaleSessionTemps
    , ensureSessionTemp
    , removeSessionTemp
    , removeReservedTemp
    , ensurePrivateDir
    , symbolicLinkStatusMaybe
    ) where

import Agent.CLI.SessionLock
    ( acquireSessionLock
    , releaseSessionLock
    )
import Agent.OsPath (toText, unsafeToFilePath)
import Control.Exception.Safe
    ( SomeException
    , displayException
    , finally
    , tryAny
    , tryIO
    )
import Control.Monad (foldM)
import Data.Char (isHexDigit)
import Data.List (sortOn)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (Day)
import Data.Time.Clock
    ( UTCTime
    , getCurrentTime
    , nominalDiffTimeToSeconds
    , utctDay
    )
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Numeric (showHex)
import System.Directory.OsPath
    ( createDirectory
    , createDirectoryIfMissing
    , doesDirectoryExist
    , listDirectory
    , removeFile
    , removePathForcibly
    )
import qualified System.FileLock as FileLock
import System.OsPath
    ( OsPath
    , equalFilePath
    , normalise
    , takeDirectory
    , takeFileName
    , unsafeEncodeUtf
    , (</>)
    )
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files
    ( FileStatus
    , getSymbolicLinkStatus
    , isSymbolicLink
    , setFileMode
    )

symbolicLinkStatusMaybe :: OsPath -> IO (Maybe FileStatus)
symbolicLinkStatusMaybe path =
    tryIO (getSymbolicLinkStatus (unsafeToFilePath path)) >>= \case
        Left err
            | isDoesNotExistError err -> pure Nothing
            | otherwise -> ioError err
        Right status -> pure (Just status)

-- | Keep a small recent cache for session artifacts while bounding abandoned
-- shell environments, tool outputs, and other scratch data.
defaultSessionTempKeepCount :: Int
defaultSessionTempKeepCount = 15
data SessionTempCleanupReport = SessionTempCleanupReport
    { tempCleanupRemoved :: ![OsPath]
    , tempCleanupFailures :: ![(OsPath, Text)]
    }
    deriving (Eq, Show)

instance Semigroup SessionTempCleanupReport where
    left <> right = SessionTempCleanupReport
        { tempCleanupRemoved =
            left.tempCleanupRemoved <> right.tempCleanupRemoved
        , tempCleanupFailures =
            left.tempCleanupFailures <> right.tempCleanupFailures
        }

instance Monoid SessionTempCleanupReport where
    mempty = SessionTempCleanupReport [] []

newtype SessionTempLease = SessionTempLease FileLock.FileLock

-- | @~/.haskell-agent/sessions@ given the user's home directory.
sessionsRoot :: OsPath -> OsPath
sessionsRoot home =
    home </> unsafeEncodeUtf ".haskell-agent" </> unsafeEncodeUtf "sessions"

-- | @~/.haskell-agent/tmp/sessions@ for a corresponding sessions root.
sessionTempsRoot :: OsPath -> OsPath
sessionTempsRoot root =
    takeDirectory root
        </> unsafeEncodeUtf "tmp"
        </> unsafeEncodeUtf "sessions"

sessionMaterializationMetaPath :: OsPath -> Text -> OsPath
sessionMaterializationMetaPath root sessionId =
    sessionTempsRoot root
        </> unsafeEncodeUtf
            (".materialization-" <> Text.unpack sessionId <> ".json")

removeSessionMaterializationMeta :: OsPath -> Text -> IO ()
removeSessionMaterializationMeta root sessionId = do
    _ <- tryIO (removeFile (sessionMaterializationMetaPath root sessionId))
    pure ()

-- | Session ids are single path components. Keep this deliberately broader
-- than the current date-plus-hex allocator so older ids remain resumable.
isValidSessionId :: Text -> Bool
isValidSessionId sessionId =
    not (Text.null sessionId)
        && sessionId /= "."
        && sessionId /= ".."
        && Text.all (\char -> char /= '/' && char /= '\\' && char /= '\NUL') sessionId

sessionDirForId :: OsPath -> Text -> Either Text OsPath
sessionDirForId root sessionId
    | isValidSessionId sessionId =
        Right (root </> unsafeEncodeUtf (Text.unpack sessionId))
    | otherwise = Left "invalid session id"

sessionTempDirForId :: OsPath -> Text -> Either Text OsPath
sessionTempDirForId root sessionId
    | isValidSessionId sessionId =
        Right
            (sessionTempsRoot root
                </> unsafeEncodeUtf (Text.unpack sessionId))
    | otherwise = Left "invalid session id"

-- | Reserve a unique session id by atomically creating its private scratch
-- directory. The durable session directory remains deferred until first use.
allocateSessionTemp :: OsPath -> IO (Text, OsPath)
allocateSessionTemp root = do
    let tempRoot = sessionTempsRoot root
    ensurePrivateDir tempRoot
    now <- getCurrentTime
    go tempRoot now (0 :: Int)
  where
    go tempRoot now attempt
        | attempt >= 32 = fail "could not allocate a unique session temp directory"
        | otherwise = do
            let sessionId = sessionIdForAttempt now attempt
                durableDir =
                    root </> unsafeEncodeUtf (Text.unpack sessionId)
                tempDir =
                    tempRoot </> unsafeEncodeUtf (Text.unpack sessionId)
            durableExists <-
                maybe False (const True)
                    <$> symbolicLinkStatusMaybe durableDir
            recoveryExists <-
                maybe False (const True)
                    <$> symbolicLinkStatusMaybe
                        (sessionMaterializationMetaPath root sessionId)
            if durableExists || recoveryExists
                then go tempRoot now (attempt + 1)
                else tryIO (createDirectory tempDir) >>= \case
                    Left _ -> go tempRoot now (attempt + 1)
                    Right () -> do
                        setFileMode (unsafeToFilePath tempDir) 0o700
                        pure (sessionId, tempDir)

-- | Take a shared lease for a session's scratch directory. Automatic cleanup
-- requires the matching exclusive lock, so a live process cannot lose its
-- temporary files even before its durable session lock has been acquired.
acquireSessionTempLease
    :: OsPath
    -> OsPath
    -> IO (Either Text (Maybe SessionTempLease))
acquireSessionTempLease root path =
    case sessionTempId root path of
        Nothing -> pure (Right Nothing)
        Just sessionId -> do
            let lockPath = sessionTempLockPath root sessionId
            result <- tryAny $
                ensurePrivateDir (takeDirectory lockPath)
                    >> FileLock.tryLockFile
                        (unsafeToFilePath lockPath)
                        FileLock.Shared
            pure case result of
                Left exception ->
                    Left
                        ("failed to lease session scratch directory "
                            <> toText path
                            <> ": "
                            <> Text.pack (displayException exception))
                Right Nothing ->
                    Left
                        ("session scratch directory is being cleaned up: "
                            <> toText path)
                Right (Just lock) ->
                    Right (Just (SessionTempLease lock))

releaseSessionTempLease :: SessionTempLease -> IO ()
releaseSessionTempLease (SessionTempLease lock) = do
    _ <- tryAny (FileLock.unlockFile lock)
    pure ()

-- | Remove old session scratch directories after retaining the newest entries.
-- Only allocator-shaped names are considered, and a directory with a live
-- shared lease is skipped. Failures are reported per path and never stop the
-- rest of the best-effort cleanup. Directories allocated on the current UTC
-- day are always kept, closing the startup interval before a lease is taken.
cleanupStaleSessionTemps
    :: OsPath
    -> Int
    -> [OsPath]
    -> IO SessionTempCleanupReport
cleanupStaleSessionTemps root requestedKeep protected = do
    let tempRoot = sessionTempsRoot root
    exists <- doesDirectoryExist tempRoot
    if not exists
        then pure mempty
        else do
            today <- utctDay <$> getCurrentTime
            listed <- tryAny (listDirectory tempRoot)
            case listed of
                Left exception ->
                    pure $ tempCleanupFailure tempRoot exception
                Right entries -> do
                    directories <- foldM
                        (collectDirectory tempRoot)
                        ([], [])
                        entries
                    case directories of
                        (managed, discoveryFailures) -> do
                            let candidates =
                                    filter
                                        (isBefore today . takeFileName)
                                        (drop (max 1 requestedKeep) $
                                            sortOn
                                                (Down
                                                    . unsafeToFilePath
                                                    . takeFileName)
                                                managed)
                            cleaned <- foldM cleanupOne mempty candidates
                            pure $
                                cleaned
                                    <> mempty
                                        { tempCleanupFailures =
                                            discoveryFailures
                                        }
  where
    protectedPaths = map normalise protected
    isBefore today path =
        maybe False (< today) (allocatedSessionDay path)

    collectDirectory tempRoot (managed, failures) entry = do
        let path = tempRoot </> entry
        checked <- tryAny (doesDirectoryExist path)
        pure case checked of
            Left exception ->
                ( managed
                , failures
                    <> [(path, Text.pack (displayException exception))]
                )
            Right True
                | isAllocatedSessionId entry ->
                    (managed <> [path], failures)
            Right _ -> (managed, failures)

    cleanupOne report candidate
        | any
            (\protectedPath ->
                equalFilePath protectedPath (normalise candidate))
            protectedPaths =
                pure report
        | otherwise = do
            result <- tryAny (cleanupStaleSessionTemp root candidate)
            pure $ report <> case result of
                Left exception ->
                    tempCleanupFailure candidate exception
                Right candidateReport -> candidateReport

cleanupStaleSessionTemp
    :: OsPath
    -> OsPath
    -> IO SessionTempCleanupReport
cleanupStaleSessionTemp root candidate =
    case sessionTempId root candidate of
        Nothing -> pure mempty
        Just sessionId -> do
            let durableDir = root </> sessionId
            durableExists <- doesDirectoryExist durableDir
            durableLock <-
                if durableExists
                    then fmap (fmap Just) $
                        acquireSessionLock durableDir (toText sessionId)
                    else pure (Right Nothing)
            case durableLock of
                -- A running or otherwise un-lockable durable session owns the
                -- scratch directory. Treat either case conservatively.
                Left _ -> pure mempty
                Right lock ->
                    cleanupWithSessionLock sessionId
                        `finally` mapM_ releaseSessionLock lock
  where
    cleanupWithSessionLock sessionId = do
        let lockPath = sessionTempLockPath root sessionId
        locked <- tryAny $
            ensurePrivateDir (takeDirectory lockPath)
                >> FileLock.tryLockFile
                    (unsafeToFilePath lockPath)
                    FileLock.Exclusive
        case locked of
            Left exception ->
                pure $ tempCleanupFailure candidate exception
            Right Nothing ->
                pure mempty
            Right (Just lock) -> do
                removed <- tryAny $
                    (do
                        symbolicLinkStatusMaybe candidate >>= \case
                            -- Another startup cleaner may have removed the
                            -- candidate before this process acquired its
                            -- exclusive lock.
                            Nothing -> pure False
                            Just status
                                | isSymbolicLink status -> pure False
                                | otherwise ->
                                    removePathForcibly candidate
                                        >> removeSessionMaterializationMeta
                                            root
                                            (toText sessionId)
                                        >> pure True)
                        `finally` FileLock.unlockFile lock
                pure case removed of
                    Left exception ->
                        tempCleanupFailure candidate exception
                    Right True ->
                        mempty { tempCleanupRemoved = [candidate] }
                    Right False -> mempty

sessionTempId :: OsPath -> OsPath -> Maybe OsPath
sessionTempId root rawPath =
    let tempRoot = normalise (sessionTempsRoot root)
        path = normalise rawPath
    in if equalFilePath tempRoot (takeDirectory path)
            && isAllocatedSessionId (takeFileName path)
        then Just (takeFileName path)
        else Nothing

sessionTempLockPath :: OsPath -> OsPath -> OsPath
sessionTempLockPath root sessionId =
    sessionTempsRoot root
        </> unsafeEncodeUtf ".locks"
        </> (sessionId <> unsafeEncodeUtf ".lock")

isAllocatedSessionId :: OsPath -> Bool
isAllocatedSessionId path = case allocatedSessionDay path of
    Just _ -> True
    Nothing -> False

allocatedSessionDay :: OsPath -> Maybe Day
allocatedSessionDay path =
    case unsafeToFilePath path of
        year1 : year2 : year3 : year4 : '-' :
                month1 : month2 : '-' : day1 : day2 : '-' : suffix ->
            let date =
                    [ year1, year2, year3, year4, '-'
                    , month1, month2, '-', day1, day2
                    ]
            in if length suffix == 8 && all isHexDigit suffix
                then parseTimeM True defaultTimeLocale "%Y-%m-%d" date
                else Nothing
        _ -> Nothing

tempCleanupFailure
    :: OsPath
    -> SomeException
    -> SessionTempCleanupReport
tempCleanupFailure path exception =
    mempty
        { tempCleanupFailures =
            [(path, Text.pack (displayException exception))]
        }

ensureSessionTemp :: OsPath -> Text -> IO (Either Text OsPath)
ensureSessionTemp root sessionId =
    case sessionTempDirForId root sessionId of
        Left err -> pure (Left err)
        Right tempDir -> do
            result <- tryIO (ensurePrivateDir tempDir)
            pure $ case result of
                Left err ->
                    Left
                        ("could not create session temp directory: "
                            <> Text.pack (displayException err))
                Right () -> Right tempDir

sessionIdForAttempt :: UTCTime -> Int -> Text
sessionIdForAttempt now attempt =
    let day = formatTime defaultTimeLocale "%Y-%m-%d" now
        start =
            floor
                (nominalDiffTimeToSeconds
                    (utcTimeToPOSIXSeconds now)
                    * 1000000) :: Integer
        hex = hex8 (start + fromIntegral attempt)
    in Text.pack (day <> "-" <> hex)

removeSessionTemp :: OsPath -> Text -> IO (Either Text ())
removeSessionTemp root sessionId =
    case sessionTempDirForId root sessionId of
        Left err -> pure (Left err)
        Right tempDir -> do
            removeSessionMaterializationMeta root sessionId
            exists <- doesDirectoryExist tempDir
            if not exists
                then pure (Right ())
                else tryIO (removePathForcibly tempDir) >>= \case
                    Left err ->
                        pure $ Left
                            ("could not delete session temp directory: "
                                <> Text.pack (displayException err))
                    Right () -> pure (Right ())

removeReservedTemp :: OsPath -> Text -> IO ()
removeReservedTemp root sessionId = do
    _ <- removeSessionTemp root sessionId
    pure ()

hex8 :: Integer -> String
hex8 n =
    let s = showHex (n `mod` 0x100000000) ""
    in replicate (8 - length s) '0' <> s

ensurePrivateDir :: OsPath -> IO ()
ensurePrivateDir path = do
    createDirectoryIfMissing True path
    _ <- tryIO (setFileMode (unsafeToFilePath path) 0o700)
    pure ()
