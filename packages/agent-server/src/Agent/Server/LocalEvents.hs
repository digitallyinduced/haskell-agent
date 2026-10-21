-- | Full local presentation data. Large events travel by private artifact so
-- the bounded SSE replay window retains references rather than image blobs.
module Agent.Server.LocalEvents
    ( localLoopEvent, localPresentationEvent, localValueEnvelope, localEventFile
    , writeLocalTurnResult, localTurnResultFile, cleanupLocalEvents ) where

import Agent.Loop (LoopEvent(..))
import Agent.Runtime.Session.Events (encodeDisplayEvent)
import Agent.Server.Event (projectLoopEvent)
import Agent.Server.Identifier (isUUIDText, newUUIDv7Text)
import Control.Exception.Safe (bracket, throwString, tryIO)
import Control.Monad (unless, forM, forM_, void)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.List (sortOn)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (createDirectoryIfMissing, getHomeDirectory, makeAbsolute, renameFile, listDirectory, removeFile, removeDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (hClose)
import System.Posix.Files
import System.Posix.IO
import System.Posix.User (getEffectiveUserID)

eventDirectory :: IO FilePath
eventDirectory = do
    home <- getHomeDirectory
    configured <- lookupEnv "HASKELL_AGENT_RUNTIME_DIR"
    root <- makeAbsolute (maybe (home </> ".haskell-agent/runtime") id configured)
    pure (root </> "event-payloads")

privateDirectory :: FilePath -> IO ()
privateDirectory path = do
    createDirectoryIfMissing True path
    bracket (openFd path ReadOnly defaultFileFlags
        { nofollow = True, cloexec = True, directory = True }) closeFd \descriptor -> do
        status <- getFdStatus descriptor
        user <- getEffectiveUserID
        unless (isDirectory status && fileOwner status == user) $
            throwString "local event storage must be owned by the current user"
        setFdMode descriptor 0o700

localLoopEvent :: Text -> LoopEvent -> IO (Text, Value)
localLoopEvent turnId event = do
    case event of
        TurnFinished _ -> writeLocalTurnResult turnId (object ["kind" .= ("loop" :: Text), "event" .= encodeDisplayEvent event])
        _ -> pure ()
    local <- localEnvelope turnId (encodeDisplayEvent event)
    let (kind, public) = projectLoopEvent event
    pure (kind, case public of
        Object fields -> Object (KeyMap.insert "localEvent" local fields)
        _ -> object ["localEvent" .= local])

localPresentationEvent :: Text -> Text -> Value -> IO (Text, Value)
localPresentationEvent turnId kind value = do
    envelope <- localEnvelope turnId value
    pure (kind, object ["localEvent" .= envelope])

localValueEnvelope :: Value -> IO Value
localValueEnvelope value = newUUIDv7Text >>= \identifier -> localEnvelope identifier value

localEnvelope :: Text -> Value -> IO Value
localEnvelope turnId full = do
    let bytes = encode full
    local <- if ByteString.length bytes <= 32768
        then pure (object ["event" .= full])
        else do
            root <- eventDirectory
            privateDirectory root
            let directory = root </> Text.unpack turnId
            privateDirectory directory
            identifier <- newUUIDv7Text
            let path = directory </> Text.unpack identifier <> ".json"
            bracket (openFd path WriteOnly defaultFileFlags
                { creat = Just 0o600, exclusive = True, nofollow = True, cloexec = True } >>= fdToHandle)
                hClose (\output -> ByteString.hPut output bytes)
            pure (object ["turnId" .= turnId, "payloadId" .= identifier])
    pure local

localEventFile :: Text -> Text -> IO (Maybe FilePath)
localEventFile turnId identifier
    | not (isUUIDText turnId && isUUIDText identifier) = pure Nothing
    | otherwise = do
        root <- eventDirectory
        let path = root </> Text.unpack turnId </> Text.unpack identifier <> ".json"
        pure (Just path)

localTurnResultFile :: Text -> IO (Maybe FilePath)
localTurnResultFile turnId
    | not (isUUIDText turnId) = pure Nothing
    | otherwise = do
        root <- eventDirectory
        pure (Just (root </> Text.unpack turnId </> "result.json"))

writeLocalTurnResult :: Text -> Value -> IO ()
writeLocalTurnResult turnId value = do
    unless (isUUIDText turnId) (throwString "invalid local turn identity")
    root <- eventDirectory
    privateDirectory root
    let directory = root </> Text.unpack turnId
    privateDirectory directory
    identifier <- newUUIDv7Text
    let temporary = directory </> Text.unpack identifier <> ".result"
    bracket (openFd temporary WriteOnly defaultFileFlags
        {creat = Just 0o600, exclusive = True, nofollow = True, cloexec = True} >>= fdToHandle)
        hClose (\handle -> ByteString.hPut handle (encode value))
    renameFile temporary (directory </> "result.json")

-- Presentation artifacts are disposable: canonical conversation history stays
-- in PostgreSQL. Retain recent reconnect data for a day and trim older, idle
-- payloads when their total exceeds 1 GiB. Never follow filesystem links.
cleanupLocalEvents :: IO ()
cleanupLocalEvents = do
    root <- eventDirectory
    now <- getPOSIXTime
    user <- getEffectiveUserID
    listed <- tryIO (listDirectory root)
    rows <- fmap concat $ forM (either (const []) id listed) \directory -> do
        let path = root </> directory
        status <- tryIO (getSymbolicLinkStatus path)
        case status of
            Right entry | isDirectory entry && fileOwner entry == user && isUUIDText (Text.pack directory) -> do
                files <- tryIO (listDirectory path)
                fmap concat $ forM (either (const []) id files) \name -> do
                    let file = path </> name
                    item <- tryIO (getSymbolicLinkStatus file)
                    pure $ case item of
                        Right details | isRegularFile details && fileOwner details == user ->
                            [(modificationTimeHiRes details, fromIntegral (fileSize details) :: Integer, file)]
                        _ -> []
            _ -> pure []
    let trim _ [] = pure ()
        trim retained ((modified, bytes, path) : rest)
            | now - modified > 86400 || (retained > 1024 * 1024 * 1024 && now - modified > 3600) =
                void (tryIO (removeFile path)) >> trim (retained - bytes) rest
            | otherwise = trim retained rest
    trim (sum [bytes | (_, bytes, _) <- rows]) (sortOn (\(modified, _, _) -> modified) rows)
    forM_ (either (const []) id listed) \directory ->
        whenUuid directory (void (tryIO (removeDirectory (root </> directory))))
  where
    whenUuid value action = if isUUIDText (Text.pack value) then action else pure ()
