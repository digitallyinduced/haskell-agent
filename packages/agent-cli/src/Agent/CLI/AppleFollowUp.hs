-- | On-device Apple Intelligence choice between steering and queueing a follow-up.
module Agent.CLI.AppleFollowUp
    ( AppleFollowUpTiming(..)
    , FollowUpRoute(..)
    , defaultAppleFollowUpTiming
    , defaultFollowUpRoute
    , classifyFollowUps
    , maintainAppleFollowUpRouter
    , maintainAppleFollowUpRouterTimed
    , parseFollowUpReadyJson
    , parseFollowUpRouteJson
    , routeRequestLine
    ) where

import Control.Concurrent (MVar, newMVar, threadDelay, withMVar)
import Control.Exception.Safe (bracket, finally, throwIO, tryAny)
import Control.Monad (void, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson (Value(..))
import qualified Data.ByteString.Lazy as LazyByteString
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as TextIO
import GHC.IO.Exception (IOErrorType(..))
import System.IO
    ( BufferMode(..)
    , Handle
    , hClose
    , hFlush
    , hSetBuffering
    , hSetEncoding
    , utf8
    )
import System.IO.Error (mkIOError)
import System.Posix.Signals (signalProcess, sigKILL)
import System.Process
    ( CreateProcess(..)
    , ProcessHandle
    , StdStream(..)
    , createProcess
    , getPid
    , proc
    , terminateProcess
    , waitForProcess
    )
import System.Timeout (timeout)

data FollowUpRoute
    = FollowUpSteer
    | FollowUpQueue
    deriving (Eq, Show)

data AppleFollowUpTiming = AppleFollowUpTiming
    { appleFollowUpReadyTimeoutMicros :: !Int
    , appleFollowUpRequestTimeoutMicros :: !Int
    }

defaultAppleFollowUpTiming :: AppleFollowUpTiming
defaultAppleFollowUpTiming =
    AppleFollowUpTiming
        { appleFollowUpReadyTimeoutMicros = 20_000_000
        , appleFollowUpRequestTimeoutMicros = 2_000_000
        }

routeTaskLimit :: Int
routeTaskLimit = 4000

routeMessageLimit :: Int
routeMessageLimit = 2000

defaultFollowUpRoute :: Text -> Text -> IO FollowUpRoute
defaultFollowUpRoute _ _ = pure FollowUpSteer

-- | Keep one helper process for the caller lifetime. The installed function
-- falls back to steering when the helper is not ready or a request fails.
maintainAppleFollowUpRouter
    :: FilePath
    -> ((Text -> Text -> IO FollowUpRoute) -> IO ())
    -> (Bool -> IO ())
    -> IO ()
maintainAppleFollowUpRouter =
    maintainAppleFollowUpRouterTimed defaultAppleFollowUpTiming

maintainAppleFollowUpRouterTimed
    :: AppleFollowUpTiming
    -> FilePath
    -> ((Text -> Text -> IO FollowUpRoute) -> IO ())
    -> (Bool -> IO ())
    -> IO ()
maintainAppleFollowUpRouterTimed timing executable install setEnabled =
    flip finally (do
        install defaultFollowUpRoute
        setEnabled False) $
        bracket (openRouteProcess executable) closeRouteProcess \session -> do
            ready <- awaitRouteReady timing session
            when ready $ do
                install (askRouteProcess timing session)
                setEnabled True
                void (waitForProcess session.routeChild)

-- | Classify every pair on one helper process. The first unusable response
-- stops the batch, so a transport failure is not reported as a steer.
classifyFollowUps
    :: AppleFollowUpTiming
    -> FilePath
    -> [(Text, Text)]
    -> IO (Either Text [FollowUpRoute])
classifyFollowUps timing executable items =
    bracket (openRouteProcess executable) closeRouteProcess \session -> do
        ready <- awaitRouteReady timing session
        if not ready
            then pure (Left "follow-up router did not become ready")
            else collect session (1 :: Int) items []
  where
    collect _ _ [] routes = pure (Right (reverse routes))
    collect session index ((task, message) : rest) routes =
        askRouteOutcome timing session task message >>= \case
            Right route ->
                collect session (index + 1) rest (route : routes)
            Left err ->
                pure $
                    Left $
                        "follow-up "
                            <> Text.pack (show index)
                            <> " was not classified: "
                            <> err

parseFollowUpReadyJson :: Text -> Bool
parseFollowUpReadyJson raw =
    case Aeson.decodeStrict (Text.encodeUtf8 (Text.strip raw)) of
        Just (Object object) ->
            case KeyMap.lookup "ready" object of
                Just (Bool True) -> True
                _ -> False
        _ -> False

parseFollowUpRouteJson :: Text -> Maybe FollowUpRoute
parseFollowUpRouteJson raw =
    case Aeson.decodeStrict (Text.encodeUtf8 (Text.strip raw)) of
        Just (Object object) ->
            case KeyMap.lookup "route" object of
                Just (String route) ->
                    case Text.toLower (Text.strip route) of
                        "steer" -> Just FollowUpSteer
                        "queue" -> Just FollowUpQueue
                        _ -> Nothing
                _ -> Nothing
        _ -> Nothing

routeRequestLine :: Text -> Text -> Text
routeRequestLine task message =
    Text.decodeUtf8 $
        LazyByteString.toStrict $
            Aeson.encode $
                Aeson.object
                    [ "task" Aeson..= Text.take routeTaskLimit task
                    , "message" Aeson..= Text.take routeMessageLimit message
                    ]

data RouteProcess = RouteProcess
    { routeInput :: !Handle
    , routeOutput :: !Handle
    , routeChild :: !ProcessHandle
    , routeLock :: !(MVar ())
    , routeAlive :: !(IORef Bool)
    }

openRouteProcess :: FilePath -> IO RouteProcess
openRouteProcess executable = do
    (inputMaybe, outputMaybe, _, child) <-
        createProcess
            (proc executable ["--route-serve"])
                { std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = NoStream
                , close_fds = True
                }
    case (inputMaybe, outputMaybe) of
        (Just input, Just output) -> do
            hSetEncoding input utf8
            hSetEncoding output utf8
            hSetBuffering input LineBuffering
            hSetBuffering output LineBuffering
            lock <- newMVar ()
            alive <- newIORef True
            pure RouteProcess
                { routeInput = input
                , routeOutput = output
                , routeChild = child
                , routeLock = lock
                , routeAlive = alive
                }
        _ -> do
            terminateProcess child
            void (waitForProcess child)
            throwIO
                (mkIOError
                    IllegalOperation
                    "follow-up router did not provide pipes"
                    Nothing
                    (Just executable))

awaitRouteReady :: AppleFollowUpTiming -> RouteProcess -> IO Bool
awaitRouteReady timing session =
    timeout timing.appleFollowUpReadyTimeoutMicros
        (TextIO.hGetLine session.routeOutput) >>= \case
            Just line -> pure (parseFollowUpReadyJson line)
            Nothing -> pure False

askRouteProcess
    :: AppleFollowUpTiming
    -> RouteProcess
    -> Text
    -> Text
    -> IO FollowUpRoute
askRouteProcess timing session task message =
    askRouteOutcome timing session task message >>= \case
        Right route -> pure route
        Left _ -> do
            markRouteFailed session
            pure FollowUpSteer

askRouteOutcome
    :: AppleFollowUpTiming
    -> RouteProcess
    -> Text
    -> Text
    -> IO (Either Text FollowUpRoute)
askRouteOutcome timing session task message = do
    result <- tryAny $ withMVar session.routeLock $ \() -> do
        alive <- readIORef session.routeAlive
        if not alive
            then pure AskUnavailable
            else do
                TextIO.hPutStrLn
                    session.routeInput
                    (routeRequestLine task message)
                hFlush session.routeInput
                response <-
                    timeout
                        timing.appleFollowUpRequestTimeoutMicros
                        (TextIO.hGetLine session.routeOutput)
                pure $ case response >>= parseFollowUpRouteJson of
                    Just route -> AskRoute route
                    Nothing -> AskFailed
    pure $ case result of
        Right (AskRoute route) -> Right route
        Right AskUnavailable -> Left "follow-up router is closed"
        Right AskFailed -> Left "follow-up router returned no decision"
        Left err -> Left (Text.pack (show err))

data AskOutcome
    = AskRoute !FollowUpRoute
    | AskUnavailable
    | AskFailed

markRouteFailed :: RouteProcess -> IO ()
markRouteFailed session =
    void $ tryAny $ do
        writeIORef session.routeAlive False
        terminateProcess session.routeChild

closeRouteProcess :: RouteProcess -> IO ()
closeRouteProcess session = do
    -- Kill before taking the request lock. A follow-up call holds that lock
    -- while it waits for the helper, and closing the process is what unblocks it.
    writeIORef session.routeAlive False
    void $ tryAny (terminateProcess session.routeChild)
    threadDelay 200_000
    void $ tryAny $ getPid session.routeChild >>= \case
        Just processId ->
            void $ tryAny (signalProcess sigKILL processId)
        Nothing ->
            pure ()
    void $ tryAny (hClose session.routeInput)
    void $ tryAny (waitForProcess session.routeChild)
