module Agent.CLI.ComputerUse.Linux.Portal
    ( CapturedPortalFrame(..)
    , PortalBackendState(..)
    , PortalFrameState(..)
    , PortalPngFrame(..)
    , PortalState(..)
    , PortalStream(..)
    , beginPortalCaptureRequestWith
    , closePortalStateWith
    , ensurePortalStateReadyWith
    , invalidatePortalStateWhenWith
    , newPortalBackend
    , parsePortalStartResults
    , portalDisplayForFrame
    , portalDisplayForStream
    , portalKeysym
    , portalMethodCall
    , portalMouseButtonCode
    , portalRequestPathForSender
    , publishPortalFrameWith
    , readPortalPngFrame
    , requestResponseRule
    , runPortalBackendOperationWith
    , sessionClosedRule
    , stopPortalCaptureProcessWith
    , validatePortalOwnerUser
    , waitForPortalFrameAfter
    , withPortalStateInvalidation
    , withPortalCaptureRunningWith
    , withPortalCaptureReadiness
    , withPortalInputReadiness
    ) where

import Agent.CLI.ComputerUse.Backend
    ( CapturedDisplay(..)
    , ComputerBackend(..)
    , ComputerDisplay(..)
    , ScreenshotEncoding(..)
    )
import Agent.CLI.ComputerUse.Input
    ( ComputerKey(..)
    , Modifier(..)
    , MouseButton(..)
    , NamedKey(..)
    , parseComputerKeyCombination
    , parseModifiers
    , parseMouseButton
    )
import Agent.CLI.ComputerUse.Linux.Logind
    ( WaylandPortalTarget(..)
    , withLogindReadiness
    )
import Agent.Loop (ImageAttachment(..))
import Agent.Responses.Types
    ( ComputerAction(..)
    , ComputerPoint(..)
    , TaggedObject(..)
    )
import Codec.Picture
    ( Image
    , PixelRGB8
    , convertRGB8
    , decodeImage
    , encodeJpegAtQuality
    , encodePng
    , generateImage
    , imageHeight
    , imageWidth
    , pixelAt
    )
import Codec.Picture.Types (convertImage)
import Control.Concurrent
    ( MVar
    , modifyMVarMasked
    , newEmptyMVar
    , newMVar
    , readMVar
    , takeMVar
    , threadDelay
    , tryPutMVar
    , withMVar
    )
import Control.Concurrent.Async
    ( Async
    , async
    , cancel
    , waitCatch
    )
import Control.Concurrent.STM
    ( STM
    , TVar
    , atomically
    , modifyTVar'
    , newTVarIO
    , readTVar
    , readTVarIO
    , retry
    )
import qualified Control.Exception as Exception
import Control.Exception.Safe
    ( SomeException
    , bracket
    , catchAny
    , finally
    , generalBracket
    , mask
    , onException
    , throwIO
    , tryAny
    )
import Control.Monad
    ( forM
    , forM_
    , unless
    , void
    , when
    )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Bits ((.&.), (.|.))
import Data.Char (isAlphaNum, ord)
import Data.Int (Int32)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word32, Word64, Word8)
import DBus
    ( BusName
    , InterfaceName
    , MemberName
    , MethodCall(..)
    , MethodError
    , ObjectPath
    , Signal(..)
    , Variant
    , busName_
    , formatBusName
    , formatObjectPath
    , fromVariant
    , methodCall
    , methodReturnBody
    , objectPath_
    , toVariant
    )
import qualified DBus
import DBus.Client
    ( Client
    , ClientOptions(..)
    , MatchRule(..)
    , SignalHandler
    , addMatch
    , call
    , connectWithName
    , defaultClientOptions
    , disconnect
    , matchAny
    , removeMatch
    )
import qualified DBus.Socket as Socket
import DBus.Transport (SocketTransport)
import Numeric (showHex)
import System.Entropy (getEntropy)
import System.IO
    ( Handle
    , hClose
    , hSetBinaryMode
    )
import System.Posix.IO (closeFd, fdToHandle)
import qualified System.Posix.Signals as Posix
import System.Posix.Types (Fd)
import System.Process
    ( CreateProcess(..)
    , ProcessHandle
    , StdStream(..)
    , createProcess
    , getPid
    , interruptProcessGroupOf
    , proc
    , terminateProcess
    , waitForProcess
    )
import System.Timeout (timeout)

type PortalOptions = Map Text Variant

data PortalStream = PortalStream
    { portalStreamNodeId :: !Word32
    , portalStreamId :: !(Maybe Text)
    , portalStreamPositionX :: !Int
    , portalStreamPositionY :: !Int
    , portalStreamWidth :: !Int
    , portalStreamHeight :: !Int
    , portalStreamSourceType :: !(Maybe Word32)
    , portalStreamMappingId :: !(Maybe Text)
    , portalStreamPipeWireSerial :: !(Maybe Word64)
    } deriving (Eq, Show)

data PortalSession = PortalSession
    { portalSessionPath :: !ObjectPath
    , portalSessionStream :: !PortalStream
    , portalSessionClosedHandler :: !SignalHandler
    , portalSessionCapture :: !PortalCapture
    }

data PortalCapture = PortalCapture
    { portalCaptureProcess :: !ProcessHandle
    , portalCaptureOutput :: !Handle
    , portalCaptureErrors :: !Handle
    , portalCaptureFrameReader :: !(Async ())
    , portalCaptureErrorReader :: !(Async ())
    , portalCaptureFrameState :: !(TVar PortalFrameState)
    , portalCaptureRequestLock :: !(MVar ())
    , portalCaptureRequestGeneration :: !(TVar Word64)
    , portalCaptureProcessLock :: !(MVar ())
    }

data PortalPngFrame = PortalPngFrame
    { portalPngFrameBytes :: !BS.ByteString
    , portalPngFrameWidth :: !Int
    , portalPngFrameHeight :: !Int
    } deriving (Eq, Show)

data PortalFrameState
    = PortalFramePending
    | PortalFrameAvailable !Word64 !PortalPngFrame
    | PortalFrameFailed !Text
    deriving (Eq, Show)

data PortalState session
    = PortalUninitialized
    | PortalReady !session
    | PortalFailed !Text
    | PortalClosing !session
    | PortalClosed

data PortalReadinessStep
    = PortalReadinessDone !(Either Text ())
    | PortalReadinessRetry

data PortalRuntime = PortalRuntime
    { portalClient :: !Client
    , portalUniqueName :: !BusName
    , portalOwnerName :: !BusName
    , portalState :: !(MVar (PortalState PortalSession))
    , portalReadiness :: !(IO (Either Text ()))
    }

data PortalBackendState runtime
    = PortalBackendOpen !(Maybe runtime)
    | PortalBackendRefreshing !runtime
    | PortalBackendClosing !runtime
    | PortalBackendClosed

data CapturedPortalFrame = CapturedPortalFrame
    { portalFrameSequence :: !Word64
    , portalFramePng :: !PortalPngFrame
    } deriving (Eq, Show)

newPortalBackend
    :: WaylandPortalTarget
    -> IO (Either Text ())
    -> IO (Either Text ComputerBackend)
newPortalBackend target readiness =
    mask \restore -> do
        initialized <- restore (newPortalRuntime target readiness)
        case initialized of
            Left err -> pure (Left err)
            Right runtime -> do
                backendState <-
                    newMVar (PortalBackendOpen (Just runtime))
                let run retryAfterRefresh =
                        runPortalBackendOperationWith
                            backendState
                            (newPortalRuntime target readiness)
                            closePortalRuntime
                            (Text.isInfixOf portalAssociationError)
                            retryAfterRefresh
                    -- A readiness check is safe to replay. Input may already
                    -- have happened before its post-check detects replacement.
                pure $
                    Right ComputerBackend
                        { computerBackendEnsureReady =
                            run True ensurePortalReady
                        , computerBackendInspectDisplay =
                            run False inspectPortalDisplay
                        , computerBackendExecuteAction =
                            \display action ->
                                run False
                                    (\current ->
                                        executePortalAction
                                            current
                                            display
                                            action)
                        , computerBackendCaptureDisplay =
                            \encoding ->
                                run False
                                    (\current ->
                                        capturePortalDisplay
                                            current
                                            encoding)
                        , computerBackendClose =
                            closePortalBackendWith
                                backendState
                                closePortalRuntime
                        }

newPortalRuntime
    :: WaylandPortalTarget
    -> IO (Either Text ())
    -> IO (Either Text PortalRuntime)
newPortalRuntime target readiness = do
    attempted <- tryAny do
        either (fail . Text.unpack) pure =<< readiness
        (client, uniqueName) <- connectPortal target
        flip onException
            (disconnect client `catchAny` const (pure ())) do
                ownerName <- resolvePortalOwner client
                verifyPortalOwnerUser
                    client
                    target.waylandPortalUserId
                    ownerName
                let portalReadiness =
                        checkPortalReadiness
                            client
                            target.waylandPortalUserId
                            ownerName
                            readiness
                either (fail . Text.unpack) pure =<< portalReadiness
                state <- newMVar PortalUninitialized
                let runtime = PortalRuntime
                        { portalClient = client
                        , portalUniqueName = uniqueName
                        , portalOwnerName = ownerName
                        , portalState = state
                        , portalReadiness = portalReadiness
                        }
                pure runtime
    pure case attempted of
        Left exception ->
            Left
                ( "Unable to connect to the Wayland desktop portal: "
                    <> exceptionText exception
                )
        Right runtime -> Right runtime

runPortalBackendOperationWith
    :: MVar (PortalBackendState runtime)
    -> IO (Either Text runtime)
    -> (runtime -> IO ())
    -> (Text -> Bool)
    -> Bool
    -> (runtime -> IO (Either Text value))
    -> IO (Either Text value)
runPortalBackendOperationWith
        stateVar
        initialize
        closeRuntime
        shouldRefresh
        retryAfterRefresh
        operation =
    Exception.mask \restore -> do
        let initializeAndRun allowRefresh = do
                -- Keep the acquisition-to-state handoff masked. Constructors
                -- remain cancellable at interruptible operations and must
                -- clean up any partially acquired resources before throwing.
                initialized <- tryAllExceptions initialize
                case initialized of
                    Left exception ->
                        pure
                            ( PortalBackendOpen Nothing
                            , Left exception
                            )
                    Right (Left err) ->
                        pure
                            ( PortalBackendOpen Nothing
                            , Right (Left err)
                            )
                    Right (Right runtime) ->
                        runInitialized allowRefresh runtime

            runInitialized allowRefresh runtime = do
                attempted <-
                    tryAllExceptions (restore (operation runtime))
                case attempted of
                    Left exception ->
                        pure
                            ( PortalBackendOpen (Just runtime)
                            , Left exception
                            )
                    Right result@(Right _) ->
                        pure
                            ( PortalBackendOpen (Just runtime)
                            , Right result
                            )
                    Right result@(Left err)
                        | shouldRefresh err ->
                            retireAndMaybeRefresh
                                allowRefresh
                                runtime
                                result
                        | otherwise ->
                            pure
                                ( PortalBackendOpen (Just runtime)
                                , Right result
                                )

            retireAndMaybeRefresh allowRefresh runtime original = do
                retired <-
                    tryAllExceptions (restore (closeRuntime runtime))
                case retired of
                    Left exception ->
                        pure
                            ( PortalBackendRefreshing runtime
                            , Left exception
                            )
                    Right ()
                        | allowRefresh -> initializeAndRun False
                        | otherwise ->
                            pure
                                ( PortalBackendOpen Nothing
                                , Right original
                                )

            finishRefresh runtime = do
                retired <-
                    tryAllExceptions (restore (closeRuntime runtime))
                case retired of
                    Left exception ->
                        pure
                            ( PortalBackendRefreshing runtime
                            , Left exception
                            )
                    Right () ->
                        initializeAndRun retryAfterRefresh

            finishClose runtime = do
                closed <-
                    tryAllExceptions (restore (closeRuntime runtime))
                pure case closed of
                    Left exception ->
                        ( PortalBackendClosing runtime
                        , Left exception
                        )
                    Right () ->
                        ( PortalBackendClosed
                        , Right (Left portalBackendClosedError)
                        )

            step = \case
                PortalBackendOpen Nothing ->
                    initializeAndRun retryAfterRefresh
                PortalBackendOpen (Just runtime) ->
                    runInitialized retryAfterRefresh runtime
                PortalBackendRefreshing runtime ->
                    finishRefresh runtime
                PortalBackendClosing runtime ->
                    finishClose runtime
                PortalBackendClosed ->
                    pure
                        ( PortalBackendClosed
                        , Right (Left portalBackendClosedError)
                        )
        outcome <- modifyMVarMasked stateVar step
        either Exception.throwIO pure outcome

closePortalBackendWith
    :: MVar (PortalBackendState runtime)
    -> (runtime -> IO ())
    -> IO ()
closePortalBackendWith stateVar closeRuntime =
    Exception.mask \restore -> do
        let closeAndFinish runtime = do
                closed <-
                    tryAllExceptions (restore (closeRuntime runtime))
                pure case closed of
                    Left exception ->
                        (PortalBackendClosing runtime, Left exception)
                    Right () ->
                        (PortalBackendClosed, Right ())

            step = \case
                PortalBackendOpen Nothing ->
                    pure (PortalBackendClosed, Right ())
                PortalBackendOpen (Just runtime) ->
                    closeAndFinish runtime
                PortalBackendRefreshing runtime ->
                    closeAndFinish runtime
                PortalBackendClosing runtime ->
                    closeAndFinish runtime
                PortalBackendClosed ->
                    pure (PortalBackendClosed, Right ())
        outcome <- modifyMVarMasked stateVar step
        either Exception.throwIO pure outcome

tryAllExceptions :: IO value -> IO (Either SomeException value)
tryAllExceptions = Exception.try

portalBackendClosedError :: Text
portalBackendClosedError =
    "The Wayland computer-use backend has been closed."

connectPortal :: WaylandPortalTarget -> IO (Client, BusName)
connectPortal target =
    connectWithName portalClientOptions target.waylandPortalAddress

portalClientOptions :: ClientOptions SocketTransport
portalClientOptions =
    defaultClientOptions
        { clientSocketOptions =
            Socket.defaultSocketOptions
                { Socket.socketAuthenticator =
                    Socket.authenticatorWithUnixFds
                }
        }

resolvePortalOwner :: Client -> IO BusName
resolvePortalOwner client = do
    reply <-
        portalCallBounded
            directCallTimeout
            client
            ((methodCall dbusPath dbusInterface "GetNameOwner")
                { methodCallDestination = Just dbusDestination
                , methodCallBody =
                    [toVariant (formatBusName portalDestination)]
                })
    case methodReturnBody reply of
        [value]
            | Just owner <- fromVariant value ->
                pure (busName_ owner)
        _ ->
            fail
                "The session bus returned an invalid owner for the desktop portal."

verifyPortalOwnerUser :: Client -> Word32 -> BusName -> IO ()
verifyPortalOwnerUser client expectedUserId ownerName = do
    reply <-
        portalCallBounded
            directCallTimeout
            client
            ((methodCall dbusPath dbusInterface "GetConnectionUnixUser")
                { methodCallDestination = Just dbusDestination
                , methodCallBody =
                    [toVariant (formatBusName ownerName)]
                })
    actualUserId <- case methodReturnBody reply of
        [value]
            | Just userId <- fromVariant value ->
                pure userId
        _ ->
            fail
                "The session bus returned an invalid user for the desktop portal."
    either (fail . Text.unpack) pure
        (validatePortalOwnerUser expectedUserId actualUserId)

validatePortalOwnerUser :: Word32 -> Word32 -> Either Text ()
validatePortalOwnerUser expectedUserId actualUserId
    | actualUserId == expectedUserId = Right ()
    | otherwise =
        Left
            "The desktop portal does not belong to the verified graphical-session user."

checkPortalReadiness
    :: Client
    -> Word32
    -> BusName
    -> IO (Either Text ())
    -> IO (Either Text ())
checkPortalReadiness client expectedUserId expectedOwner readiness =
    readiness >>= \case
        Left err -> pure (Left err)
        Right () -> do
            attempted <- tryAny do
                currentOwner <- resolvePortalOwner client
                unless (currentOwner == expectedOwner) $
                    fail (Text.unpack portalAssociationError)
                verifyPortalOwnerUser client expectedUserId expectedOwner
            pure case attempted of
                Left _ -> Left portalAssociationError
                Right () -> Right ()

portalAssociationError :: Text
portalAssociationError =
    "Computer use cannot associate the desktop portal with the verified systemd-logind session."

ensurePortalReady :: PortalRuntime -> IO (Either Text ())
ensurePortalReady runtime =
    runtime.portalReadiness >>= \case
        Left err -> pure (Left err)
        Right () ->
            ensurePortalStateReadyWith
                runtime.portalState
                (initializePortalSessionChecked runtime)
                (closePortalSession runtime)

ensurePortalStateReadyWith
    :: MVar (PortalState session)
    -> IO session
    -> (session -> IO ())
    -> IO (Either Text ())
ensurePortalStateReadyWith stateVar initialize closeSession = do
    readinessStep <-
        Exception.mask \restore ->
            modifyMVarMasked stateVar \case
                PortalUninitialized -> do
                    attempted <- tryAny (restore initialize)
                    pure case attempted of
                        Left exception ->
                            let err =
                                    "Wayland portal initialization failed: "
                                        <> exceptionText exception
                            in
                                ( PortalUninitialized
                                , PortalReadinessDone (Left err)
                                )
                        Right session ->
                            ( PortalReady session
                            , PortalReadinessDone (Right ())
                            )
                state@(PortalReady _) ->
                    pure (state, PortalReadinessDone (Right ()))
                state@(PortalFailed err) ->
                    pure (state, PortalReadinessDone (Left err))
                PortalClosing session -> do
                    closed <- tryAny (restore (closeSession session))
                    pure case closed of
                        Left exception ->
                            ( PortalClosing session
                            , PortalReadinessDone
                                (Left
                                    ( "Wayland portal cleanup failed: "
                                        <> exceptionText exception
                                    ))
                            )
                        Right () ->
                            (PortalUninitialized, PortalReadinessRetry)
                PortalClosed ->
                    pure
                        ( PortalClosed
                        , PortalReadinessDone
                            (Left
                                "The Wayland computer-use session has been closed.")
                        )
    case readinessStep of
        PortalReadinessDone outcome -> pure outcome
        PortalReadinessRetry ->
            ensurePortalStateReadyWith stateVar initialize closeSession

initializePortalSessionChecked :: PortalRuntime -> IO PortalSession
initializePortalSessionChecked runtime = do
    completed <- newIORef Nothing
    checked <-
        withLogindReadiness runtime.portalReadiness $
            mask \restore -> do
                session <- restore (initializePortalSession runtime)
                writeIORef completed (Just session)
                pure (Right session)
    case checked of
        Right session -> pure session
        Left err -> do
            readIORef completed >>= mapM_ (closePortalSession runtime)
            fail (Text.unpack err)

initializePortalSession :: PortalRuntime -> IO PortalSession
initializePortalSession runtime = do
    validatePortalCapabilities runtime
    sessionToken <- newPortalToken "session"
    createResults <-
        portalRequest runtime remoteDesktopInterface "CreateSession"
            \requestOptions ->
                [toVariant
                    (Map.insert
                        "session_handle_token"
                        (toVariant sessionToken)
                        requestOptions)]
    sessionPath <-
        either (fail . Text.unpack) pure
            (requiredResult
                "session_handle"
                "The desktop portal did not return a session handle."
                createResults)
    closedHandler <-
        addMatch
            runtime.portalClient
            (sessionClosedRule runtime.portalOwnerName sessionPath)
            (\_ -> markPortalSessionClosed runtime sessionPath)
            `onException` closePortalSessionPath runtime sessionPath
    let cleanup =
            (removeMatch runtime.portalClient closedHandler
                `catchAny` const (pure ()))
                `finally` closePortalSessionPath runtime sessionPath
    flip onException cleanup do
        void $
            portalRequest runtime screenCastInterface "SelectSources"
                \requestOptions ->
                    [ toVariant sessionPath
                    , toVariant
                        (Map.unions
                            [ requestOptions
                            , Map.fromList
                                [ ("types", toVariant monitorSourceType)
                                , ("multiple", toVariant False)
                                , ("cursor_mode",
                                    toVariant embeddedCursorMode)
                                ]
                            ])
                    ]
        void $
            portalRequest runtime remoteDesktopInterface "SelectDevices"
                \requestOptions ->
                    [ toVariant sessionPath
                    , toVariant
                        (Map.insert
                            "types"
                            (toVariant requiredDeviceTypes)
                            requestOptions)
                    ]
        startResults <-
            portalRequest runtime remoteDesktopInterface "Start"
                \requestOptions ->
                    [ toVariant sessionPath
                    , toVariant ("" :: Text)
                    , toVariant requestOptions
                    ]
        stream <- either (fail . Text.unpack) pure
            (parsePortalStartResults startResults)
        capture <- startPortalCapture runtime sessionPath stream
        pure PortalSession
            { portalSessionPath = sessionPath
            , portalSessionStream = stream
            , portalSessionClosedHandler = closedHandler
            , portalSessionCapture = capture
            }

validatePortalCapabilities :: PortalRuntime -> IO ()
validatePortalCapabilities runtime = do
    sources <-
        getPortalWord32Property
            runtime
            screenCastInterfaceName
            "AvailableSourceTypes"
    cursors <-
        getPortalWord32Property
            runtime
            screenCastInterfaceName
            "AvailableCursorModes"
    devices <-
        getPortalWord32Property
            runtime
            remoteDesktopInterfaceName
            "AvailableDeviceTypes"
    unless (sources .&. monitorSourceType == monitorSourceType) $
        fail "The desktop portal does not support monitor capture."
    unless (cursors .&. embeddedCursorMode == embeddedCursorMode) $
        fail "The desktop portal does not support an embedded cursor."
    unless (devices .&. requiredDeviceTypes == requiredDeviceTypes) $
        fail
            "The desktop portal does not support both keyboard and pointer control."

getPortalWord32Property
    :: PortalRuntime
    -> Text
    -> Text
    -> IO Word32
getPortalWord32Property runtime interfaceName propertyName = do
    reply <-
        portalCallBounded
            directCallTimeout
            runtime.portalClient
            (portalMethodCall
                runtime.portalOwnerName
                propertiesInterface
                "Get"
                [ toVariant interfaceName
                , toVariant propertyName
                ])
    case methodReturnBody reply of
        [outer]
            | Just inner <- fromVariant outer :: Maybe Variant
            , Just value <- fromVariant inner ->
                pure value
        _ ->
            fail
                ( "The desktop portal returned an invalid "
                    <> Text.unpack propertyName
                    <> " property."
                )

portalRequest
    :: PortalRuntime
    -> InterfaceName
    -> MemberName
    -> (PortalOptions -> [Variant])
    -> IO (Map Text Variant)
portalRequest runtime interface member bodyForOptions = do
    token <- newPortalToken "request"
    let expectedPathText =
            portalRequestPathForSender
                (Text.pack (formatBusName runtime.portalUniqueName))
                token
    unless (validPortalPath expectedPathText) $
        fail "Generated an invalid desktop portal request path."
    let expectedPath = objectPath_ (Text.unpack expectedPathText)
        requestOptions =
            Map.singleton "handle_token" (toVariant token)
        request =
            portalMethodCall
                runtime.portalOwnerName
                interface
                member
                (bodyForOptions requestOptions)
    responseVar <- newEmptyMVar
    waited <-
        (timeout requestTimeout do
            bracket
                (addMatch
                    runtime.portalClient
                    (requestResponseRule runtime.portalOwnerName expectedPath)
                    \signal ->
                        void
                            (tryPutMVar responseVar
                                (parsePortalResponseSignal signal)))
                (removeMatch runtime.portalClient)
                \_ -> do
                    reply <- portalCall runtime.portalClient request
                    returnedPath <- case methodReturnBody reply of
                        [value]
                            | Just path <- fromVariant value ->
                                pure path
                        _ ->
                            fail
                                "The desktop portal returned an invalid request handle."
                    unless (returnedPath == expectedPath) $
                        fail
                            "The desktop portal returned an unexpected request handle."
                    takeMVar responseVar)
            `onException` closePortalRequest runtime expectedPath
    case waited of
        Nothing -> do
            closePortalRequest runtime expectedPath
            fail "The desktop portal request timed out."
        Just (Left err) -> fail (Text.unpack err)
        Just (Right (responseCode, results)) ->
            case responseCode of
                0 -> pure results
                1 -> fail "The desktop portal request was cancelled."
                2 -> fail "The desktop portal request was denied or failed."
                other ->
                    fail
                        ("The desktop portal returned response code "
                            <> show other
                            <> ".")

parsePortalResponseSignal
    :: Signal
    -> Either Text (Word32, Map Text Variant)
parsePortalResponseSignal signal =
    case signal.signalBody of
        [codeValue, resultValue]
            | Just code <- fromVariant codeValue
            , Just results <- fromVariant resultValue ->
                Right (code, results)
        _ -> Left "The desktop portal returned an invalid response signal."

portalRequestPathForSender :: Text -> Text -> Text
portalRequestPathForSender sender token =
    "/org/freedesktop/portal/desktop/request/"
        <> senderComponent
        <> "/"
        <> token
  where
    senderComponent =
        Text.map
            (\character -> if character == '.' then '_' else character)
            (Text.dropWhile (== ':') sender)

validPortalPath :: Text -> Bool
validPortalPath path =
    not (Text.null path)
        && Text.all
            (\character ->
                character == '/'
                    || character == '_'
                    || isAlphaNum character)
            path

newPortalToken :: Text -> IO Text
newPortalToken prefix = do
    bytes <- getEntropy 16
    pure
        ( prefix
            <> "_"
            <> Text.pack (concatMap byteHex (BS.unpack bytes))
        )
  where
    byteHex byte =
        case showHex byte "" of
            [digit] -> ['0', digit]
            digits -> digits

requestResponseRule :: BusName -> ObjectPath -> MatchRule
requestResponseRule owner path =
    matchAny
        { matchSender = Just owner
        , matchPath = Just path
        , matchInterface = Just requestInterface
        , matchMember = Just "Response"
        }

sessionClosedRule :: BusName -> ObjectPath -> MatchRule
sessionClosedRule owner path =
    matchAny
        { matchSender = Just owner
        , matchPath = Just path
        , matchInterface = Just sessionInterface
        , matchMember = Just "Closed"
        }

markPortalSessionClosed :: PortalRuntime -> ObjectPath -> IO ()
markPortalSessionClosed runtime closedPath =
    invalidatePortalStateWhenWith
        runtime.portalState
        (\session -> session.portalSessionPath == closedPath)
        "The desktop portal closed the computer-use session."
        (disposePortalSession runtime)

closePortalRequest :: PortalRuntime -> ObjectPath -> IO ()
closePortalRequest runtime requestPath =
    void $
        timeout closeCallTimeout
            (portalCall runtime.portalClient
                (portalObjectCall
                    runtime.portalOwnerName
                    requestPath
                    requestInterface
                    "Close"
                    []))
        `catchAny` const (pure Nothing)

closePortalSessionPath :: PortalRuntime -> ObjectPath -> IO ()
closePortalSessionPath runtime sessionPath =
    void $
        timeout closeCallTimeout
            (portalCall runtime.portalClient
                (portalObjectCall
                    runtime.portalOwnerName
                    sessionPath
                    sessionInterface
                    "Close"
                    []))
        `catchAny` const (pure Nothing)

disposePortalSession :: PortalRuntime -> PortalSession -> IO ()
disposePortalSession runtime session =
    (removeMatch
        runtime.portalClient
        session.portalSessionClosedHandler
        `catchAny` const (pure ()))
        `finally`
            closePortalCapture session.portalSessionCapture

closePortalSession :: PortalRuntime -> PortalSession -> IO ()
closePortalSession runtime session =
    disposePortalSession runtime session
        `finally`
            closePortalSessionPath runtime session.portalSessionPath

closePortalRuntime :: PortalRuntime -> IO ()
closePortalRuntime runtime = do
    closePortalStateWith
        runtime.portalState
        (closePortalSession runtime)
    disconnect runtime.portalClient `catchAny` const (pure ())

closePortalStateWith
    :: MVar (PortalState session)
    -> (session -> IO ())
    -> IO ()
closePortalStateWith stateVar closeSession =
    Exception.mask \restore -> do
        let closeAndFinish session = do
                closed <-
                    tryAllExceptions (restore (closeSession session))
                pure case closed of
                    Left exception ->
                        (PortalClosing session, Left exception)
                    Right () ->
                        (PortalClosed, Right ())
            step = \case
                PortalReady session -> closeAndFinish session
                PortalClosing session -> closeAndFinish session
                PortalUninitialized ->
                    pure (PortalClosed, Right ())
                PortalFailed _ ->
                    pure (PortalClosed, Right ())
                PortalClosed ->
                    pure (PortalClosed, Right ())
        outcome <- modifyMVarMasked stateVar step
        either Exception.throwIO pure outcome

invalidatePortalStateWhenWith
    :: MVar (PortalState session)
    -> (session -> Bool)
    -> Text
    -> (session -> IO ())
    -> IO ()
invalidatePortalStateWhenWith stateVar matches _err closeSession =
    Exception.mask \restore -> do
        let closeAndReset session = do
                closed <-
                    tryAllExceptions (restore (closeSession session))
                pure case closed of
                    Left exception ->
                        (PortalClosing session, Left exception)
                    Right () ->
                        (PortalUninitialized, Right ())
            step = \case
                PortalReady session
                    | matches session -> closeAndReset session
                PortalClosing session
                    | matches session -> closeAndReset session
                state -> pure (state, Right ())
        outcome <- modifyMVarMasked stateVar step
        either Exception.throwIO pure outcome

withPortalStateInvalidation
    :: MVar (PortalState session)
    -> (session -> Bool)
    -> Text
    -> (session -> IO ())
    -> IO value
    -> IO value
withPortalStateInvalidation stateVar matches err closeSession action =
    action
        `onException`
            invalidatePortalStateWhenWith
                stateVar
                matches
                err
                closeSession

inspectPortalDisplay :: PortalRuntime -> IO (Either Text ComputerDisplay)
inspectPortalDisplay runtime =
    ensurePortalReady runtime >>= \case
        Left err -> pure (Left err)
        Right () ->
            withPortalCaptureReadiness runtime.portalReadiness $
                withPortalSessionOperation runtime "display inspection"
                    \session -> do
                        frame <-
                            capturePortalFrame session.portalSessionCapture
                        pure
                            (portalDisplayForPngFrame
                                session
                                frame.portalFramePng)

capturePortalDisplay
    :: PortalRuntime
    -> ScreenshotEncoding
    -> IO (Either Text CapturedDisplay)
capturePortalDisplay runtime encoding =
    ensurePortalReady runtime >>= \case
        Left err -> pure (Left err)
        Right () ->
            withPortalCaptureReadiness runtime.portalReadiness $
                withPortalSessionOperation runtime "screen capture" \session -> do
                    frame <-
                        capturePortalFrame session.portalSessionCapture
                    image <-
                        encodePortalFrame
                            encoding
                            session.portalSessionStream
                            frame.portalFramePng
                    pure CapturedDisplay
                        { capturedComputerDisplay =
                            portalDisplayForPngFrame
                                session
                                frame.portalFramePng
                        , capturedComputerImage = image
                        }

withPortalCaptureReadiness
    :: IO (Either Text ())
    -> IO (Either Text value)
    -> IO (Either Text value)
withPortalCaptureReadiness readiness capture = do
    result <- capture
    case result of
        Left err -> pure (Left err)
        Right value ->
            readiness >>= \case
                Left err -> pure (Left err)
                Right () -> pure (Right value)

executePortalAction
    :: PortalRuntime
    -> ComputerDisplay
    -> ComputerAction
    -> IO (Either Text ())
executePortalAction runtime expected action =
    ensurePortalReady runtime >>= \case
        Left err -> pure (Left err)
        Right () ->
            withPortalInputReadiness runtime.portalReadiness $
                withPortalSessionOperation runtime "input injection" \session -> do
                    let stream = session.portalSessionStream
                        expectedIdentity =
                            portalDisplayForFrame
                                session.portalSessionPath
                                stream
                                ( expected.computerDisplayFrameWidth
                                , expected.computerDisplayFrameHeight
                                )
                    unless
                        (expectedIdentity == expected)
                        (fail
                            "The selected portal stream changed during computer use.")
                    executePortalActionUnchecked runtime session stream action

withPortalInputReadiness
    :: IO (Either Text ())
    -> IO (Either Text value)
    -> IO (Either Text value)
withPortalInputReadiness = withLogindReadiness

withPortalSessionOperation
    :: PortalRuntime
    -> Text
    -> (PortalSession -> IO value)
    -> IO (Either Text value)
withPortalSessionOperation runtime operation action = do
    state <- readMVar runtime.portalState
    case state of
        PortalReady session -> do
            let matchesSession current =
                    current.portalSessionPath
                        == session.portalSessionPath
                interruptedError =
                    "Wayland portal "
                        <> operation
                        <> " was interrupted."
            attempted <-
                tryAny $
                    withPortalStateInvalidation
                        runtime.portalState
                        matchesSession
                        interruptedError
                        (closePortalSession runtime)
                        (action session)
            case attempted of
                Left exception -> do
                    let err =
                            "Wayland portal "
                                <> operation
                                <> " failed: "
                                <> exceptionText exception
                    invalidatePortalStateWhenWith
                        runtime.portalState
                        matchesSession
                        err
                        (closePortalSession runtime)
                    pure (Left err)
                Right value -> pure (Right value)
        PortalFailed err -> pure (Left err)
        PortalClosing _ ->
            pure (Left "The Wayland computer-use session is being closed.")
        PortalClosed ->
            pure (Left "The Wayland computer-use session has been closed.")
        PortalUninitialized ->
            pure (Left "The Wayland portal session is not initialized.")

executePortalActionUnchecked
    :: PortalRuntime
    -> PortalSession
    -> PortalStream
    -> ComputerAction
    -> IO ()
executePortalActionUnchecked runtime session stream = \case
    ScreenshotAction -> pure ()
    WaitAction -> threadDelay 2000000
    TypeAction value ->
        forM_ (Text.unpack value) \character ->
            tapKeysym runtime session (portalKeysym character)
    KeypressAction keys -> do
        (modifiers, key) <-
            either (fail . Text.unpack) pure
                (parseComputerKeyCombination keys)
        withPortalModifiers runtime session modifiers $
            tapKeysym runtime session (computerKeyKeysym key)
    ClickAction{clickX, clickY, clickButton, clickKeys} -> do
        button <-
            either (fail . Text.unpack) pure
                (parseMouseButton clickButton)
        modifiers <-
            either (fail . Text.unpack) pure
                (parseModifiers clickKeys)
        withPortalModifiers runtime session modifiers do
            notifyPointerAbsolute runtime session stream clickX clickY
            tapPointerButton
                runtime
                session
                (portalMouseButtonCode button)
    DoubleClickAction{doubleClickX, doubleClickY, doubleClickKeys} -> do
        modifiers <-
            either (fail . Text.unpack) pure
                (parseModifiers doubleClickKeys)
        withPortalModifiers runtime session modifiers do
            notifyPointerAbsolute
                runtime session stream doubleClickX doubleClickY
            tapPointerButton runtime session leftButtonCode
            threadDelay 80000
            tapPointerButton runtime session leftButtonCode
    MoveAction{moveX, moveY, moveKeys} -> do
        modifiers <-
            either (fail . Text.unpack) pure
                (parseModifiers moveKeys)
        withPortalModifiers runtime session modifiers $
            notifyPointerAbsolute runtime session stream moveX moveY
    ScrollAction
        { scrollX
        , scrollY
        , scrollDx
        , scrollDy
        , scrollKeys
        } -> do
            modifiers <-
                either (fail . Text.unpack) pure
                    (parseModifiers scrollKeys)
            withPortalModifiers runtime session modifiers do
                notifyPointerAbsolute runtime session stream scrollX scrollY
                notifyPointerAxis runtime session scrollDx scrollDy
    DragAction{dragPath, dragKeys} -> do
        modifiers <-
            either (fail . Text.unpack) pure
                (parseModifiers dragKeys)
        withPortalModifiers runtime session modifiers $
            dragPortalPointer runtime session stream dragPath
    UnknownComputerAction tagged ->
        fail
            ("Unsupported computer action: "
                <> Text.unpack tagged.tag)

withPortalModifiers
    :: PortalRuntime
    -> PortalSession
    -> [Modifier]
    -> IO value
    -> IO value
withPortalModifiers _ _ [] action = action
withPortalModifiers runtime session modifiers action = do
    let keysyms = map modifierKeysym modifiers
    fst <$> generalBracket
        (pure ())
        (\() _ -> releaseKeysyms runtime session keysyms)
        (\() -> do
            forM_ keysyms \keysym ->
                notifyKeyboardKeysym runtime session keysym keyPressed
            action)

releaseKeysyms
    :: PortalRuntime
    -> PortalSession
    -> [Int32]
    -> IO ()
releaseKeysyms runtime session keysyms = do
    results <-
        forM (reverse keysyms) \keysym ->
            tryAny
                (notifyKeyboardKeysym
                    runtime session keysym keyReleased)
    case [exception | Left exception <- results] of
        exception : _ -> throwIO (exception :: SomeException)
        [] -> pure ()

tapKeysym :: PortalRuntime -> PortalSession -> Int32 -> IO ()
tapKeysym runtime session keysym =
    (notifyKeyboardKeysym runtime session keysym keyPressed
        >> threadDelay 1000)
        `finally`
            notifyKeyboardKeysym runtime session keysym keyReleased

tapPointerButton :: PortalRuntime -> PortalSession -> Int32 -> IO ()
tapPointerButton runtime session button =
    (notifyPointerButton runtime session button buttonPressed
        >> threadDelay 20000)
        `finally`
            notifyPointerButton runtime session button buttonReleased

dragPortalPointer
    :: PortalRuntime
    -> PortalSession
    -> PortalStream
    -> [ComputerPoint]
    -> IO ()
dragPortalPointer _ _ _ [] =
    fail "Computer drag path is empty."
dragPortalPointer _ _ _ [_] =
    fail "Computer drag path needs at least two points."
dragPortalPointer runtime session stream
    (ComputerPoint firstX firstY : rest) = do
        notifyPointerAbsolute runtime session stream firstX firstY
        (notifyPointerButton
            runtime session leftButtonCode buttonPressed
            >> forM_ rest movePoint)
            `finally`
                notifyPointerButton
                    runtime session leftButtonCode buttonReleased
  where
    movePoint ComputerPoint{pointX, pointY} = do
        threadDelay 20000
        notifyPointerAbsolute runtime session stream pointX pointY

notifyPointerAbsolute
    :: PortalRuntime
    -> PortalSession
    -> PortalStream
    -> Int
    -> Int
    -> IO ()
notifyPointerAbsolute runtime session stream x y =
    notifyPortal runtime "NotifyPointerMotionAbsolute"
        [ toVariant session.portalSessionPath
        , toVariant emptyPortalOptions
        , toVariant stream.portalStreamNodeId
        , toVariant (fromIntegral x :: Double)
        , toVariant (fromIntegral y :: Double)
        ]

notifyPointerButton
    :: PortalRuntime
    -> PortalSession
    -> Int32
    -> Word32
    -> IO ()
notifyPointerButton runtime session button state =
    notifyPortal runtime "NotifyPointerButton"
        [ toVariant session.portalSessionPath
        , toVariant emptyPortalOptions
        , toVariant button
        , toVariant state
        ]

notifyPointerAxis
    :: PortalRuntime
    -> PortalSession
    -> Int
    -> Int
    -> IO ()
notifyPointerAxis _ _ 0 0 = pure ()
notifyPointerAxis runtime session dx dy = do
    notifyPortal runtime "NotifyPointerAxis"
        [ toVariant session.portalSessionPath
        , toVariant emptyPortalOptions
        , toVariant (fromIntegral dx :: Double)
        , toVariant (fromIntegral dy :: Double)
        ]
    notifyPortal runtime "NotifyPointerAxis"
        [ toVariant session.portalSessionPath
        , toVariant
            (Map.singleton "finish" (toVariant True) :: PortalOptions)
        , toVariant (0 :: Double)
        , toVariant (0 :: Double)
        ]

notifyKeyboardKeysym
    :: PortalRuntime
    -> PortalSession
    -> Int32
    -> Word32
    -> IO ()
notifyKeyboardKeysym runtime session keysym state =
    notifyPortal runtime "NotifyKeyboardKeysym"
        [ toVariant session.portalSessionPath
        , toVariant emptyPortalOptions
        , toVariant keysym
        , toVariant state
        ]

notifyPortal :: PortalRuntime -> MemberName -> [Variant] -> IO ()
notifyPortal runtime member body =
    void $
        portalCallBounded
            directCallTimeout
            runtime.portalClient
            (portalMethodCall
                runtime.portalOwnerName
                remoteDesktopInterface
                member
                body)

capturePortalFrame :: PortalCapture -> IO CapturedPortalFrame
capturePortalFrame capture =
    withMVar capture.portalCaptureRequestLock \() ->
        mask \restore -> do
            baseline <-
                beginPortalCaptureRequestWith
                    capture.portalCaptureProcessLock
                    capture.portalCaptureRequestGeneration
                    capture.portalCaptureFrameState
                    (resumePortalCaptureProcess capture.portalCaptureProcess)
            restore
                (waitForPortalFrameAfter
                    captureRefreshTimeout
                    baseline
                    capture.portalCaptureFrameState
                    >>= either (fail . Text.unpack) pure)
                `finally`
                    suspendPortalCaptureWith
                        capture.portalCaptureProcessLock
                        (pausePortalCaptureProcess capture.portalCaptureProcess)

beginPortalCaptureRequestWith
    :: MVar ()
    -> TVar Word64
    -> TVar PortalFrameState
    -> IO ()
    -> IO Word64
beginPortalCaptureRequestWith processLock requestGeneration frameState resume =
    Exception.mask_ $
        withMVar processLock \() -> do
            baseline <-
                readTVarIO frameState >>= \case
                    PortalFramePending -> pure 0
                    PortalFrameAvailable sequenceNumber _ ->
                        pure sequenceNumber
                    PortalFrameFailed err -> fail (Text.unpack err)
            atomically (modifyTVar' requestGeneration (+ 1))
            resume
            pure baseline

publishPortalFrameWith
    :: MVar ()
    -> TVar Word64
    -> TVar PortalFrameState
    -> IO ()
    -> Word64
    -> PortalPngFrame
    -> IO Bool
publishPortalFrameWith
        processLock
        requestGeneration
        frameState
        suspend
        frameGeneration
        pngFrame =
    fst <$>
        publishPortalFrameForReaderWith
            processLock
            requestGeneration
            frameState
            suspend
            frameGeneration
            pngFrame

publishPortalFrameForReaderWith
    :: MVar ()
    -> TVar Word64
    -> TVar PortalFrameState
    -> IO ()
    -> Word64
    -> PortalPngFrame
    -> IO (Bool, Word64)
publishPortalFrameForReaderWith
        processLock
        requestGeneration
        frameState
        suspend
        frameGeneration
        pngFrame =
    Exception.mask_ $
        withMVar processLock \() -> do
            currentGeneration <- readTVarIO requestGeneration
            if frameGeneration /= currentGeneration
                then pure (False, currentGeneration)
                else do
                    suspend
                    atomically $
                        modifyTVar' frameState \case
                            PortalFramePending ->
                                PortalFrameAvailable 1 pngFrame
                            PortalFrameAvailable sequenceNumber _ ->
                                PortalFrameAvailable
                                    (sequenceNumber + 1)
                                    pngFrame
                            failed@(PortalFrameFailed _) -> failed
                    pure (True, currentGeneration)

suspendPortalCaptureWith :: MVar () -> IO () -> IO ()
suspendPortalCaptureWith processLock suspend =
    Exception.mask_ (withMVar processLock (const suspend))

withPortalCaptureRunningWith
    :: IO ()
    -> IO ()
    -> IO value
    -> IO value
withPortalCaptureRunningWith resume suspend action =
    mask \restore -> do
        resume
        restore action `finally` suspend

waitForPortalFrameAfter
    :: Int
    -> Word64
    -> TVar PortalFrameState
    -> IO (Either Text CapturedPortalFrame)
waitForPortalFrameAfter microseconds baseline stateVar = do
    waited <-
        timeout microseconds $
            atomically (portalFrameAfter baseline stateVar)
    pure case waited of
        Nothing ->
            Left "GStreamer portal capture did not produce a fresh frame."
        Just result -> result

latestPortalFrame :: PortalCapture -> IO CapturedPortalFrame
latestPortalFrame capture = do
    waited <-
        timeout captureTimeout $
            atomically
                (currentPortalFrame capture.portalCaptureFrameState)
    case waited of
        Nothing -> fail "GStreamer portal capture timed out."
        Just (Left err) -> fail (Text.unpack err)
        Just (Right frame) -> pure frame

currentPortalFrame
    :: TVar PortalFrameState
    -> STM (Either Text CapturedPortalFrame)
currentPortalFrame stateVar =
    readTVar stateVar >>= \case
        PortalFramePending -> retry
        PortalFrameAvailable sequenceNumber pngFrame ->
            pure
                (Right CapturedPortalFrame
                    { portalFrameSequence = sequenceNumber
                    , portalFramePng = pngFrame
                    })
        PortalFrameFailed err -> pure (Left err)

portalFrameAfter
    :: Word64
    -> TVar PortalFrameState
    -> STM (Either Text CapturedPortalFrame)
portalFrameAfter baseline stateVar =
    currentPortalFrame stateVar >>= \case
        Right frame
            | frame.portalFrameSequence <= baseline -> retry
        result -> pure result

startPortalCapture
    :: PortalRuntime
    -> ObjectPath
    -> PortalStream
    -> IO PortalCapture
startPortalCapture runtime sessionPath stream = do
    pipeWireFd <- openPipeWireRemote runtime sessionPath
    input <-
        fdToHandle pipeWireFd
            `onException` closeFd pipeWireFd
    bracket (pure input) hClose \pipeWireHandle -> do
        hSetBinaryMode pipeWireHandle True
        startGstreamerPortalCapture
            pipeWireHandle
            stream.portalStreamNodeId

openPipeWireRemote :: PortalRuntime -> ObjectPath -> IO Fd
openPipeWireRemote runtime sessionPath = do
    reply <-
        portalCallBounded
            directCallTimeout
            runtime.portalClient
            (portalMethodCall
                runtime.portalOwnerName
                screenCastInterface
                "OpenPipeWireRemote"
                [ toVariant sessionPath
                , toVariant emptyPortalOptions
                ])
    case methodReturnBody reply of
        [value]
            | Just pipeWireFd <- fromVariant value ->
                pure pipeWireFd
        _ ->
            fail "The desktop portal returned an invalid PipeWire descriptor."

startGstreamerPortalCapture
    :: Handle
    -> Word32
    -> IO PortalCapture
startGstreamerPortalCapture pipeWireHandle nodeId = do
    frameState <- newTVarIO PortalFramePending
    errorBuffer <- newTVarIO BS.empty
    requestLock <- newMVar ()
    requestGeneration <- newTVarIO 0
    processLock <- newMVar ()
    mask \restore -> do
        (_, maybeOutput, maybeErrors, processHandle) <-
            createProcess
                ( (proc "gst-launch-1.0"
                    [ "-q"
                    , "pipewiresrc"
                    , "fd=0"
                    , "path=" <> show nodeId
                    , "!"
                    , "videorate"
                    , "drop-only=true"
                    , "max-rate=4"
                    , "!"
                    , "videoconvert"
                    , "!"
                    , "pngenc"
                    , "snapshot=false"
                    , "!"
                    , "fdsink"
                    , "fd=1"
                    , "sync=false"
                    ])
                    { std_in = UseHandle pipeWireHandle
                    , std_out = CreatePipe
                    , std_err = CreatePipe
                    , close_fds = True
                    , create_group = True
                    }
                )
        case (maybeOutput, maybeErrors) of
            (Just outputHandle, Just errorHandle) -> do
                flip onException
                    (closeBarePortalCapture
                        processHandle
                        outputHandle
                        errorHandle) do
                            hSetBinaryMode outputHandle True
                            hSetBinaryMode errorHandle True
                            errorReader <-
                                async
                                    (drainPortalCaptureErrors
                                        errorHandle
                                        errorBuffer)
                            frameReader <-
                                async
                                    (runPortalFrameReader
                                        processHandle
                                        outputHandle
                                        errorBuffer
                                        frameState
                                        requestGeneration
                                        processLock)
                                    `onException` do
                                        cancel errorReader
                                        void (waitCatch errorReader)
                            let capture = PortalCapture
                                    { portalCaptureProcess = processHandle
                                    , portalCaptureOutput = outputHandle
                                    , portalCaptureErrors = errorHandle
                                    , portalCaptureFrameReader = frameReader
                                    , portalCaptureErrorReader = errorReader
                                    , portalCaptureFrameState = frameState
                                    , portalCaptureRequestLock = requestLock
                                    , portalCaptureRequestGeneration =
                                        requestGeneration
                                    , portalCaptureProcessLock = processLock
                                    }
                            void (restore (latestPortalFrame capture))
                                `onException` closePortalCapture capture
                            pure capture
            _ -> do
                void (tryAny (stopPortalCaptureProcess processHandle))
                forM_ maybeOutput \handle ->
                    void (tryAny (hClose handle))
                forM_ maybeErrors \handle ->
                    void (tryAny (hClose handle))
                fail "GStreamer did not expose the portal capture pipes."

runPortalFrameReader
    :: ProcessHandle
    -> Handle
    -> TVar BS.ByteString
    -> TVar PortalFrameState
    -> TVar Word64
    -> MVar ()
    -> IO ()
runPortalFrameReader
        processHandle
        outputHandle
        errorBuffer
        frameState
        requestGeneration
        processLock =
    loop 0
        `catchAny` \exception -> do
            details <- portalCaptureErrorDetails errorBuffer
            let err
                    | Text.null details =
                        "GStreamer portal capture stopped: "
                            <> exceptionText exception
                    | otherwise =
                        "GStreamer portal capture failed: " <> details
            Exception.mask_ $
                withMVar processLock \() ->
                    atomically $
                        modifyTVar' frameState \case
                            failed@(PortalFrameFailed _) -> failed
                            _ -> PortalFrameFailed err
  where
    -- Carry the generation chosen before each blocking read. A request bumps
    -- the generation before resuming GStreamer, so a PNG that was already
    -- buffered or in progress while idle cannot satisfy that request.
    loop frameGeneration = do
        pngFrame <- readPortalPngFrame outputHandle
        (_, nextGeneration) <-
            publishPortalFrameForReaderWith
                processLock
                requestGeneration
                frameState
                (pausePortalCaptureProcess processHandle)
                frameGeneration
                pngFrame
        loop nextGeneration

drainPortalCaptureErrors
    :: Handle
    -> TVar BS.ByteString
    -> IO ()
drainPortalCaptureErrors errorHandle errorBuffer =
    loop `catchAny` const (pure ())
  where
    loop = do
        chunk <- BS.hGetSome errorHandle 4096
        unless (BS.null chunk) do
            atomically $
                modifyTVar' errorBuffer
                    (BS.take maximumPortalErrorBytes . (<> chunk))
            loop

portalCaptureErrorDetails :: TVar BS.ByteString -> IO Text
portalCaptureErrorDetails errorBuffer =
    Text.strip
        . TextEncoding.decodeUtf8With (\_ _ -> Just '\xfffd')
        <$> readTVarIO errorBuffer

readPortalPngFrame :: Handle -> IO PortalPngFrame
readPortalPngFrame input = do
    signature <- readPortalBytes input (BS.length pngSignature)
    unless (signature == pngSignature) $
        fail "GStreamer returned an invalid PNG stream."
    chunks <- readChunks [] (toInteger (BS.length signature))
    let bytes = BS.concat (signature : reverse chunks)
    (width, height) <- validatePortalPngHeader bytes
    pure PortalPngFrame
        { portalPngFrameBytes = bytes
        , portalPngFrameWidth = width
        , portalPngFrameHeight = height
        }
  where
    readChunks chunks consumed = do
        header <- readPortalBytes input 8
        let payloadLength =
                portalBigEndianWord (BS.take 4 header)
            chunkType = BS.drop 4 header
            chunkBytes = payloadLength + 12
        when
            (payloadLength > toInteger (maxBound :: Int)
                || consumed + chunkBytes > maximumPortalPngBytes) $
            fail "The portal PNG frame is too large."
        body <-
            readPortalBytes
                input
                (fromInteger payloadLength + 4)
        let chunk = header <> body
        if chunkType == "IEND"
            then do
                unless (payloadLength == 0) $
                    fail "The portal PNG frame has an invalid terminator."
                pure (chunk : chunks)
            else
                readChunks
                    (chunk : chunks)
                    (consumed + chunkBytes)

decodePortalPngFrame :: PortalPngFrame -> IO (Image PixelRGB8)
decodePortalPngFrame frame = do
    dynamicImage <-
        either
            (\err ->
                fail
                    ("Unable to decode the portal screenshot: "
                        <> err))
            pure
            (decodeImage frame.portalPngFrameBytes)
    let image = convertRGB8 dynamicImage
    unless
        ( imageWidth image == frame.portalPngFrameWidth
            && imageHeight image == frame.portalPngFrameHeight
        ) $
        fail "The decoded portal screenshot dimensions changed."
    pure image

readPortalBytes :: Handle -> Int -> IO BS.ByteString
readPortalBytes handle byteCount =
    go [] byteCount
  where
    go chunks remaining
        | remaining == 0 = pure (BS.concat (reverse chunks))
        | otherwise = do
            chunk <- BS.hGetSome handle remaining
            when (BS.null chunk) $
                fail "GStreamer ended the portal PNG stream."
            go (chunk : chunks) (remaining - BS.length chunk)

validatePortalPngHeader :: BS.ByteString -> IO (Int, Int)
validatePortalPngHeader bytes = do
    let headerLength =
            portalBigEndianWord (BS.take 4 (BS.drop 8 bytes))
        headerType = BS.take 4 (BS.drop 12 bytes)
        width = portalBigEndianWord (BS.take 4 (BS.drop 16 bytes))
        height = portalBigEndianWord (BS.take 4 (BS.drop 20 bytes))
    unless
        ( BS.length bytes >= 33
            && headerLength == 13
            && headerType == "IHDR"
            && width <= toInteger maximumFrameDimension
            && height <= toInteger maximumFrameDimension
            && validFrameSize (fromInteger width) (fromInteger height)
        ) $
        fail "The portal returned an invalid PNG frame header."
    pure (fromInteger width, fromInteger height)

portalBigEndianWord :: BS.ByteString -> Integer
portalBigEndianWord =
    BS.foldl' (\value byte -> value * 256 + fromIntegral byte) 0

closeBarePortalCapture
    :: ProcessHandle
    -> Handle
    -> Handle
    -> IO ()
closeBarePortalCapture processHandle outputHandle errorHandle = do
    void (tryAny (stopPortalCaptureProcess processHandle))
    void (tryAny (hClose outputHandle))
    void (tryAny (hClose errorHandle))

closePortalCapture :: PortalCapture -> IO ()
closePortalCapture capture =
    mask \_ -> do
        atomically $
            modifyTVar' capture.portalCaptureFrameState \case
                failed@(PortalFrameFailed _) -> failed
                _ -> PortalFrameFailed
                    "The GStreamer portal capture has been closed."
        stopped <-
            tryAllExceptions
                (stopPortalCaptureProcess capture.portalCaptureProcess)
        forM_
            [ capture.portalCaptureFrameReader
            , capture.portalCaptureErrorReader
            ]
            \worker -> do
                cancel worker
                void (waitCatch worker)
        void (tryAny (hClose capture.portalCaptureOutput))
        void (tryAny (hClose capture.portalCaptureErrors))
        either Exception.throwIO pure stopped

stopPortalCaptureProcess :: ProcessHandle -> IO ()
stopPortalCaptureProcess processHandle =
    stopPortalCaptureProcessWith
        (resumePortalCaptureProcess processHandle)
        (interruptProcessGroupOf processHandle)
        (terminateProcess processHandle)
        (getPid processHandle)
        (Posix.signalProcessGroup Posix.sigKILL)
        (timeout closeCallTimeout (waitForProcess processHandle))

stopPortalCaptureProcessWith
    :: IO ()
    -> IO ()
    -> IO ()
    -> IO (Maybe processGroup)
    -> (processGroup -> IO ())
    -> IO (Maybe exitResult)
    -> IO ()
stopPortalCaptureProcessWith
    resumeProcess
    interruptProcess
    terminateProcess'
    getProcessGroup
    killProcessGroup
    waitForExit = do
    void (tryAny resumeProcess)
    void (tryAny interruptProcess)
    void (tryAny terminateProcess')
    waitForExit >>= \case
        Just _ -> pure ()
        Nothing -> do
            getProcessGroup >>= mapM_
                (\processGroup ->
                    void
                        (tryAny
                            (killProcessGroup processGroup)))
            waitForExit >>= \case
                Just _ -> pure ()
                Nothing ->
                    fail
                        "The GStreamer portal capture process did not exit after SIGKILL."

pausePortalCaptureProcess :: ProcessHandle -> IO ()
pausePortalCaptureProcess =
    signalPortalCaptureProcess Posix.sigSTOP

resumePortalCaptureProcess :: ProcessHandle -> IO ()
resumePortalCaptureProcess =
    signalPortalCaptureProcess Posix.sigCONT

signalPortalCaptureProcess :: Posix.Signal -> ProcessHandle -> IO ()
signalPortalCaptureProcess signal processHandle =
    getPid processHandle >>= \case
        Nothing -> fail "The GStreamer portal capture process has exited."
        Just processGroup -> Posix.signalProcessGroup signal processGroup

encodePortalFrame
    :: ScreenshotEncoding
    -> PortalStream
    -> PortalPngFrame
    -> IO ImageAttachment
encodePortalFrame encoding stream frame = do
    image <- decodePortalPngFrame frame
    let normalized =
            resizeImage
                stream.portalStreamWidth
                stream.portalStreamHeight
                image
    pure case encoding of
        ScreenshotPng ->
            ImageAttachment
                "image/png"
                (LBS.toStrict (encodePng normalized))
        ScreenshotJpeg ->
            ImageAttachment
                "image/jpeg"
                (LBS.toStrict
                    (encodeJpegAtQuality
                        (80 :: Word8)
                        (convertImage normalized)))

resizeImage :: Int -> Int -> Image PixelRGB8 -> Image PixelRGB8
resizeImage targetWidth targetHeight source
    | imageWidth source == targetWidth
        && imageHeight source == targetHeight =
        source
    | otherwise =
        generateImage
            (\x y ->
                pixelAt
                    source
                    (x * imageWidth source `div` targetWidth)
                    (y * imageHeight source `div` targetHeight))
            targetWidth
            targetHeight

portalDisplayForPngFrame
    :: PortalSession
    -> PortalPngFrame
    -> ComputerDisplay
portalDisplayForPngFrame session frame =
    portalDisplayForFrame
        session.portalSessionPath
        session.portalSessionStream
        (frame.portalPngFrameWidth, frame.portalPngFrameHeight)

portalDisplayForStream :: ObjectPath -> PortalStream -> ComputerDisplay
portalDisplayForStream sessionPath stream =
    portalDisplayForFrame
        sessionPath
        stream
        (stream.portalStreamWidth, stream.portalStreamHeight)

portalDisplayForFrame
    :: ObjectPath
    -> PortalStream
    -> (Int, Int)
    -> ComputerDisplay
portalDisplayForFrame sessionPath stream (frameWidth, frameHeight) =
    ComputerDisplay
        { computerDisplayId =
            Text.intercalate ":"
                [ "wayland-portal"
                , Text.pack
                    (formatObjectPath sessionPath)
                , Text.pack (show stream.portalStreamNodeId)
                , fromMaybe "-" stream.portalStreamId
                , Text.pack
                    (show
                        ( stream.portalStreamPositionX
                        , stream.portalStreamPositionY
                        ))
                , fromMaybe "-" stream.portalStreamMappingId
                , maybe "-" (Text.pack . show)
                    stream.portalStreamPipeWireSerial
                ]
        , computerDisplayOriginX = 0
        , computerDisplayOriginY = 0
        , computerDisplayWidth = stream.portalStreamWidth
        , computerDisplayHeight = stream.portalStreamHeight
        , computerDisplayFrameWidth = frameWidth
        , computerDisplayFrameHeight = frameHeight
        }

parsePortalStartResults
    :: Map Text Variant
    -> Either Text PortalStream
parsePortalStartResults results = do
    devices <-
        (requiredResult
            "devices"
            "The desktop portal did not report granted input devices."
            results :: Either Text Word32)
    unlessEither
        (devices .&. requiredDeviceTypes == requiredDeviceTypes)
        "The desktop portal did not grant both keyboard and pointer access."
    streams <-
        requiredResult
            "streams"
            "The desktop portal did not return a monitor stream."
            results
    case streams :: [(Word32, Map Text Variant)] of
        [(nodeId, properties)] ->
            parsePortalStream nodeId properties
        [] -> Left "The desktop portal did not return a monitor stream."
        _ -> Left "The desktop portal returned more than one monitor stream."

parsePortalStream
    :: Word32
    -> Map Text Variant
    -> Either Text PortalStream
parsePortalStream nodeId properties = do
    (rawWidth, rawHeight) <-
        requiredResult
            "size"
            "The desktop portal stream has no logical size."
            properties
    let width = fromIntegral (rawWidth :: Int32)
        height = fromIntegral (rawHeight :: Int32)
    unlessEither
        (validLogicalSize width height)
        "The desktop portal returned an invalid logical monitor size."
    (positionX, positionY) <-
        optionalResult "position" properties >>= \case
            Nothing -> Right (0 :: Int32, 0 :: Int32)
            Just value -> Right value
    sourceType <- optionalResult "source_type" properties
    unlessEither
        (maybe True (== monitorSourceType) sourceType)
        "The desktop portal returned a non-monitor stream."
    streamId <- optionalResult "id" properties
    mappingId <- optionalResult "mapping_id" properties
    pipeWireSerial <- optionalResult "pipewire-serial" properties
    pure PortalStream
        { portalStreamNodeId = nodeId
        , portalStreamId = streamId
        , portalStreamPositionX = fromIntegral positionX
        , portalStreamPositionY = fromIntegral positionY
        , portalStreamWidth = width
        , portalStreamHeight = height
        , portalStreamSourceType = sourceType
        , portalStreamMappingId = mappingId
        , portalStreamPipeWireSerial = pipeWireSerial
        }

requiredResult
    :: DBus.IsVariant value
    => Text
    -> Text
    -> Map Text Variant
    -> Either Text value
requiredResult name missingMessage values =
    case Map.lookup name values of
        Nothing -> Left missingMessage
        Just value ->
            maybe
                (Left
                    ("The desktop portal returned an invalid "
                        <> name
                        <> " value."))
                Right
                (fromVariant value)

optionalResult
    :: DBus.IsVariant value
    => Text
    -> Map Text Variant
    -> Either Text (Maybe value)
optionalResult name values =
    case Map.lookup name values of
        Nothing -> Right Nothing
        Just value ->
            maybe
                (Left
                    ("The desktop portal returned an invalid "
                        <> name
                        <> " value."))
                (Right . Just)
                (fromVariant value)

unlessEither :: Bool -> Text -> Either Text ()
unlessEither condition err
    | condition = Right ()
    | otherwise = Left err

portalKeysym :: Char -> Int32
portalKeysym character =
    case character of
        '\b' -> namedKeyKeysym KeyBackspace
        '\t' -> namedKeyKeysym KeyTab
        '\n' -> namedKeyKeysym KeyEnter
        '\r' -> namedKeyKeysym KeyEnter
        _ ->
            let codepoint = ord character
            in fromIntegral
                (if (codepoint >= 0x20 && codepoint <= 0x7e)
                        || (codepoint >= 0xa0 && codepoint <= 0xff)
                    then codepoint
                    else 0x01000000 .|. codepoint)

portalMouseButtonCode :: MouseButton -> Int32
portalMouseButtonCode = \case
    MouseLeft -> leftButtonCode
    MouseRight -> 0x111
    MouseMiddle -> 0x112
    MouseBack -> 0x113
    MouseForward -> 0x114

computerKeyKeysym :: ComputerKey -> Int32
computerKeyKeysym = \case
    ComputerNamedKey key -> namedKeyKeysym key
    ComputerTextKey value -> textKeysym value
    ComputerShortcutKey value -> textKeysym value
  where
    textKeysym value =
        case Text.uncons value of
            Just (character, rest)
                | Text.null rest -> portalKeysym character
            _ -> 0

modifierKeysym :: Modifier -> Int32
modifierKeysym = \case
    ModifierMeta -> namedKeyKeysym KeyMeta
    ModifierControl -> namedKeyKeysym KeyControl
    ModifierAlt -> namedKeyKeysym KeyAlt
    ModifierShift -> namedKeyKeysym KeyShift
    -- XF86XK_Fn, encoded with the X.Org evdev keysym offset.
    ModifierFunction -> 0x100811d0

namedKeyKeysym :: NamedKey -> Int32
namedKeyKeysym = \case
    KeyEnter -> 0xff0d
    KeyTab -> 0xff09
    KeySpace -> 0x20
    KeyBackspace -> 0xff08
    KeyEscape -> 0xff1b
    KeyMeta -> 0xffeb
    KeyShift -> 0xffe1
    KeyCapsLock -> 0xffe5
    KeyAlt -> 0xffe9
    KeyControl -> 0xffe3
    KeyHome -> 0xff50
    KeyPageUp -> 0xff55
    KeyDelete -> 0xffff
    KeyEnd -> 0xff57
    KeyPageDown -> 0xff56
    KeyLeft -> 0xff51
    KeyRight -> 0xff53
    KeyDown -> 0xff54
    KeyUp -> 0xff52

portalMethodCall
    :: BusName
    -> InterfaceName
    -> MemberName
    -> [Variant]
    -> MethodCall
portalMethodCall owner =
    portalObjectCall owner portalDesktopPath

portalObjectCall
    :: BusName
    -> ObjectPath
    -> InterfaceName
    -> MemberName
    -> [Variant]
    -> MethodCall
portalObjectCall owner path interface member body =
    (methodCall path interface member)
        { methodCallDestination = Just owner
        , methodCallBody = body
        }

portalCall :: Client -> MethodCall -> IO DBus.MethodReturn
portalCall client request = do
    result <- call client request
    either throwPortalMethodError pure result

portalCallBounded
    :: Int
    -> Client
    -> MethodCall
    -> IO DBus.MethodReturn
portalCallBounded microseconds client request =
    timeout microseconds (portalCall client request) >>= \case
        Nothing -> fail "The desktop portal D-Bus call timed out."
        Just reply -> pure reply

throwPortalMethodError :: MethodError -> IO value
throwPortalMethodError methodError =
    fail ("Desktop portal D-Bus method failed: " <> show methodError)

exceptionText :: SomeException -> Text
exceptionText = Text.pack . show

validLogicalSize :: Int -> Int -> Bool
validLogicalSize width height =
    width > 0
        && height > 0
        && width <= maximumFrameDimension
        && height <= maximumFrameDimension
        && toInteger width * toInteger height <= maximumFramePixels

validFrameSize :: Int -> Int -> Bool
validFrameSize = validLogicalSize

emptyPortalOptions :: PortalOptions
emptyPortalOptions = Map.empty

portalDestination :: BusName
portalDestination = "org.freedesktop.portal.Desktop"

dbusDestination :: BusName
dbusDestination = "org.freedesktop.DBus"

dbusPath :: ObjectPath
dbusPath = "/org/freedesktop/DBus"

dbusInterface :: InterfaceName
dbusInterface = "org.freedesktop.DBus"

portalDesktopPath :: ObjectPath
portalDesktopPath = "/org/freedesktop/portal/desktop"

screenCastInterface :: InterfaceName
screenCastInterface = "org.freedesktop.portal.ScreenCast"

screenCastInterfaceName :: Text
screenCastInterfaceName = "org.freedesktop.portal.ScreenCast"

remoteDesktopInterface :: InterfaceName
remoteDesktopInterface = "org.freedesktop.portal.RemoteDesktop"

remoteDesktopInterfaceName :: Text
remoteDesktopInterfaceName = "org.freedesktop.portal.RemoteDesktop"

propertiesInterface :: InterfaceName
propertiesInterface = "org.freedesktop.DBus.Properties"

requestInterface :: InterfaceName
requestInterface = "org.freedesktop.portal.Request"

sessionInterface :: InterfaceName
sessionInterface = "org.freedesktop.portal.Session"

monitorSourceType :: Word32
monitorSourceType = 1

embeddedCursorMode :: Word32
embeddedCursorMode = 2

requiredDeviceTypes :: Word32
requiredDeviceTypes = 1 .|. 2

keyReleased, keyPressed, buttonReleased, buttonPressed :: Word32
keyReleased = 0
keyPressed = 1
buttonReleased = 0
buttonPressed = 1

leftButtonCode :: Int32
leftButtonCode = 0x110

requestTimeout, captureTimeout, captureRefreshTimeout :: Int
directCallTimeout, closeCallTimeout :: Int
requestTimeout = 120000000
captureTimeout = 20000000
captureRefreshTimeout = 1000000
directCallTimeout = 10000000
closeCallTimeout = 2000000

maximumFrameDimension :: Int
maximumFrameDimension = 32768

maximumFramePixels :: Integer
maximumFramePixels = 100000000

maximumPortalPngBytes :: Integer
maximumPortalPngBytes = 512 * 1024 * 1024

maximumPortalErrorBytes :: Int
maximumPortalErrorBytes = 8192

pngSignature :: BS.ByteString
pngSignature = "\137PNG\r\n\SUB\n"
