module Agent.CLI.ComputerUse.Linux.Portal
    ( PortalStream(..)
    , newPortalBackend
    , parsePortalStartResults
    , portalKeysym
    , portalMouseButtonCode
    , portalRequestPathForSender
    , requestResponseRule
    , sessionClosedRule
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
    , modifyMVar
    , modifyMVar_
    , newEmptyMVar
    , newMVar
    , readMVar
    , takeMVar
    , threadDelay
    , tryPutMVar
    )
import Control.Exception.Safe
    ( SomeException
    , bracket
    , catchAny
    , finally
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
    , getSessionAddress
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
import System.Directory
    ( getTemporaryDirectory
    , removeFile
    )
import System.Entropy (getEntropy)
import System.Exit (ExitCode(..))
import System.IO
    ( Handle
    , IOMode(..)
    , hClose
    , hSetBinaryMode
    , openBinaryFile
    , openBinaryTempFile
    )
import System.Posix.IO (closeFd, fdToHandle)
import System.Posix.Signals (sigKILL, signalProcessGroup)
import System.Posix.Types (Fd)
import System.Process
    ( CreateProcess(..)
    , ProcessHandle
    , StdStream(..)
    , getPid
    , interruptProcessGroupOf
    , proc
    , terminateProcess
    , waitForProcess
    , withCreateProcess
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
    }

data PortalState
    = PortalUninitialized
    | PortalReady !PortalSession
    | PortalFailed !Text
    | PortalClosed

data PortalRuntime = PortalRuntime
    { portalClient :: !Client
    , portalUniqueName :: !BusName
    , portalOwnerName :: !BusName
    , portalState :: !(MVar PortalState)
    , portalReadiness :: !(IO (Either Text ()))
    }

data CapturedPortalFrame = CapturedPortalFrame
    { portalFrameImage :: !(Image PixelRGB8)
    , portalFrameWidth :: !Int
    , portalFrameHeight :: !Int
    }

newPortalBackend
    :: IO (Either Text ())
    -> IO (Either Text ComputerBackend)
newPortalBackend readiness = do
    attempted <- tryAny do
        (client, uniqueName) <- connectPortal
        ownerName <-
            resolvePortalOwner client
                `onException`
                    (disconnect client `catchAny` const (pure ()))
        state <-
            newMVar PortalUninitialized
                `onException`
                    (disconnect client `catchAny` const (pure ()))
        let runtime = PortalRuntime
                { portalClient = client
                , portalUniqueName = uniqueName
                , portalOwnerName = ownerName
                , portalState = state
                , portalReadiness = readiness
                }
        pure ComputerBackend
            { computerBackendEnsureReady = ensurePortalReady runtime
            , computerBackendInspectDisplay = inspectPortalDisplay runtime
            , computerBackendExecuteAction = executePortalAction runtime
            , computerBackendCaptureDisplay = capturePortalDisplay runtime
            , computerBackendClose = closePortalRuntime runtime
            }
    pure case attempted of
        Left exception ->
            Left
                ( "Unable to connect to the Wayland desktop portal: "
                    <> exceptionText exception
                )
        Right backend -> Right backend

connectPortal :: IO (Client, BusName)
connectPortal =
    getSessionAddress >>= \case
        Nothing ->
            fail
                "DBUS_SESSION_BUS_ADDRESS is unavailable for the Wayland portal"
        Just address -> connectWithName portalClientOptions address

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

ensurePortalReady :: PortalRuntime -> IO (Either Text ())
ensurePortalReady runtime =
    runtime.portalReadiness >>= \case
        Left err -> pure (Left err)
        Right () ->
            modifyMVar runtime.portalState \case
                PortalUninitialized -> do
                    attempted <- tryAny (initializePortalSession runtime)
                    pure case attempted of
                        Left exception ->
                            let err =
                                    "Wayland portal initialization failed: "
                                        <> exceptionText exception
                            in (PortalFailed err, Left err)
                        Right session ->
                            (PortalReady session, Right ())
                state@(PortalReady _) -> pure (state, Right ())
                state@(PortalFailed err) -> pure (state, Left err)
                PortalClosed ->
                    pure
                        ( PortalClosed
                        , Left "The Wayland computer-use session has been closed."
                        )

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
        pure PortalSession
            { portalSessionPath = sessionPath
            , portalSessionStream = stream
            , portalSessionClosedHandler = closedHandler
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
    modifyMVar_ runtime.portalState \case
        PortalReady session
            | session.portalSessionPath == closedPath ->
                pure
                    (PortalFailed
                        "The desktop portal closed the computer-use session.")
        state -> pure state

closePortalRequest :: PortalRuntime -> ObjectPath -> IO ()
closePortalRequest runtime requestPath =
    void $
        timeout closeCallTimeout
            (portalCall runtime.portalClient
                (portalObjectCall
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
                    sessionPath
                    sessionInterface
                    "Close"
                    []))
        `catchAny` const (pure Nothing)

closePortalSession :: PortalRuntime -> PortalSession -> IO ()
closePortalSession runtime session =
    (removeMatch
        runtime.portalClient
        session.portalSessionClosedHandler
        `catchAny` const (pure ()))
        `finally`
            closePortalSessionPath runtime session.portalSessionPath

closePortalRuntime :: PortalRuntime -> IO ()
closePortalRuntime runtime = do
    session <-
        modifyMVar runtime.portalState \case
            PortalReady value -> pure (PortalClosed, Just value)
            _ -> pure (PortalClosed, Nothing)
    mapM_ (closePortalSession runtime) session
    disconnect runtime.portalClient `catchAny` const (pure ())

invalidatePortalRuntime :: PortalRuntime -> Text -> IO ()
invalidatePortalRuntime runtime err = do
    session <-
        modifyMVar runtime.portalState \case
            PortalReady value -> pure (PortalFailed err, Just value)
            PortalUninitialized -> pure (PortalFailed err, Nothing)
            state -> pure (state, Nothing)
    mapM_ (closePortalSession runtime) session

inspectPortalDisplay :: PortalRuntime -> IO (Either Text ComputerDisplay)
inspectPortalDisplay runtime =
    ensurePortalReady runtime >>= \case
        Left err -> pure (Left err)
        Right () ->
            withPortalSessionOperation runtime "screen capture" \session -> do
                frame <- capturePortalFrame runtime session
                pure
                    (portalDisplay
                        session
                        frame.portalFrameWidth
                        frame.portalFrameHeight)

capturePortalDisplay
    :: PortalRuntime
    -> ScreenshotEncoding
    -> IO (Either Text CapturedDisplay)
capturePortalDisplay runtime encoding =
    ensurePortalReady runtime >>= \case
        Left err -> pure (Left err)
        Right () ->
            withPortalSessionOperation runtime "screen capture" \session -> do
                frame <- capturePortalFrame runtime session
                pure CapturedDisplay
                    { capturedComputerDisplay =
                        portalDisplay
                            session
                            frame.portalFrameWidth
                            frame.portalFrameHeight
                    , capturedComputerImage =
                        encodePortalFrame
                            encoding
                            session.portalSessionStream
                            frame.portalFrameImage
                    }

executePortalAction
    :: PortalRuntime
    -> ComputerDisplay
    -> ComputerAction
    -> IO (Either Text ())
executePortalAction runtime expected action =
    ensurePortalReady runtime >>= \case
        Left err -> pure (Left err)
        Right () ->
            withPortalSessionOperation runtime "input injection" \session -> do
                let stream = session.portalSessionStream
                    expectedIdentity =
                        portalDisplay
                            session
                            expected.computerDisplayFrameWidth
                            expected.computerDisplayFrameHeight
                unless
                    ( expectedIdentity.computerDisplayId
                        == expected.computerDisplayId
                        && expectedIdentity.computerDisplayWidth
                            == expected.computerDisplayWidth
                        && expectedIdentity.computerDisplayHeight
                            == expected.computerDisplayHeight
                    )
                    (fail
                        "The selected portal stream changed during computer use.")
                executePortalActionUnchecked runtime session stream action

withPortalSessionOperation
    :: PortalRuntime
    -> Text
    -> (PortalSession -> IO value)
    -> IO (Either Text value)
withPortalSessionOperation runtime operation action = do
    state <- readMVar runtime.portalState
    case state of
        PortalReady session -> do
            attempted <- tryAny (action session)
            case attempted of
                Left exception -> do
                    let err =
                            "Wayland portal "
                                <> operation
                                <> " failed: "
                                <> exceptionText exception
                    invalidatePortalRuntime runtime err
                    pure (Left err)
                Right value -> pure (Right value)
        PortalFailed err -> pure (Left err)
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
    pressed <- tryAny $
        forM_ keysyms \keysym ->
            notifyKeyboardKeysym runtime session keysym keyPressed
    case pressed of
        Left exception -> do
            releaseKeysymsIgnoringErrors runtime session keysyms
            throwIO exception
        Right () ->
            action
                `finally`
                    releaseKeysyms runtime session keysyms

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

releaseKeysymsIgnoringErrors
    :: PortalRuntime
    -> PortalSession
    -> [Int32]
    -> IO ()
releaseKeysymsIgnoringErrors runtime session =
    mapM_
        (\keysym ->
            void
                (tryAny
                    (notifyKeyboardKeysym
                        runtime session keysym keyReleased)))
        . reverse

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
            (portalMethodCall remoteDesktopInterface member body)

capturePortalFrame
    :: PortalRuntime
    -> PortalSession
    -> IO CapturedPortalFrame
capturePortalFrame runtime session = do
    pipeWireFd <- openPipeWireRemote runtime session
    input <-
        fdToHandle pipeWireFd
            `onException` closeFd pipeWireFd
    bracket (pure input) hClose \pipeWireHandle -> do
        hSetBinaryMode pipeWireHandle True
        withTemporaryPath "agent-computer-use-wayland.png" \imagePath -> do
            withTemporaryPath "agent-computer-use-gstreamer.log" \logPath -> do
                runGstreamerCapture
                    pipeWireHandle
                    session.portalSessionStream.portalStreamNodeId
                    imagePath
                    logPath
                bytes <- BS.readFile imagePath
                when (BS.null bytes) $
                    fail "GStreamer returned an empty portal screenshot."
                dynamicImage <-
                    either
                        (\err ->
                            fail
                                ("Unable to decode the portal screenshot: "
                                    <> err))
                        pure
                        (decodeImage bytes)
                let image = convertRGB8 dynamicImage
                    width = imageWidth image
                    height = imageHeight image
                unless (validFrameSize width height) $
                    fail "The portal returned an invalid screenshot size."
                pure CapturedPortalFrame
                    { portalFrameImage = image
                    , portalFrameWidth = width
                    , portalFrameHeight = height
                    }

openPipeWireRemote :: PortalRuntime -> PortalSession -> IO Fd
openPipeWireRemote runtime session = do
    reply <-
        portalCallBounded
            directCallTimeout
            runtime.portalClient
            (portalMethodCall
                screenCastInterface
                "OpenPipeWireRemote"
                [ toVariant session.portalSessionPath
                , toVariant emptyPortalOptions
                ])
    case methodReturnBody reply of
        [value]
            | Just pipeWireFd <- fromVariant value ->
                pure pipeWireFd
        _ ->
            fail "The desktop portal returned an invalid PipeWire descriptor."

runGstreamerCapture
    :: Handle
    -> Word32
    -> FilePath
    -> FilePath
    -> IO ()
runGstreamerCapture pipeWireHandle nodeId imagePath logPath = do
    exitResult <-
        bracket
            (openBinaryFile logPath WriteMode)
            hClose
            \logHandle ->
                withCreateProcess
                    ( (proc "gst-launch-1.0"
                        [ "-q"
                        , "pipewiresrc"
                        , "fd=0"
                        , "path=" <> show nodeId
                        , "num-buffers=1"
                        , "!"
                        , "videoconvert"
                        , "!"
                        , "pngenc"
                        , "snapshot=true"
                        , "!"
                        , "filesink"
                        , "location=" <> imagePath
                        ])
                        { std_in = UseHandle pipeWireHandle
                        , std_out = NoStream
                        , std_err = UseHandle logHandle
                        , close_fds = True
                        , create_group = True
                        }
                    )
                    \_ _ _ processHandle -> do
                        waited <-
                            timeout captureTimeout
                                (waitForProcess processHandle)
                        case waited of
                            Just exitCode -> pure (Just exitCode)
                            Nothing -> do
                                stopPortalCaptureProcess processHandle
                                pure Nothing
    details <-
        Text.strip
            . TextEncoding.decodeUtf8With (\_ _ -> Just '\xfffd')
            . BS.take 8192
            <$> BS.readFile logPath
    case exitResult of
        Nothing -> fail "GStreamer portal capture timed out."
        Just ExitSuccess -> pure ()
        Just (ExitFailure code)
            | Text.null details ->
                fail
                    ("GStreamer portal capture failed with exit code "
                        <> show code
                        <> ".")
            | otherwise ->
                fail
                    ("GStreamer portal capture failed: "
                        <> Text.unpack details)

stopPortalCaptureProcess :: ProcessHandle -> IO ()
stopPortalCaptureProcess processHandle = do
    void (tryAny (interruptProcessGroupOf processHandle))
    void (tryAny (terminateProcess processHandle))
    timeout closeCallTimeout (waitForProcess processHandle) >>= \case
        Just _ -> pure ()
        Nothing -> do
            getPid processHandle >>= mapM_
                (\processGroup ->
                    void
                        (tryAny
                            (signalProcessGroup sigKILL processGroup)))
            void $
                timeout closeCallTimeout
                    (waitForProcess processHandle)

withTemporaryPath :: String -> (FilePath -> IO value) -> IO value
withTemporaryPath template action = do
    temporaryDirectory <- getTemporaryDirectory
    bracket
        (openBinaryTempFile temporaryDirectory template)
        cleanup
        \(path, handle) -> do
            hClose handle
            action path
  where
    cleanup (path, handle) =
        (hClose handle `catchAny` const (pure ()))
            `finally`
                (removeFile path `catchAny` const (pure ()))

encodePortalFrame
    :: ScreenshotEncoding
    -> PortalStream
    -> Image PixelRGB8
    -> ImageAttachment
encodePortalFrame encoding stream image =
    let normalized =
            resizeImage
                stream.portalStreamWidth
                stream.portalStreamHeight
                image
    in case encoding of
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

portalDisplay :: PortalSession -> Int -> Int -> ComputerDisplay
portalDisplay session frameWidth frameHeight =
    let stream = session.portalSessionStream
    in ComputerDisplay
        { computerDisplayId =
            Text.intercalate ":"
                [ "wayland-portal"
                , Text.pack
                    (formatObjectPath session.portalSessionPath)
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
    :: InterfaceName
    -> MemberName
    -> [Variant]
    -> MethodCall
portalMethodCall =
    portalObjectCall portalDesktopPath

portalObjectCall
    :: ObjectPath
    -> InterfaceName
    -> MemberName
    -> [Variant]
    -> MethodCall
portalObjectCall path interface member body =
    (methodCall path interface member)
        { methodCallDestination = Just portalDestination
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

requestTimeout, captureTimeout, directCallTimeout, closeCallTimeout :: Int
requestTimeout = 120000000
captureTimeout = 20000000
directCallTimeout = 10000000
closeCallTimeout = 2000000

maximumFrameDimension :: Int
maximumFrameDimension = 32768

maximumFramePixels :: Integer
maximumFramePixels = 100000000
