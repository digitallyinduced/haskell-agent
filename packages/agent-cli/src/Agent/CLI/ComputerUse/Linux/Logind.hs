module Agent.CLI.ComputerUse.Linux.Logind
    ( LogindGuard(..)
    , LogindSessionTarget(..)
    , WaylandPortalTarget(..)
    , newLogindGuard
    , processSessionRequest
    , validateLogindSession
    , validateLogindState
    , validateWaylandPortalSessions
    , waylandPortalTarget
    , withLogindReadiness
    ) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar_
    , newMVar
    , withMVar
    )
import Control.Concurrent.Async (race)
import Control.Exception.Safe (catchAny, onException, tryAny)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32)
import DBus
    ( Address
    , IsVariant
    , MemberName
    , MethodCall(..)
    , MethodError
    , ObjectPath
    , Variant
    , address
    , fromVariant
    , methodCall
    , methodReturnBody
    , toVariant
    )
import DBus.Client
    ( Client
    , call
    , connectSystem
    , disconnect
    )
import qualified System.FilePath.Posix as Posix
import System.Posix.Process (getProcessID)
import System.Posix.Types (ProcessID)
import System.Posix.User (getEffectiveUserID)
import System.Timeout (timeout)
import Text.Read (readMaybe)

data LogindGuard = LogindGuard
    { checkLogindGuard :: IO (Either Text ())
    , closeLogindGuard :: IO ()
    , logindWaylandPortalTarget :: !(Maybe WaylandPortalTarget)
    }

data LogindSessionTarget
    = LogindX11Session !Text
    | LogindWaylandSession
    deriving (Eq, Show)

data WaylandPortalTarget = WaylandPortalTarget
    { waylandPortalAddress :: !Address
    , waylandPortalUserId :: !Word32
    , waylandPortalUserPath :: !ObjectPath
    , waylandPortalSessionPath :: !ObjectPath
    , waylandPortalDisplayId :: !Text
    , waylandPortalRuntimePath :: !Text
    } deriving (Eq, Show)

newLogindGuard :: LogindSessionTarget -> IO (Either Text LogindGuard)
newLogindGuard target = do
    attempted <- tryAny $
        timeout logindCallTimeout connectAndResolve >>= \case
            Nothing ->
                fail "systemd-logind connection or session lookup timed out"
            Just guard -> pure guard
    pure case attempted of
        Left exception ->
            Left
                ( "Linux computer use cannot verify the graphical session "
                    <> "through systemd-logind: "
                    <> Text.pack (show exception)
                )
        Right guard -> Right guard
  where
    connectAndResolve = do
        client <- connectSystem
        flip onException
            (disconnect client `catchAny` const (pure ())) do
                sessionPath <- resolveSessionPath client
                portalTarget <- case target of
                    LogindX11Session _ -> pure Nothing
                    LogindWaylandSession -> do
                        resolved <- resolveWaylandPortalTarget client sessionPath
                        Just <$> either (fail . Text.unpack) pure resolved
                state <- newMVar (Just client)
                pure LogindGuard
                    { checkLogindGuard =
                        checkGuard target portalTarget state sessionPath
                    , closeLogindGuard = closeGuard state
                    , logindWaylandPortalTarget = portalTarget
                    }

resolveSessionPath :: Client -> IO ObjectPath
resolveSessionPath client = do
    pid <- getProcessID
    let (member, body) = processSessionRequest pid
    callForPath client member body

processSessionRequest :: ProcessID -> (MemberName, [Variant])
processSessionRequest pid =
    ( "GetSessionByPID"
    , [toVariant (fromIntegral pid :: Word32)]
    )

callForPath :: Client -> MemberName -> [Variant] -> IO ObjectPath
callForPath client member body = do
    result <- call client
        ( (methodCall
            "/org/freedesktop/login1"
            "org.freedesktop.login1.Manager"
            member
          )
            { methodCallDestination = Just "org.freedesktop.login1"
            , methodCallBody = body
            }
        )
    reply <- either throwMethodError pure result
    case methodReturnBody reply of
        [value]
            | Just path <- fromVariant value ->
                pure path
        _ ->
            fail "systemd-logind returned an invalid session path"

checkGuard
    :: LogindSessionTarget
    -> Maybe WaylandPortalTarget
    -> MVar (Maybe Client)
    -> ObjectPath
    -> IO (Either Text ())
checkGuard target portalTarget state sessionPath =
    withMVar state \case
        Nothing ->
            pure (Left "The Linux computer-use session has been closed.")
        Just client -> do
            attempted <- tryAny $
                timeout logindCallTimeout
                    (do
                        currentSessionPath <- resolveSessionPath client
                        active <-
                            getSessionProperty client sessionPath "Active"
                        locked <-
                            getSessionProperty
                                client sessionPath "LockedHint"
                        sessionType <-
                            getSessionProperty client sessionPath "Type"
                        sessionDisplay <- case target of
                            LogindX11Session _ ->
                                Just <$> getSessionProperty
                                    client sessionPath "Display"
                            LogindWaylandSession -> pure Nothing
                        portalBinding <- case (target, portalTarget) of
                            (LogindWaylandSession, Just expected) ->
                                checkWaylandPortalTarget client expected
                            (LogindWaylandSession, Nothing) ->
                                pure
                                    (Left
                                        "Computer use cannot associate the Wayland portal with the current systemd-logind session.")
                            (LogindX11Session _, _) -> pure (Right ())
                        finalSessionPath <- resolveSessionPath client
                        pure do
                            if currentSessionPath == sessionPath
                                && finalSessionPath == sessionPath
                                then Right ()
                                else Left
                                    "Computer use cannot verify that the process remains in the selected systemd-logind session."
                            validateLogindSession
                                target
                                active
                                locked
                                sessionType
                                sessionDisplay
                            portalBinding)
                    >>= \case
                        Nothing ->
                            fail
                                "systemd-logind session verification timed out"
                        Just result -> pure result
            pure case attempted of
                Left exception ->
                    Left
                        ( "Linux graphical session verification failed: "
                            <> Text.pack (show exception)
                        )
                Right result -> result

resolveWaylandPortalTarget
    :: Client
    -> ObjectPath
    -> IO (Either Text WaylandPortalTarget)
resolveWaylandPortalTarget client sessionPath = do
    effectiveUserId <- fromIntegral <$> getEffectiveUserID
    sessionUser <-
        getSessionProperty client sessionPath "User"
            :: IO (Word32, ObjectPath)
    let (_, userPath) = sessionUser
    primaryDisplay <-
        getUserProperty client userPath "Display"
            :: IO (Text, ObjectPath)
    runtimePath <-
        getUserProperty client userPath "RuntimePath"
            :: IO Text
    userSessions <- getStableUserSessions client userPath
    pure
        (waylandPortalTarget
            sessionPath
            effectiveUserId
            sessionUser
            primaryDisplay
            runtimePath
            userSessions)

checkWaylandPortalTarget
    :: Client
    -> WaylandPortalTarget
    -> IO (Either Text ())
checkWaylandPortalTarget client expected = do
    effectiveUserId <- fromIntegral <$> getEffectiveUserID
    sessionUser <-
        getSessionProperty
            client
            expected.waylandPortalSessionPath
            "User"
            :: IO (Word32, ObjectPath)
    primaryDisplay <-
        getUserProperty
            client
            expected.waylandPortalUserPath
            "Display"
            :: IO (Text, ObjectPath)
    runtimePath <-
        getUserProperty
            client
            expected.waylandPortalUserPath
            "RuntimePath"
            :: IO Text
    userSessions <-
        getStableUserSessions
            client
            expected.waylandPortalUserPath
    pure
        (validateWaylandPortalIdentity
            expected.waylandPortalSessionPath
            expected.waylandPortalUserId
            expected.waylandPortalUserPath
            expected.waylandPortalDisplayId
            expected.waylandPortalRuntimePath
            effectiveUserId
            sessionUser
            primaryDisplay
            runtimePath
            userSessions)

waylandPortalTarget
    :: ObjectPath
    -> Word32
    -> (Word32, ObjectPath)
    -> (Text, ObjectPath)
    -> Text
    -> [(Text, ObjectPath, Text)]
    -> Either Text WaylandPortalTarget
waylandPortalTarget
    sessionPath
    effectiveUserId
    (sessionUserId, userPath)
    (primaryDisplayId, primaryDisplayPath)
    runtimePath
    userSessions
        | Left err <-
            validateWaylandPortalIdentity
                sessionPath
                sessionUserId
                userPath
                primaryDisplayId
                runtimePath
                effectiveUserId
                (sessionUserId, userPath)
                (primaryDisplayId, primaryDisplayPath)
                runtimePath
                userSessions = Left err
        | otherwise =
            case address
                "unix"
                (Map.singleton
                    "path"
                    (Text.unpack runtimePath Posix.</> "bus")) of
                Nothing -> Left associationError
                Just busAddress ->
                    Right WaylandPortalTarget
                        { waylandPortalAddress = busAddress
                        , waylandPortalUserId = sessionUserId
                        , waylandPortalUserPath = userPath
                        , waylandPortalSessionPath = sessionPath
                        , waylandPortalDisplayId = primaryDisplayId
                        , waylandPortalRuntimePath = runtimePath
                        }
  where
    associationError = waylandAssociationError

validateWaylandPortalIdentity
    :: ObjectPath
    -> Word32
    -> ObjectPath
    -> Text
    -> Text
    -> Word32
    -> (Word32, ObjectPath)
    -> (Text, ObjectPath)
    -> Text
    -> [(Text, ObjectPath, Text)]
    -> Either Text ()
validateWaylandPortalIdentity
    expectedSessionPath
    expectedUserId
    expectedUserPath
    expectedDisplayId
    expectedRuntimePath
    effectiveUserId
    (sessionUserId, userPath)
    (primaryDisplayId, primaryDisplayPath)
    runtimePath
    userSessions
        | effectiveUserId /= expectedUserId = Left associationError
        | sessionUserId /= expectedUserId = Left associationError
        | userPath /= expectedUserPath = Left associationError
        | Text.null expectedDisplayId = Left associationError
        | primaryDisplayId /= expectedDisplayId = Left associationError
        | primaryDisplayPath /= expectedSessionPath = Left associationError
        | runtimePath /= expectedRuntimePath = Left associationError
        | Text.null runtimePath = Left associationError
        | Text.any (== '\NUL') runtimePath = Left associationError
        | not (Posix.isAbsolute (Text.unpack runtimePath)) =
            Left associationError
        | otherwise =
            validateWaylandPortalSessions
                (expectedDisplayId, expectedSessionPath)
                userSessions
  where
    associationError = waylandAssociationError

validateWaylandPortalSessions
    :: (Text, ObjectPath)
    -> [(Text, ObjectPath, Text)]
    -> Either Text ()
validateWaylandPortalSessions expectedDisplay sessions =
    case filter matchesExpected sessions of
        [(sessionId, sessionPath, sessionType)]
            | (sessionId, sessionPath) == expectedDisplay
            , sessionType == "wayland"
            , all allowedSession sessions ->
                Right ()
        _ -> Left waylandAssociationError
  where
    matchesExpected (sessionId, sessionPath, _) =
        sessionId == fst expectedDisplay
            || sessionPath == snd expectedDisplay
    allowedSession session@(_, _, sessionType) =
        matchesExpected session || sessionType == "tty"

getStableUserSessions
    :: Client
    -> ObjectPath
    -> IO [(Text, ObjectPath, Text)]
getStableUserSessions client userPath = do
    before <-
        getUserProperty client userPath "Sessions"
            :: IO [(Text, ObjectPath)]
    typed <- traverse addSessionType before
    after <-
        getUserProperty client userPath "Sessions"
            :: IO [(Text, ObjectPath)]
    if before == after
        then pure typed
        else fail (Text.unpack waylandAssociationError)
  where
    addSessionType (sessionId, sessionPath) = do
        sessionType <-
            getSessionProperty client sessionPath "Type"
                :: IO Text
        pure (sessionId, sessionPath, sessionType)

waylandAssociationError :: Text
waylandAssociationError =
    "Computer use cannot associate the Wayland portal with the current systemd-logind session."

getSessionProperty
    :: IsVariant value
    => Client
    -> ObjectPath
    -> Text
    -> IO value
getSessionProperty client path =
    getLoginProperty
        client
        path
        "org.freedesktop.login1.Session"

getUserProperty
    :: IsVariant value
    => Client
    -> ObjectPath
    -> Text
    -> IO value
getUserProperty client path =
    getLoginProperty
        client
        path
        "org.freedesktop.login1.User"

getLoginProperty
    :: IsVariant value
    => Client
    -> ObjectPath
    -> Text
    -> Text
    -> IO value
getLoginProperty client path interfaceName propertyName = do
    result <- call client
        ( (methodCall
            path
            "org.freedesktop.DBus.Properties"
            "Get"
          )
            { methodCallDestination = Just "org.freedesktop.login1"
            , methodCallBody =
                [ toVariant
                    interfaceName
                , toVariant propertyName
                ]
            }
        )
    reply <- either throwMethodError pure result
    case methodReturnBody reply of
        [outer]
            | Just inner <- fromVariant outer :: Maybe Variant
            , Just value <- fromVariant inner ->
                pure value
        _ ->
            fail
                ( "systemd-logind returned an invalid "
                    <> Text.unpack propertyName
                    <> " property"
                )

validateLogindState :: Bool -> Bool -> Either Text ()
validateLogindState active locked
    | not active =
        Left "Computer use is unavailable while the Linux session is inactive."
    | locked =
        Left "Computer use is unavailable while the Linux session is locked."
    | otherwise = Right ()

validateLogindSession
    :: LogindSessionTarget
    -> Bool
    -> Bool
    -> Text
    -> Maybe Text
    -> Either Text ()
validateLogindSession target active locked sessionType sessionDisplay = do
    validateLogindState active locked
    case target of
        LogindWaylandSession
            | sessionType == "wayland" -> Right ()
            | otherwise ->
                Left
                    "Computer use cannot verify that the current systemd-logind session is Wayland."
        LogindX11Session expectedDisplay
            | sessionType /= "x11" ->
                Left
                    "Computer use cannot verify that the current systemd-logind session is X11."
            | Just actualDisplay <- sessionDisplay
            , sameLocalX11Server expectedDisplay actualDisplay ->
                Right ()
            | otherwise ->
                Left
                    "Computer use cannot associate DISPLAY with the current systemd-logind X11 session."

sameLocalX11Server :: Text -> Text -> Bool
sameLocalX11Server first second =
    case (localX11ServerNumber first, localX11ServerNumber second) of
        (Just firstNumber, Just secondNumber) ->
            firstNumber == secondNumber
        _ -> False

localX11ServerNumber :: Text -> Maybe Integer
localX11ServerNumber display = do
    suffix <-
        Text.stripPrefix "unix/:" display
            <|> Text.stripPrefix "unix:" display
            <|> Text.stripPrefix ":" display
    let (displayNumber, screenSuffix) = Text.breakOn "." suffix
    if not (asciiDigits displayNumber) || not (validScreen screenSuffix)
        then Nothing
        else readMaybe (Text.unpack displayNumber)
  where
    asciiDigits value =
        not (Text.null value)
            && Text.all (\character ->
                character >= '0' && character <= '9') value
    validScreen value
        | Text.null value = True
        | Just screen <- Text.stripPrefix "." value =
            asciiDigits screen
        | otherwise = False

withLogindReadiness
    :: IO (Either Text ())
    -> IO (Either Text value)
    -> IO (Either Text value)
withLogindReadiness readiness action =
    readiness >>= \case
        Left err -> pure (Left err)
        Right () ->
            race action (waitForReadinessFailure readiness) >>= \case
                Left (Left err) -> pure (Left err)
                Left (Right value) ->
                    readiness >>= \case
                        Left err -> pure (Left err)
                        Right () -> pure (Right value)
                Right err -> pure (Left err)

waitForReadinessFailure
    :: IO (Either Text ())
    -> IO Text
waitForReadinessFailure readiness = do
    threadDelay readinessPollDelay
    timeout readinessPollDelay readiness >>= \case
        Nothing ->
            pure "Linux graphical session readiness verification timed out."
        Just (Left err) -> pure err
        Just (Right ()) -> waitForReadinessFailure readiness

closeGuard :: MVar (Maybe Client) -> IO ()
closeGuard state =
    modifyMVar_ state \case
        Nothing -> pure Nothing
        Just client -> do
            disconnect client `catchAny` const (pure ())
            pure Nothing

throwMethodError :: MethodError -> IO value
throwMethodError methodError =
    fail ("D-Bus method failed: " <> show methodError)

logindCallTimeout :: Int
logindCallTimeout = 5000000

readinessPollDelay :: Int
readinessPollDelay = 50000
