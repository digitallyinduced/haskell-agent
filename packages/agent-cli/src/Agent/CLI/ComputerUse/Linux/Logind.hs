module Agent.CLI.ComputerUse.Linux.Logind
    ( LogindGuard(..)
    , LogindSessionTarget(..)
    , newLogindGuard
    , processSessionRequest
    , validateLogindSession
    , validateLogindState
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
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32)
import DBus
    ( IsVariant
    , MemberName
    , MethodCall(..)
    , MethodError
    , ObjectPath
    , Variant
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
import System.Posix.Process (getProcessID)
import System.Posix.Types (ProcessID)
import System.Timeout (timeout)
import Text.Read (readMaybe)

data LogindGuard = LogindGuard
    { checkLogindGuard :: IO (Either Text ())
    , closeLogindGuard :: IO ()
    }

data LogindSessionTarget
    = LogindX11Session !Text
    | LogindWaylandSession
    deriving (Eq, Show)

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
                state <- newMVar (Just client)
                pure LogindGuard
                    { checkLogindGuard =
                        checkGuard target state sessionPath
                    , closeLogindGuard = closeGuard state
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
    -> MVar (Maybe Client)
    -> ObjectPath
    -> IO (Either Text ())
checkGuard target state sessionPath =
    withMVar state \case
        Nothing ->
            pure (Left "The Linux computer-use session has been closed.")
        Just client -> do
            attempted <- tryAny $
                timeout logindCallTimeout
                    (do
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
                        pure
                            (validateLogindSession
                                target
                                active
                                locked
                                sessionType
                                sessionDisplay))
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

getSessionProperty
    :: IsVariant value
    => Client
    -> ObjectPath
    -> Text
    -> IO value
getSessionProperty client sessionPath propertyName = do
    result <- call client
        ( (methodCall
            sessionPath
            "org.freedesktop.DBus.Properties"
            "Get"
          )
            { methodCallDestination = Just "org.freedesktop.login1"
            , methodCallBody =
                [ toVariant
                    ("org.freedesktop.login1.Session" :: Text)
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
    readiness >>= \case
        Left err -> pure err
        Right () -> waitForReadinessFailure readiness

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
