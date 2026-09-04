module Agent.CLI.ComputerUse.Linux
    ( LinuxSessionType(..)
    , detectLinuxSessionType
    , newLinuxBackend
    ) where

import Agent.CLI.ComputerUse.Backend (ComputerBackend(..))
import Agent.CLI.ComputerUse.Linux.Logind
    ( LogindGuard(..)
    , LogindSessionTarget(..)
    , newLogindGuard
    )
import Agent.CLI.ComputerUse.Linux.Portal (newPortalBackend)
import Agent.CLI.ComputerUse.Linux.X11 (newX11Backend)
import Control.Exception.Safe (finally, onException)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Environment (lookupEnv)

data LinuxSessionType
    = LinuxX11
    | LinuxWayland
    deriving (Eq, Show)

detectLinuxSessionType
    :: Maybe String
    -> Maybe String
    -> Maybe String
    -> Either Text LinuxSessionType
detectLinuxSessionType sessionType waylandDisplay xDisplay =
    case fmap (Text.toLower . Text.strip . Text.pack) sessionType of
        Just "wayland" -> Right LinuxWayland
        Just "x11" -> requireDisplay LinuxX11 xDisplay
        Just value
            | not (Text.null value) ->
                Left ("Unsupported Linux graphical session type: " <> value)
        _
            | present waylandDisplay -> Right LinuxWayland
            | otherwise -> requireDisplay LinuxX11 xDisplay
  where
    present = maybe False (not . null)
    requireDisplay result value
        | present value = Right result
        | otherwise =
            Left
                "Linux computer use requires an active X11 or Wayland graphical session."

newLinuxBackend :: IO (Either Text ComputerBackend)
newLinuxBackend = do
    sessionType <- lookupEnv "XDG_SESSION_TYPE"
    waylandDisplay <- lookupEnv "WAYLAND_DISPLAY"
    xDisplay <- lookupEnv "DISPLAY"
    case detectLinuxSessionType sessionType waylandDisplay xDisplay of
        Left err -> pure (Left err)
        Right graphicalSession ->
            case logindSessionTarget graphicalSession xDisplay of
                Left err -> pure (Left err)
                Right target -> do
                    logindResult <- newLogindGuard target
                    case logindResult of
                        Left err -> pure (Left err)
                        Right logind ->
                            case target of
                                LogindX11Session display -> do
                                    backend <-
                                        newX11Backend
                                            display
                                            logind.checkLogindGuard
                                            `onException`
                                                logind.closeLogindGuard
                                    pure
                                        (Right
                                            (attachLogindGuard logind backend))
                                LogindWaylandSession -> do
                                    portal <-
                                        newPortalBackend
                                            logind.checkLogindGuard
                                            `onException`
                                                logind.closeLogindGuard
                                    case portal of
                                        Left err -> do
                                            logind.closeLogindGuard
                                            pure (Left err)
                                        Right backend ->
                                            pure
                                                (Right
                                                    (attachLogindGuard
                                                        logind
                                                        backend))

logindSessionTarget
    :: LinuxSessionType
    -> Maybe String
    -> Either Text LogindSessionTarget
logindSessionTarget LinuxWayland _ = Right LogindWaylandSession
logindSessionTarget LinuxX11 xDisplay =
    case xDisplay of
        Just display
            | not (null display) ->
                Right (LogindX11Session (Text.pack display))
        _ ->
            Left
                "Linux computer use requires an active X11 or Wayland graphical session."

attachLogindGuard :: LogindGuard -> ComputerBackend -> ComputerBackend
attachLogindGuard logind backend =
    backend
        { computerBackendClose =
            backend.computerBackendClose
                `finally` logind.closeLogindGuard
        }
