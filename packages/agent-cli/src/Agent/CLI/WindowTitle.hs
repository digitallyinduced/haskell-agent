-- | Coordinate the terminal window title with agent turn activity.
module Agent.CLI.WindowTitle
    ( WindowTitleController(..)
    , busyWindowTitle
    , newWindowTitleController
    , oscWindowTitleBytes
    ) where

import Agent.CLI.Style (spinnerFrames)
import Agent.TUI.Motion
    ( MotionDemand(..)
    , MotionMode(..)
    , motionIntervalMicros
    )
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
    ( atomically
    , check
    , modifyTVar'
    , newTVarIO
    , readTVar
    , writeTVar
    )
import Control.Monad (forM_, when)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

data WindowTitleController = WindowTitleController
    { windowTitleSet :: !(Text -> IO ())
    , windowTitleBeginBusy :: !(IO ())
    , windowTitleEndBusy :: !(IO ())
    , windowTitleWorker :: !(IO ())
    }

data WindowTitleState = WindowTitleState
    { titleBase :: !Text
    , titleBusyDepth :: !Int
    }

-- | Build a title controller. The caller supplies a shared output lock and the
-- renderer-specific title writer, then scopes 'windowTitleWorker' with
-- structured concurrency for the session lifetime.
newWindowTitleController
    :: MotionMode
    -> Text
    -> (IO () -> IO ())
    -> (Text -> IO ())
    -> IO WindowTitleController
newWindowTitleController motionMode initialTitle withOutputLock writeTitle = do
    stateVar <- newTVarIO
        WindowTitleState
            { titleBase = initialTitle
            , titleBusyDepth = 0
            }
    let frames = case spinnerFrames of
            [] -> ["*"]
            available -> available
        firstFrame = case frames of
            frame : _ -> frame
            [] -> "*"
        setTitle title =
            withOutputLock do
                state <- atomically do
                    current <- readTVar stateVar
                    let updated = current { titleBase = title }
                    writeTVar stateVar updated
                    pure updated
                writeTitle
                    (if state.titleBusyDepth > 0
                        then busyWindowTitle firstFrame title
                        else title)
        beginBusy =
            withOutputLock do
                state <- atomically do
                    modifyTVar' stateVar \current ->
                        current
                            { titleBusyDepth = current.titleBusyDepth + 1 }
                    readTVar stateVar
                when (state.titleBusyDepth == 1) $
                    writeTitle (busyWindowTitle firstFrame state.titleBase)
        endBusy =
            withOutputLock do
                state <- atomically do
                    modifyTVar' stateVar \current ->
                        current
                            { titleBusyDepth =
                                max 0 (current.titleBusyDepth - 1)
                            }
                    readTVar stateVar
                when (state.titleBusyDepth == 0) $
                    writeTitle state.titleBase
        worker =
            when (motionMode == MotionFull) $
                forM_ (cycle frames) \frame -> do
                    atomically do
                        state <- readTVar stateVar
                        check (state.titleBusyDepth > 0)
                    withOutputLock do
                        state <- atomically (readTVar stateVar)
                        when (state.titleBusyDepth > 0) $
                            writeTitle
                                (busyWindowTitle frame state.titleBase)
                    threadDelay (motionIntervalMicros motionMode MotionSlow)
    pure WindowTitleController
        { windowTitleSet = setTitle
        , windowTitleBeginBusy = beginBusy
        , windowTitleEndBusy = endBusy
        , windowTitleWorker = worker
        }

busyWindowTitle :: Text -> Text -> Text
busyWindowTitle frame title = frame <> " " <> title

-- | OSC 2 window-title bytes, UTF-8 encoded.
-- Vty's 'setOutputWindowTitle' Latin-1 packs the title, which garbles braille
-- spinner frames. Write this payload with 'outputByteBuffer' instead.
oscWindowTitleBytes :: Text -> ByteString
oscWindowTitleBytes title =
    TextEncoding.encodeUtf8
        ("\ESC]2;" <> sanitizeOscWindowTitle title <> "\a")

sanitizeOscWindowTitle :: Text -> Text
sanitizeOscWindowTitle =
    Text.filter \c ->
        c /= '\ESC'
            && c /= '\a'
            && c /= '\n'
            && c /= '\r'
            && c /= '\x9c'
