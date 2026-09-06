module Agent.CLI.ComputerUse.Backend
    ( CapturedDisplay(..)
    , ComputerBackend(..)
    , ComputerDisplay(..)
    , ScreenshotEncoding(..)
    , displayLogicalSize
    ) where

import Agent.Loop (ImageAttachment)
import Agent.Responses.Types (ComputerAction)
import Data.Text (Text)

data ScreenshotEncoding
    = ScreenshotPng
    | ScreenshotJpeg
    deriving (Eq, Show)

-- | A complete identity for the coordinate space exposed to the model.
-- Backends include any physical frame dimensions needed to detect a
-- mid-call scale or mode change.
data ComputerDisplay = ComputerDisplay
    { computerDisplayId :: !Text
    , computerDisplayOriginX :: !Int
    , computerDisplayOriginY :: !Int
    , computerDisplayWidth :: !Int
    , computerDisplayHeight :: !Int
    , computerDisplayFrameWidth :: !Int
    , computerDisplayFrameHeight :: !Int
    } deriving (Eq, Show)

data CapturedDisplay = CapturedDisplay
    { capturedComputerDisplay :: !ComputerDisplay
    , capturedComputerImage :: !ImageAttachment
    }

data ComputerBackend = ComputerBackend
    { computerBackendEnsureReady :: !(IO (Either Text ()))
    , computerBackendInspectDisplay :: !(IO (Either Text ComputerDisplay))
    , computerBackendExecuteAction
        :: !(ComputerDisplay -> ComputerAction -> IO (Either Text ()))
    , computerBackendCaptureDisplay
        :: !(ScreenshotEncoding -> IO (Either Text CapturedDisplay))
    , computerBackendClose :: !(IO ())
    }

displayLogicalSize :: ComputerDisplay -> (Int, Int)
displayLogicalSize display =
    (display.computerDisplayWidth, display.computerDisplayHeight)
