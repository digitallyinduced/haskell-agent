-- | Translate admitted native turn inputs and scope temporary image files.
module Agent.CLI.MacOS.TurnInputs
    ( nativeTurnArguments
    , withTurnImages
    ) where

import Agent.CLI.MacOS.NativeRequest (TurnStart(..))
import Agent.Runtime.ManagedTurn
    ( ManagedTurnMedia(..)
    , managedTurnRequestWithImages
    , renderManagedTurnPrompt
    )
import Agent.Loop (ImageAttachment(..))
import Control.Exception.Safe (bracket, tryAny)
import Control.Monad (void)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openBinaryTempFile)

nativeTurnArguments :: TurnStart -> [String]
nativeTurnArguments start =
    [ "--minimal"
    , "--motion", "off"
    , "--save-session"
    , "--no-yolo"
    ]
        <> maybe
            []
            (\sessionId -> ["--resume", Text.unpack sessionId])
            start.turnStartSessionId
        <> (if start.turnStartWorktree then ["--worktree"] else [])
        <> (if start.turnStartComputerUse
            then ["--computer-use"]
            else ["--no-computer-use"])
        <> modelArgs
        <> maybe
            []
            (\effort -> ["--effort", Text.unpack effort])
            start.turnStartEffort
  where
    modelArgs = case
        (start.turnStartProvider, start.turnStartModel) of
            (Just provider, Just model) ->
                [ "--provider", Text.unpack provider
                , "--model", Text.unpack model
                ]
            _ -> []

withTurnImages
    :: Text
    -> [ImageAttachment]
    -> (Maybe FilePath -> IO a)
    -> IO a
withTurnImages _ [] action = action Nothing
withTurnImages prompt images action = do
    temporaryDirectory <- getTemporaryDirectory
    withImageFiles temporaryDirectory images \paths -> do
        let request = managedTurnRequestWithImages prompt
                [ ManagedTurnMedia
                    { managedTurnMediaPath = path
                    , managedTurnMediaMime = image.imageMime
                    , managedTurnMediaName = Nothing
                    }
                | (path, image) <- zip paths images
                ]
        bracket
            (openBinaryTempFile temporaryDirectory "ha-native-turn-")
            (\(path, handle) -> do
                hClose handle
                void (tryAny (removeFile path)))
            \(path, handle) -> do
                hClose handle
                BS.writeFile path
                    (TextEncoding.encodeUtf8 (renderManagedTurnPrompt request))
                action (Just path)
  where
    withImageFiles _ [] action = action []
    withImageFiles directory (image : rest) action =
        bracket
            (openBinaryTempFile directory "ha-native-image-")
            (\(path, handle) -> do
                hClose handle
                void (tryAny (removeFile path)))
            \(path, handle) -> do
                hClose handle
                BS.writeFile path image.imageBytes
                withImageFiles directory rest (action . (path :))
