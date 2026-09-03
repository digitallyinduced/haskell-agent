-- | Shared dependencies for REPL command-domain handlers.
module Agent.CLI.Runtime.Repl.Context
    ( ReplHandlerContext(..)
    , continueRepl
    , displayReplError
    , displayReplInfo
    , requestReplChoice
    , requestReplText
    , withReplSuspended
    ) where

import Agent.CLI.Input ( readChoiceSelectionAt, readModalText )
import Agent.CLI.Runtime.Types ( RunResult )
import Agent.CLI.SessionEnv ( SessionEnv(..) )
import Agent.CLI.Style ( roleMuted, roleSuccess )
import Agent.CLI.Terminal ( resolveColor )
import Agent.CLI.TUI.App
    ( emitUiEvent
    , requestFullscreenChoiceWithBody
    , requestFullscreenText
    , withFullscreenSuspended
    )
import Agent.TUI.Model
    ( UiEvent(UiErrorMessage, UiSystemMessage) )
import Control.Monad ( unless )
import Data.List ( elemIndex )
import Data.Text ( Text )
import qualified Data.Text as Text
    ( intercalate, null, strip )
import qualified Data.Text.IO as Text ( hPutStrLn )
import System.IO ( stderr )

data ReplHandlerContext = ReplHandlerContext
    { handlerSessionEnv :: !SessionEnv
    , handlerContinueWith :: Text -> IO RunResult
    , handlerStdoutColor :: !Bool
    }

continueRepl :: ReplHandlerContext -> IO RunResult
continueRepl context = context.handlerContinueWith ""

displayReplInfo :: ReplHandlerContext -> Text -> IO () -> IO ()
displayReplInfo context message minimalAction =
    case context.handlerSessionEnv.sessionFullscreen of
        Nothing -> minimalAction
        Just runtime ->
            emitUiEvent runtime (UiSystemMessage message)

displayReplError :: ReplHandlerContext -> Text -> IO () -> IO ()
displayReplError context message minimalAction =
    case context.handlerSessionEnv.sessionFullscreen of
        Nothing -> minimalAction
        Just runtime ->
            emitUiEvent runtime (UiErrorMessage message)

requestReplChoice
    :: ReplHandlerContext
    -> Text
    -> Text
    -> Int
    -> [(Text, Text)]
    -> IO (Maybe Int)
requestReplChoice context title body initial rows
    | null rows = pure Nothing
    | otherwise =
        case context.handlerSessionEnv.sessionFullscreen of
            Just runtime ->
                requestFullscreenChoiceWithBody
                    runtime
                    title
                    body
                    (max 0 (min (length rows - 1) initial))
                    rows
            Nothing -> do
                color <- resolveColor stderr
                Text.hPutStrLn stderr
                    (roleMuted color
                        (Text.intercalate
                            "\n"
                            (filter
                                (not . Text.null)
                                [title, body])))
                let labels =
                        [ if Text.null detail
                            then label
                            else label <> " — " <> detail
                        | (label, detail) <- rows
                        ]
                selected <-
                    readChoiceSelectionAt initial
                        (\active label ->
                            if active
                                then roleSuccess color label
                                else roleMuted color label)
                        labels
                pure (selected >>= (`elemIndex` labels))

requestReplText
    :: ReplHandlerContext
    -> Text
    -> Text
    -> Text
    -> IO (Maybe Text)
requestReplText context title body initial =
    case context.handlerSessionEnv.sessionFullscreen of
        Just runtime ->
            requestFullscreenText runtime title body initial
        Nothing -> do
            color <- resolveColor stderr
            unless (Text.null (Text.strip body)) $
                Text.hPutStrLn stderr (roleMuted color body)
            readModalText
                context.handlerSessionEnv.sessionInterrupt
                (title <> ": ")
                initial

withReplSuspended :: ReplHandlerContext -> IO a -> IO a
withReplSuspended context action =
    case context.handlerSessionEnv.sessionFullscreen of
        Nothing -> action
        Just runtime -> withFullscreenSuspended runtime action
