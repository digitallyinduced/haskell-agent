{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Export the visible session transcript as Markdown.
module Agent.CLI.TranscriptExport
    ( defaultExportFileName
    , renderTranscriptMarkdown
    , resolveExportPath
    , saveTranscriptNoClobber
    , visibleSessionTurns
    ) where

import Agent.CLI.Session
    ( SessionTurn(..)
    )
import Agent.CLI.Session.Types (TranscriptEffect(..))
import Agent.CLI.TUI.History (HistoryTurn(historyTurnBlocks))
import Agent.CLI.TUI.SessionHistory (sessionHistoryTurn)
import Agent.OsPath (toText, unsafeToFilePath)
import Agent.TUI.Model
    ( BlockKind(..)
    , UiBlock(..)
    )
import Control.Exception.Safe
    ( bracketOnError
    , displayException
    , tryAny
    )
import Data.Foldable (toList)
import Data.List (findIndex)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory.OsPath
    ( doesDirectoryExist
    , getHomeDirectory
    , makeAbsolute
    , removeFile
    )
import System.OsPath
    ( OsPath
    , isAbsolute
    , takeDirectory
    , takeFileName
    , unsafeEncodeUtf
    , (</>)
    )
import System.IO
    ( hClose
    , openBinaryTempFile
    )
import System.Posix.Files (createLink)

-- | Keep only the current visible generation. A reset marker itself remains
-- visible, matching the fullscreen history projection.
visibleSessionTurns :: [SessionTurn] -> [SessionTurn]
visibleSessionTurns turns =
    case findIndex ((== TranscriptReset) . (.turnEffect)) (reverse turns) of
        Nothing -> turns
        Just fromEnd -> drop (length turns - fromEnd - 1) turns

-- | Render the visible transcript using the same durable-to-UI projection as
-- the fullscreen history. Scratchpad reasoning is omitted by that projection.
renderTranscriptMarkdown :: [SessionTurn] -> Text
renderTranscriptMarkdown turns =
    let blocks =
            concat
                [ toList (historyTurnBlocks (sessionHistoryTurn index turn))
                | (index, turn) <- zip [0 :: Int ..] (visibleSessionTurns turns)
                ]
        sections = map renderBlock (filter useful blocks)
    in "# Agent conversation\n\n"
        <> Text.intercalate "\n\n" sections
  where
    useful block =
        not
            (Text.null (Text.strip block.blockBody)
                && Text.null (Text.strip block.blockTitle)
                && Text.null (Text.strip block.blockDetail))

renderBlock :: UiBlock -> Text
renderBlock block =
    let (heading, body) =
            case block.blockKind of
                BlockUser -> ("User", block.blockBody)
                BlockAssistant -> ("Assistant", block.blockBody)
                BlockThinking -> ("Reasoning", block.blockBody)
                BlockSystem -> ("System", block.blockBody)
                BlockRecap -> ("Recap", block.blockBody)
                BlockError -> ("Error", block.blockBody)
                _ -> ("Activity", activityBody block)
    in "## " <> heading <> "\n\n" <> body

activityBody :: UiBlock -> Text
activityBody block =
    Text.intercalate
        "\n\n"
        (filter (not . Text.null . Text.strip)
            [block.blockTitle, block.blockBody, block.blockDetail])

defaultExportFileName :: Text -> Text
defaultExportFileName sessionId =
    "agent-session-" <> sessionId <> ".md"

-- | Resolve an export argument relative to cwd, expanding @~/@ against HOME.
resolveExportPath :: OsPath -> Text -> IO (Either Text OsPath)
resolveExportPath cwd raw =
    tryAny (do
        home <- getHomeDirectory
        let stripped = Text.strip raw
            path
                | stripped == "~" = home
                | Just suffix <- Text.stripPrefix "~/" stripped =
                    home </> unsafeEncodeUtf (Text.unpack suffix)
                | otherwise = unsafeEncodeUtf (Text.unpack stripped)
        makeAbsolute
            (if isAbsolute path then path else cwd </> path))
    >>= pure . either (Left . Text.pack . displayException) Right

-- | Save without replacing an existing file. A same-directory temporary file
-- is hard-linked into place, making target creation atomic and no-clobber.
saveTranscriptNoClobber :: OsPath -> Text -> IO (Either Text ())
saveTranscriptNoClobber target markdown = do
    parentExists <- doesDirectoryExist (takeDirectory target)
    if not parentExists
        then pure (Left "export directory does not exist")
        else do
            result <- tryAny $
                bracketOnError
                    (openBinaryTempFile
                        (unsafeToFilePath (takeDirectory target))
                        (unsafeToFilePath (takeFileName target) <> ".tmp"))
                    (\(tmp, handle) -> do
                        _ <- tryAny (hClose handle)
                        _ <- tryAny (removeFile (unsafeEncodeUtf tmp))
                        pure ())
                    \(tmp, handle) -> do
                        Text.hPutStr handle markdown
                        hClose handle
                        createLink tmp (unsafeToFilePath target)
                        removeFile (unsafeEncodeUtf tmp)
            pure $ case result of
                Left err -> Left (Text.pack (displayException err))
                Right () -> Right ()

