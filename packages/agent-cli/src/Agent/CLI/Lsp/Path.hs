module Agent.CLI.Lsp.Path
    ( exceptionText
    , pathWithin
    , quote
    , resolveWorkspace
    , sanitizeName
    ) where

import Control.Exception.Safe
    ( SomeException
    , displayException
    , tryAny
    )
import Control.Monad (unless)
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (canonicalizePath, doesDirectoryExist)
import qualified System.FilePath as FilePath

resolveWorkspace
    :: FilePath
    -> Maybe Text
    -> IO (Either Text FilePath)
resolveWorkspace workspace override = do
    let requested = case override of
            Nothing -> workspace
            Just value
                | FilePath.isAbsolute (Text.unpack value) ->
                    Text.unpack value
                | otherwise ->
                    workspace FilePath.</> Text.unpack value
    resolved <-
        tryAny do
            exists <- doesDirectoryExist requested
            unless exists $
                ioError (userError "workspace folder does not exist")
            canonicalizePath requested
    canonicalWorkspace <- tryAny (canonicalizePath workspace)
    pure case (canonicalWorkspace, resolved) of
        (Left exception, _) ->
            Left
                ("failed to resolve active workspace: "
                    <> exceptionText exception)
        (_, Left exception) ->
            Left ("invalid workspaceFolder: " <> exceptionText exception)
        (Right root, Right child)
            | pathWithin root child -> Right child
            | otherwise ->
                Left "workspaceFolder must be inside the active workspace"

pathWithin :: FilePath -> FilePath -> Bool
pathWithin root candidate =
    let relative =
            FilePath.normalise
                (FilePath.makeRelative
                    (FilePath.normalise root)
                    (FilePath.normalise candidate))
    in relative == "."
        || (not (FilePath.isAbsolute relative)
            && relative /= ".."
            && not
                ( (".." <> [FilePath.pathSeparator])
                    `isPrefixOfString` relative
                ))

sanitizeName :: Text -> FilePath
sanitizeName =
    Text.unpack
        . Text.map
            (\character ->
                if isAlphaNum character || character `elem` ("-_" :: String)
                    then character
                    else '_')

exceptionText :: SomeException -> Text
exceptionText = Text.pack . displayException

quote :: Text -> Text
quote value = "'" <> value <> "'"

isPrefixOfString :: String -> String -> Bool
isPrefixOfString prefix value =
    take (length prefix) value == prefix
