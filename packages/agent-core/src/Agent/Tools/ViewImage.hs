-- | Model-facing local image inspection.
--
-- Unlike 'Agent.Tools.ShowImage', this tool puts the selected image in the
-- next provider request so a vision-capable model can inspect it.
module Agent.Tools.ViewImage
    ( viewImageTool
    , viewImageToolName
    ) where

import Agent.Image.File (readImageFileResult)
import Agent.Json.Decode (Decoder)
import Agent.OsPath (fromText)
import Agent.ToolArgs (objectArgs, optText, reqText)
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolHandlerResult
    , decodeToolArguments
    , toolArgumentsValue
    , typedRichToolWithCall
    )
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.Tools.FileSystem (displayPathInWorkspace, resolveForRead)
import Agent.Tools.Scheduling
    ( ToolAccess(..)
    , ToolResource(..)
    , ToolResourceClaim(..)
    )
import Agent.Tools.Types
    ( AppTool
    , ToolEnv
    , ToolExecutionPolicy(..)
    , jsonTool
    , withSharedToolResourceClaims
    )
import Data.Text (Text)
import System.Directory.OsPath (doesFileExist)

viewImageToolName :: Text
viewImageToolName = "view_image"

data ViewImageArgs = ViewImageArgs
    { path :: !Text
    , detail :: !(Maybe Text)
    }

viewImageArgsDecoder :: Decoder ViewImageArgs
viewImageArgsDecoder = objectArgs \object ->
    ViewImageArgs
        <$> reqText object "path"
        <*> optText object "detail"

viewImageTool :: ToolEnv -> AppTool
viewImageTool env =
    withSharedToolResourceClaims env (viewImageClaims env) $
        jsonTool viewImageToolName viewImageDescription
            [ PropertySchema "path" PropertyString True $ Just
                "Local filesystem path to an image file."
            ]
            True
            ParallelSafe
            (typedRichToolWithCall viewImageToolName viewImageArgsDecoder
                (runViewImage env))

viewImageDescription :: Text
viewImageDescription =
    "View a local image file from the filesystem when visual inspection is needed. \
    \Use this for images already available on disk. The image is added to your context."

viewImageClaims
    :: ToolEnv
    -> ToolCall
    -> IO (Either Text [ToolResourceClaim])
viewImageClaims env call =
    case decodeToolArguments viewImageArgsDecoder (toolArgumentsValue call.arguments) of
        Left err -> pure (Left err)
        Right args ->
            resolveForRead env (fromText args.path)
                >>= pure . fmap (\path -> [ToolResourceClaim ToolRead (ToolPath path)])

runViewImage
    :: ToolEnv
    -> ToolCall
    -> ViewImageArgs
    -> IO (Either Text ToolHandlerResult)
runViewImage env _call args =
    case args.detail of
        Just detail | detail /= "high" ->
            pure . Left $
                "view_image.detail only supports `high` for this model, got `"
                    <> detail <> "`"
        _ ->
            resolveForRead env (fromText args.path) >>= \case
                Left err -> pure (Left err)
                Right imagePath -> do
                    display <- displayPathInWorkspace env imagePath
                    doesFileExist imagePath >>= \case
                        False -> pure (Left ("File not found: " <> display))
                        True -> readImageFileResult imagePath display
