module Agent.Tools.ViewImageSpec (spec) where

import Agent.ToolDispatch
    ( ToolCallResult(..)
    , ToolResultImage(..)
    , ToolDispatchConfig(..)
    , dispatchToolCall
    , functionToolCall
    , toolCallResultImages
    )
import Agent.ToolDSL (PropertySchema(..))
import Agent.Tools.Scheduling
    ( ToolAccess(..)
    , ToolResource(..)
    , ToolResourceClaim(..)
    , ToolSchedulingPlan(..)
    )
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , defaultToolEnv
    , jsonToolParameters
    , mkToolRegistry
    , setToolSessionTmp
    , toolSchedulingPlanFor
    )
import Agent.Tools.ViewImage (viewImageTool)
import Control.Exception.Safe (bracket)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import System.Directory
    ( canonicalizePath
    , getTemporaryDirectory
    , removeDirectoryRecursive
    )
import System.FilePath ((</>))
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

spec :: Spec
spec = describe "Agent.Tools.ViewImage" do
    it "advertises a read-only, parallel-safe model-facing tool" do
        withTool \_ tool -> do
            tool.appToolName `shouldBe` "view_image"
            map (.propertyName) (fromMaybe [] (jsonToolParameters tool))
                `shouldBe` ["path"]
            case tool.appToolApproval of
                AlwaysReadOnly -> pure ()
                _ -> expectationFailure "view_image should not require approval"
            tool.appToolExecution `shouldBe` ParallelSafe

    it "returns a data-url image result for the model" do
        withTool \workspace tool -> do
            BS.writeFile (workspace </> "error.png") pngBytes
            result <- runTool tool "{\"path\":\"error.png\"}"
            case result of
                ToolCallResult{output, toolResultImages = [image]} -> do
                    output `shouldBe` "Viewed image file: error.png"
                    image.imageDetail `shouldBe` Just "high"
                    image.imageUrl `shouldSatisfy`
                        Text.isPrefixOf "data:image/png;base64,"
                _ -> expectationFailure ("expected image result, got " <> show result)

    it "accepts a structurally valid WebP image" do
        withTool \workspace tool -> do
            BS.writeFile (workspace </> "error.webp") webpBytes
            result <- runTool tool "{\"path\":\"error.webp\"}"
            case result of
                ToolCallResult{toolResultImages = [image]} ->
                    image.imageUrl `shouldSatisfy`
                        Text.isPrefixOf "data:image/webp;base64,"
                _ -> expectationFailure ("expected WebP image result, got " <> show result)

    it "rejects unsupported detail hints" do
        withTool \workspace tool -> do
            BS.writeFile (workspace </> "error.png") pngBytes
            result <- runTool tool "{\"path\":\"error.png\",\"detail\":\"original\"}"
            result.output `shouldSatisfy`
                Text.isInfixOf "only supports `high` for this model"

    it "rejects invalid detail and non-image input" do
        withTool \workspace tool -> do
            BS.writeFile (workspace </> "notes.txt") "not an image"
            invalidDetail <- runTool tool "{\"path\":\"notes.txt\",\"detail\":\"low\"}"
            invalidDetail.output `shouldSatisfy`
                Text.isInfixOf "only supports `high` for this model"
            invalidImage <- runTool tool "{\"path\":\"notes.txt\"}"
            invalidImage.output `shouldSatisfy` Text.isInfixOf "not a supported image"

    it "rejects corrupt image data after recognizing its magic bytes" do
        withTool \workspace tool -> do
            BS.writeFile (workspace </> "broken.png")
                (BS.take 8 pngBytes <> "not-a-png")
            result <- runTool tool "{\"path\":\"broken.png\"}"
            result.output `shouldSatisfy`
                Text.isInfixOf "invalid or unsupported image data"

    it "reports missing files and refuses paths outside the workspace" do
        withTool \_ tool -> do
            missing <- runTool tool "{\"path\":\"missing.png\"}"
            missing.output `shouldSatisfy`
                Text.isInfixOf "File not found: missing.png"
            outside <- runTool tool "{\"path\":\"/etc/hosts\"}"
            toolCallResultImages outside `shouldBe` []
            outside.output `shouldNotSatisfy` Text.isPrefixOf "Viewed"

    it "claims a read on the resolved image path" do
        withTool \workspace tool -> do
            BS.writeFile (workspace </> "error.png") pngBytes
            registry <- either (fail . Text.unpack) pure (mkToolRegistry [tool])
            plan <- toolSchedulingPlanFor registry
                (functionToolCall "view-2" "view_image" "{\"path\":\"error.png\"}")
            canonical <- canonicalizePath (workspace </> "error.png")
            plan `shouldBe`
                ToolResourceClaims
                    [ ToolResourceClaim ToolRead
                        (ToolPath (unsafeEncodeUtf canonical))
                    ]

pngBytes :: BS.ByteString
pngBytes = BS.pack
    [ 0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a
    , 0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44, 0x52
    , 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01
    , 0x08, 0x02, 0x00, 0x00, 0x00, 0x90, 0x77, 0x53, 0xde
    , 0x00, 0x00, 0x00, 0x0c, 0x49, 0x44, 0x41, 0x54
    , 0x08, 0xd7, 0x63, 0xf8, 0xcf, 0xc0, 0x00, 0x00
    , 0x03, 0x01, 0x01, 0x00, 0x18, 0xdd, 0x8d, 0xb0
    , 0x00, 0x00, 0x00, 0x00, 0x49, 0x45, 0x4e, 0x44
    , 0xae, 0x42, 0x60, 0x82
    ]

-- | A minimal valid 1×1 lossy WebP.
webpBytes :: BS.ByteString
webpBytes = BS.pack
    [ 0x52, 0x49, 0x46, 0x46, 0x22, 0x00, 0x00, 0x00
    , 0x57, 0x45, 0x42, 0x50, 0x56, 0x50, 0x38, 0x20
    , 0x16, 0x00, 0x00, 0x00, 0x30, 0x01, 0x00, 0x9d
    , 0x01, 0x2a, 0x01, 0x00, 0x01, 0x00, 0x0e, 0xc0
    , 0xfe, 0x25, 0xa4, 0x00, 0x03, 0x70, 0x00, 0x00
    , 0x00, 0x00
    ]

withTool :: (FilePath -> AppTool -> IO a) -> IO a
withTool action = do
    root <- getTemporaryDirectory
    bracket
        (mkdtemp (root </> "agent-view-image-test-"))
        removeDirectoryRecursive
        \workspace -> do
            temp <- mkdtemp (workspace </> "session-tmp-")
            env <- defaultToolEnv (unsafeEncodeUtf workspace)
            setToolSessionTmp env (Just (unsafeEncodeUtf temp))
            action workspace (viewImageTool env)

runTool :: AppTool -> Text.Text -> IO ToolCallResult
runTool tool arguments =
    dispatchToolCall testDispatchConfig [tool.appToolHandler]
        (functionToolCall "view-1" "view_image" arguments)

testDispatchConfig :: ToolDispatchConfig
testDispatchConfig = ToolDispatchConfig
    { toolDispatchUnknownTool = \name -> "unknown:" <> name
    , toolDispatchFormatResult = either ("ERR " <>) id
    , toolDispatchFormatException = \name _ -> "EX " <> name
    , toolDispatchOnException = \_ _ -> pure ()
    , toolDispatchOnOutput = \_ _ -> pure ()
    , toolDispatchFinalizeOutput = \_ output -> pure output
    }
