module Agent.Tools.ShowImageSpec (spec) where

import Agent.Loop (ImageAttachment(..))
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , ToolDispatchConfig(..)
    , dispatchToolCall
    , functionToolCall
    )
import Agent.ToolDSL (PropertySchema(..))
import Agent.Tools.Scheduling
    ( ToolAccess(..)
    , ToolResource(..)
    , ToolResourceClaim(..)
    , ToolSchedulingPlan(..)
    )
import Agent.Tools.ShowImage
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
import Control.Exception.Safe (bracket)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
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
spec = describe "Agent.Tools.ShowImage" do
    describe "sniffImageMime" do
        it "recognizes common raster formats by magic bytes" do
            sniffImageMime pngBytes `shouldBe` Just "image/png"
            sniffImageMime (BS.pack [0xff, 0xd8, 0xff, 0xe0, 0, 0])
                `shouldBe` Just "image/jpeg"
            sniffImageMime "GIF89a\0\0" `shouldBe` Just "image/gif"
            sniffImageMime ("BM" <> BS.replicate 20 0) `shouldBe` Just "image/bmp"
            sniffImageMime (BS.pack [0x49, 0x49, 0x2a, 0x00, 1])
                `shouldBe` Just "image/tiff"

        it "rejects text and vector formats" do
            sniffImageMime "<svg xmlns=\"http://www.w3.org/2000/svg\"/>"
                `shouldBe` Nothing
            sniffImageMime "" `shouldBe` Nothing

    it "advertises a read-only, parallel-safe path tool" do
        withTool (const (pure (Right ()))) \_ tool -> do
            tool.appToolName `shouldBe` "show_image"
            let parameters = fromMaybe [] (jsonToolParameters tool)
            map (.propertyName) parameters `shouldBe` ["path", "caption"]
            map (.required) parameters `shouldBe` [True, False]
            case tool.appToolApproval of
                AlwaysReadOnly -> pure ()
                _ -> expectationFailure "show_image should not require approval"
            tool.appToolExecution `shouldBe` ParallelSafe

    it "hands the sniffed image and call id to the host hook" do
        seen <- newIORef Nothing
        withTool (\request -> writeIORef seen (Just request) >> pure (Right ())) \workspace tool -> do
            BS.writeFile (workspace </> "preview.jpg") pngBytes
            output <- runTool tool "{\"path\":\"preview.jpg\",\"caption\":\"  Blue icon \"}"
            request <- readIORef seen
            fmap (.displayCallId) request `shouldBe` Just "show-1"
            fmap (.displayPath) request `shouldBe` Just "preview.jpg"
            fmap (.displayCaption) request `shouldBe` Just (Just "Blue icon")
            fmap (.displayImage) request
                `shouldBe` Just (ImageAttachment "image/png" pngBytes)
            output `shouldSatisfy` Text.isPrefixOf "Displayed preview.jpg to the user (image/png, "
            output `shouldSatisfy` Text.isInfixOf "Caption: Blue icon"
            output `shouldSatisfy` Text.isInfixOf "not added to your context"

    it "omits the caption line when none was given" do
        withTool (const (pure (Right ()))) \workspace tool -> do
            BS.writeFile (workspace </> "shot.png") pngBytes
            output <- runTool tool "{\"path\":\"shot.png\"}"
            output `shouldNotSatisfy` Text.isInfixOf "Caption:"

    it "reports files that are not raster images without calling the host" do
        calls <- newIORef (0 :: Int)
        withTool (\_ -> modifyIORef' calls (+ 1) >> pure (Right ())) \workspace tool -> do
            writeFile (workspace </> "icon.svg") "<svg xmlns=\"http://www.w3.org/2000/svg\"/>"
            output <- runTool tool "{\"path\":\"icon.svg\"}"
            output `shouldSatisfy` Text.isInfixOf "not a supported image"
            output `shouldSatisfy` Text.isInfixOf "Convert other formats"
            readIORef calls `shouldReturn` 0

    it "reports missing files" do
        withTool (const (pure (Right ()))) \_ tool -> do
            output <- runTool tool "{\"path\":\"missing.png\"}"
            output `shouldSatisfy` Text.isInfixOf "File not found: missing.png"

    it "refuses paths outside the workspace" do
        withTool (const (pure (Right ()))) \_ tool -> do
            output <- runTool tool "{\"path\":\"/etc/hosts\"}"
            output `shouldNotSatisfy` Text.isPrefixOf "Displayed"

    it "surfaces host presentation failures to the model" do
        withTool (const (pure (Left "terminal has no graphics support"))) \workspace tool -> do
            BS.writeFile (workspace </> "shot.png") pngBytes
            output <- runTool tool "{\"path\":\"shot.png\"}"
            output `shouldSatisfy`
                Text.isInfixOf "Could not display shot.png: terminal has no graphics support"

    it "claims a read on the resolved image path" do
        withTool (const (pure (Right ()))) \workspace tool -> do
            BS.writeFile (workspace </> "shot.png") pngBytes
            registry <- either (fail . Text.unpack) pure (mkToolRegistry [tool])
            plan <- toolSchedulingPlanFor registry
                (functionToolCall "show-2" "show_image" "{\"path\":\"shot.png\"}")
            canonical <- canonicalizePath (workspace </> "shot.png")
            plan `shouldBe`
                ToolResourceClaims
                    [ ToolResourceClaim ToolRead
                        (ToolPath (unsafeEncodeUtf canonical))
                    ]

-- | A minimal valid 1×1 PNG.
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

withTool
    :: (ImageDisplayRequest -> IO (Either Text ()))
    -> (FilePath -> AppTool -> IO a)
    -> IO a
withTool hook action = do
    root <- getTemporaryDirectory
    bracket
        (mkdtemp (root </> "agent-show-image-test-"))
        removeDirectoryRecursive
        \workspace -> do
            temp <- mkdtemp (workspace </> "session-tmp-")
            env <- defaultToolEnv (unsafeEncodeUtf workspace)
            setToolSessionTmp env (Just (unsafeEncodeUtf temp))
            action workspace (showImageTool env (ImageDisplayHooks hook))

runTool :: AppTool -> Text -> IO Text
runTool tool arguments = do
    result <- dispatchToolCall testDispatchConfig
        [tool.appToolHandler]
        (functionToolCall "show-1" "show_image" arguments)
    pure result.output

testDispatchConfig :: ToolDispatchConfig
testDispatchConfig = ToolDispatchConfig
    { toolDispatchUnknownTool = \name -> "unknown:" <> name
    , toolDispatchFormatResult = either ("ERR " <>) id
    , toolDispatchFormatException = \name _ -> "EX " <> name
    , toolDispatchOnException = \_ _ -> pure ()
    , toolDispatchOnOutput = \_ _ -> pure ()
    , toolDispatchFinalizeOutput = \_ output -> pure output
    }
