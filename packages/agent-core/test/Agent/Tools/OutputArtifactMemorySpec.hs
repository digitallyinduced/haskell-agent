module Agent.Tools.OutputArtifactMemorySpec (spec) where

import Agent.Tools.OutputArtifact
import Agent.Tools.Types
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception.Safe (bracket)
import Control.Monad (replicateM)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import Data.Bits ((.&.))
import Data.Char (isHexDigit)
import Data.Either (isLeft)
import Data.List (nub)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import System.Directory
    ( createDirectory, createFileLink, getTemporaryDirectory
    , listDirectory, removeDirectoryRecursive
    )
import System.FilePath ((</>))
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Files (fileMode, getFileStatus)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

spec :: Spec
spec = describe "resident tool-output artifacts" do
    it "uses full random identifiers rather than process-local counters" do
        artifacts <- replicateM 32 do
            env <- defaultToolEnv (unsafeEncodeUtf ".")
            requireRight =<< writeOutputArtifactDetailed env "hello"
        let handles = map (.artifactHandle) artifacts
        length (nub handles) `shouldBe` 32
        mapM_ (\handle -> case Text.stripPrefix "output-memory-" handle of
            Nothing -> expectationFailure "missing resident handle prefix"
            Just suffix -> do
                Text.length suffix `shouldBe` 32
                suffix `shouldSatisfy` Text.all isHexDigit) handles

    it "reads bounded responses without any scratch directory" do
        env <- defaultToolEnv (unsafeEncodeUtf ".")
        artifact <- requireRight =<< writeOutputArtifactDetailed env "hello"
        artifact.artifactPath `shouldBe` Nothing
        readOutputArtifact env artifact.artifactHandle `shouldReturn` Right "hello"

    it "accounts for aggregate resident bytes and falls back without evicting earlier handles" $
        withScratch \env _ -> do
            let limited = env { toolOutputMemoryCap = 261 }
            first <- requireRight =<< writeOutputArtifactDetailed limited "hello"
            second <- requireRight =<< writeOutputArtifactDetailed limited "world"
            first.artifactPath `shouldBe` Nothing
            second.artifactPath `shouldSatisfy` isJust
            readOutputArtifact limited first.artifactHandle `shouldReturn` Right "hello"
            readOutputArtifact limited second.artifactHandle `shouldReturn` Right "world"

    it "bounds entry overhead even for empty artifacts" $
        withScratch \env _ -> do
            artifacts <- replicateM 3
                (requireRight =<< writeOutputArtifactDetailed
                    (env { toolOutputMemoryCap = 512 }) "")
            length (filter (isNothing . (.artifactPath)) artifacts) `shouldBe` 2

    it "applies the aggregate limit atomically across concurrent writers" $
        withScratch \env _ -> do
            artifacts <- mapConcurrently
                (\_ -> requireRight =<< writeOutputArtifactDetailed
                    (env { toolOutputMemoryCap = 261 }) "hello")
                ([1 .. 12] :: [Int])
            length (filter (isNothing . (.artifactPath)) artifacts) `shouldBe` 1
            length (nub (map (.artifactHandle) artifacts)) `shouldBe` 12

    it "does not share resident handles between independent environments" do
        first <- defaultToolEnv (unsafeEncodeUtf ".")
        second <- defaultToolEnv (unsafeEncodeUtf ".")
        artifact <- requireRight =<< writeOutputArtifactDetailed first "hello"
        readOutputArtifact second artifact.artifactHandle >>= (`shouldSatisfy` isLeft)

    it "shares resident handles explicitly and clears them for all participants" do
        parent <- defaultToolEnv (unsafeEncodeUtf ".")
        independent <- defaultToolEnv (unsafeEncodeUtf ".")
        let child = independent { toolOutputMemoryStore = parent.toolOutputMemoryStore }
        artifact <- requireRight =<< writeOutputArtifactDetailed parent "hello"
        readOutputArtifact child artifact.artifactHandle `shouldReturn` Right "hello"
        clearMemoryOutputArtifacts parent.toolOutputMemoryStore
        readOutputArtifact child artifact.artifactHandle >>= (`shouldSatisfy` isLeft)

    it "clears resident output when the scratch session changes" $
        withScratch \env _ -> do
            artifact <- requireRight =<< writeOutputArtifactDetailed env "hello"
            setToolSessionTmp env Nothing
            readOutputArtifact env artifact.artifactHandle >>= (`shouldSatisfy` isLeft)

    it "exports exact bytes only on request and creates unique private snapshots" $
        withScratch \env directory -> do
            let bytes = Encoding.encodeUtf8 "{\"amount\":123,\"currency\":\"€\"}"
            artifact <- requireRight =<< writeOutputArtifactDetailed env bytes
            listDirectory directory `shouldReturn` []
            first <- requireRight =<< exportOutputArtifact env artifact.artifactHandle
            second <- requireRight =<< exportOutputArtifact env artifact.artifactHandle
            firstObject <- decodeObject first
            secondObject <- decodeObject second
            firstPath <- decodePath firstObject
            secondPath <- decodePath secondObject
            firstPath `shouldNotBe` secondPath
            ByteString.readFile firstPath `shouldReturn` bytes
            ByteString.readFile secondPath `shouldReturn` bytes
            KeyMap.lookup "complete" firstObject `shouldBe` Just (Aeson.Bool True)
            permissions <- fileMode <$> getFileStatus firstPath
            permissions .&. 0o777 `shouldBe` 0o600

    it "preserves a known storage truncation warning on export" $
        withScratch \env _ -> do
            artifact <- requireRight =<< writeOutputArtifactDetailed
                (env { toolOutputArtifactCap = 3 }) "hello"
            rendered <- requireRight =<< exportOutputArtifact env artifact.artifactHandle
            object <- decodeObject rendered
            KeyMap.lookup "complete" object `shouldBe` Just (Aeson.Bool False)
            path <- decodePath object
            ByteString.readFile path `shouldReturn` "hel"

    it "preserves invalid UTF-8 bytes rather than exporting replacement characters" $
        withScratch \env _ -> do
            let bytes = ByteString.pack [255, 0, 195, 40]
            artifact <- requireRight =<< writeOutputArtifactDetailed env bytes
            rendered <- requireRight =<< exportOutputArtifact env artifact.artifactHandle
            object <- decodeObject rendered
            path <- decodePath object
            ByteString.readFile path `shouldReturn` bytes

    it "does not invent completeness metadata for disk-only artifacts" $
        withScratch \env _ -> do
            artifact <- requireRight =<< writeOutputArtifactDetailed
                (env { toolOutputMemoryCap = 0, toolOutputArtifactCap = 3 }) "hello"
            rendered <- requireRight =<< exportOutputArtifact env artifact.artifactHandle
            object <- decodeObject rendered
            KeyMap.lookup "complete" object `shouldBe` Just Aeson.Null
            path <- decodePath object
            ByteString.readFile path `shouldReturn` "hel"

    it "fails export without scratch storage while retaining the resident output" do
        env <- defaultToolEnv (unsafeEncodeUtf ".")
        artifact <- requireRight =<< writeOutputArtifactDetailed env "hello"
        exportOutputArtifact env artifact.artifactHandle >>= (`shouldSatisfy` isLeft)
        readOutputArtifact env artifact.artifactHandle `shouldReturn` Right "hello"

    it "rejects traversal and missing handles before creating export files" $
        withScratch \env directory -> do
            exportOutputArtifact env "../secret" >>= (`shouldSatisfy` isLeft)
            exportOutputArtifact env "output-missing" >>= (`shouldSatisfy` isLeft)
            listDirectory directory `shouldReturn` []

    it "rejects a symlink source without creating an export" $
        withScratch \env directory -> do
            let artifacts = directory </> "tool-output-artifacts"
            createDirectory artifacts
            ByteString.writeFile (directory </> "source") "secret"
            createFileLink (directory </> "source") (artifacts </> "output-link")
            exportOutputArtifact env "output-link" >>= (`shouldSatisfy` isLeft)
            listDirectory artifacts `shouldReturn` ["output-link"]

    it "removes incomplete exports when the export limit is exceeded" $
        withScratch \env directory -> do
            artifact <- requireRight =<< writeOutputArtifactDetailed
                (env { toolOutputMemoryCap = 0 }) "hello"
            exportOutputArtifact (env { toolOutputArtifactCap = 3 })
                artifact.artifactHandle >>= (`shouldSatisfy` isLeft)
            listDirectory (directory </> "tool-output-artifacts")
                `shouldReturn` [Text.unpack artifact.artifactHandle]
            readOutputArtifact env artifact.artifactHandle `shouldReturn` Right "hello"

requireRight :: Either Text a -> IO a
requireRight = either (fail . Text.unpack) pure

decodeObject :: Text -> IO (KeyMap.KeyMap Aeson.Value)
decodeObject text = case Aeson.eitherDecodeStrict' (Encoding.encodeUtf8 text) of
    Right (Aeson.Object object) -> pure object
    _ -> fail "export did not return a JSON object"

decodePath :: KeyMap.KeyMap Aeson.Value -> IO FilePath
decodePath object = case KeyMap.lookup "path" object of
    Just (Aeson.String path) -> pure (Text.unpack path)
    _ -> fail "export did not return a path"

withScratch :: (ToolEnv -> FilePath -> IO a) -> IO a
withScratch action = do
    root <- getTemporaryDirectory
    bracket (mkdtemp (root </> "artifact-memory-test-")) removeDirectoryRecursive \directory -> do
        env <- defaultToolEnv (unsafeEncodeUtf directory)
        setToolSessionTmp env (Just (unsafeEncodeUtf directory))
        action env directory
