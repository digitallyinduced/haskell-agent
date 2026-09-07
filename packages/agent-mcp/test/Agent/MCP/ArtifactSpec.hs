module Agent.MCP.ArtifactSpec (spec) where

import Agent.Json (rawJsonFromEncoding)
import Agent.MCP.Artifact (materializeArtifacts, maximumArtifactBytes)
import Agent.MCP.Types (McpResourceContent(..))
import Control.Exception.Safe (bracket)
import Data.Aeson (Value, object, (.=), toEncoding)
import Data.IORef (newIORef, modifyIORef', readIORef)
import Data.Bits ((.&.))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive, listDirectory, createDirectory, removeFile)
import System.IO (openTempFile, hClose)
import System.Posix.Files (getFileStatus, fileMode, setFileMode)
import Test.Hspec

spec :: Spec
spec = describe "MCP artifact materialization" do
    it "fetches only the originating resource URI and returns a private file" $
        withDirectory \directory -> do
            seen <- newIORef []
            result <- materializeArtifacts directory (\uri -> do
                modifyIORef' seen (uri :)
                pure (Right [resource uri "YWJj"])) (payload [link "note.txt" 3 True])
            readIORef seen `shouldReturn` ["opaque:attachment"]
            case result of
                Right [path] -> do
                    let filename = Text.unpack (Text.drop 11 path)
                    readFile filename `shouldReturn` "abc"
                    status <- getFileStatus filename
                    (fileMode status .&. 0o777) `shouldBe` 0o600
                _ -> expectationFailure (show result)
            tooLarge <- materializeArtifacts directory
                (\_ -> expectationFailure "unexpected fetch" >> pure (Right []))
                (payload [link "one.bin" maximumArtifactBytes True, link "two.bin" 1 True])
            tooLarge `shouldSatisfy` isLeft
    it "does not fetch ordinary resource links" $
        withDirectory \directory ->
            materializeArtifacts directory (\_ -> expectationFailure "unexpected fetch" >> pure (Right []))
                (payload [link "note.txt" 3 False]) `shouldReturn` Right []
    it "rejects traversal and oversized advertised sizes before fetching" $
        withDirectory \directory -> do
            let fetch _ = expectationFailure "unexpected fetch" >> pure (Right [])
            mapM_ (\value -> do
                result <- materializeArtifacts directory fetch (payload [value])
                result `shouldSatisfy` isLeft)
                [link "../secret" 3 True, link "note.txt" (maximumArtifactBytes + 1) True]
            listDirectory directory `shouldReturn` []
    it "preserves the exact 20 MiB decoded attachment boundary" $
        withDirectory \directory -> do
            maximumArtifactBytes `shouldBe` 20 * 1024 * 1024
            let bytes = BS.replicate maximumArtifactBytes 97
                encoded = Text.decodeUtf8 (Base64.encode bytes)
            result <- materializeArtifacts directory
                (\uri -> pure (Right [resource uri encoded]))
                (payload [link "large.bin" maximumArtifactBytes True])
            case result of
                Right [path] ->
                    BS.readFile (Text.unpack (Text.drop 11 path)) `shouldReturn` bytes
                _ -> expectationFailure (show result)
    it "rejects mismatched resource URIs and actual lengths without files" $
        withDirectory \directory -> do
            mapM_ (\value -> do
                result <- materializeArtifacts directory (\_ -> pure (Right [value]))
                    (payload [link "note.txt" 3 True])
                result `shouldSatisfy` isLeft)
                [resource "different" "YWJj", resource "opaque:attachment" "YWJjZA=="
                ,resource "opaque:attachment" "%%%%"]
            listDirectory directory `shouldReturn` []
    it "does not fetch artifacts in an error result" $
        withDirectory \directory ->
            materializeArtifacts directory (\_ -> expectationFailure "unexpected fetch" >> pure (Right []))
                (rawJsonFromEncoding (toEncoding (object
                    ["isError" .= True, "content" .= [link "note.txt" 3 True]])))
                `shouldReturn` Right []
    it "rejects a non-private destination before fetching" $
        withDirectory \directory -> do
            setFileMode directory 0o755
            result <- materializeArtifacts directory (\_ -> expectationFailure "unexpected fetch" >> pure (Right []))
                (payload [link "note.txt" 3 True])
            result `shouldSatisfy` isLeft
    it "removes completed artifacts if a later resource fails" $
        withDirectory \directory -> do
            counter <- newIORef (0 :: Int)
            result <- materializeArtifacts directory (\uri -> do
                modifyIORef' counter (+ 1)
                n <- readIORef counter
                pure $ if n == 1 then Right [resource uri "YWJj"] else Left "private error")
                (payload [link "one.txt" 3 True, link "two.txt" 3 True])
            result `shouldSatisfy` isLeft
            listDirectory directory `shouldReturn` []

isLeft (Left _) = True
isLeft _ = False

resource uri blob = McpResourceContent uri Nothing Nothing (Just blob)
payload contents = rawJsonFromEncoding (toEncoding (object ["content" .= contents]))
link :: Text.Text -> Int -> Bool -> Value
link name size tagged = object $
    [ "type" .= ("resource_link" :: Text.Text)
    , "uri" .= ("opaque:attachment" :: Text.Text)
    , "name" .= name
    , "size" .= size
    ] <> ["_meta" .= object ["dev.haskell-agent/artifact" .= True] | tagged]

withDirectory = bracket
    (do
        root <- getTemporaryDirectory
        (path, handle) <- openTempFile root "mcp-artifact-test"
        hClose handle
        removeFile path
        createDirectory path
        setFileMode path 0o700
        pure path)
    removeDirectoryRecursive
