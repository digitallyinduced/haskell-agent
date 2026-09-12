module Agent.Runtime.ManagedTurnSpec (spec) where

import Agent.Runtime.AgentSessions.Process (waitForManagedSessionReadyWith)
import Agent.Runtime.ManagedTurn
    ( ManagedTurnMedia(..)
    , ManagedTurnRequest(..)
    , managedTurnInputs
    , managedTurnRequestFromText
    , loadTextPrompt
    , loadManagedTurnRequest
    , renderManagedTurnPrompt
    )
import Agent.Loop
    ( FileAttachment(..)
    , ImageAttachment(..)
    , TurnAttachment(..)
    , userMessageWithAttachments
    )
import Agent.OsPath (fromText)
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory
    ( createDirectoryIfMissing
    , getTemporaryDirectory
    , removePathForcibly
    )
import System.FilePath ((</>))
import Control.Exception.Safe (finally)
import Data.Unique (newUnique, hashUnique)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import System.Exit (ExitCode(..))
import Test.Hspec

spec :: Spec
spec = describe "Agent.Runtime.ManagedTurn" do
    describe "managed session readiness" do
        let observeExit finalContents exitCode = do
                contents <- newIORef Nothing
                reads <- newIORef (0 :: Int)
                result <- waitForManagedSessionReadyWith
                    (modifyIORef' reads (+ 1) >> readIORef contents)
                    (writeIORef contents finalContents >> pure (Just exitCode))
                readIORef reads `shouldReturn` 2
                pure result

        it "accepts readiness published between the initial read and process exit" do
            observeExit (Just "ready\n") ExitSuccess `shouldReturn` Right ()

        it "preserves an error published between the initial read and process exit" do
            observeExit (Just "error\ncould not acquire lock") (ExitFailure 1)
                `shouldReturn` Left "could not acquire lock"

        it "does not mistake a successful exit without readiness for readiness" do
            observeExit Nothing ExitSuccess
                `shouldReturn` Left "agent session exited before acquiring its lock"

        it "reports the exit code when the final marker is incomplete" do
            observeExit (Just "rea") (ExitFailure 7)
                `shouldReturn`
                    Left "agent session exited before acquiring its lock (exit code 7)"

        it "returns published readiness without requiring the child to exit" do
            waitForManagedSessionReadyWith
                (pure (Just "ready\n"))
                (expectationFailure "unexpected exit observation" >> pure Nothing)
                `shouldReturn` Right ()

    it "loads --prompt-file as plain text" $
        withManagedTempDir \dir -> do
            let pathFile = dir </> "prompt.txt"
                path = fromText (Text.pack pathFile)
            Text.writeFile pathFile "  inspect this project\ncarefully  \n"
            loadTextPrompt path `shouldReturn`
                managedTurnRequestFromText "inspect this project\ncarefully"

    it "round-trips a prompt-file request through JSON" $
        withManagedTempDir \dir -> do
            let request = managedTurnRequestFromText "hello"
                pathFile = dir </> "prompt.json"
                path = fromText (Text.pack pathFile)
            Text.writeFile pathFile (renderManagedTurnPrompt request)
            loaded <- loadManagedTurnRequest path
            loaded `shouldBe` Right request

    it "rejects non-JSON managed turn files" $
        withManagedTempDir \dir -> do
            let pathFile = dir </> "prompt.txt"
                path = fromText (Text.pack pathFile)
            Text.writeFile pathFile "plain text"
            loaded <- loadManagedTurnRequest path
            loaded `shouldSatisfy` \case
                Left err -> "could not decode" `Text.isInfixOf` err
                Right _ -> False

    it "loads multimodal images and files concurrently without reordering them" $
        withManagedTempDir \dir -> do
            let imagePaths =
                    [ dir </> "image-1.bin"
                    , dir </> "image-2.bin"
                    ]
                filePaths =
                    [ dir </> "file-1.txt"
                    , dir </> "file-2.txt"
                    ]
                write path contents =
                    ByteString.Char8.writeFile path contents
            mapM_ (\(path, contents) -> write path contents)
                [ (imagePaths !! 0, "image-one")
                , (imagePaths !! 1, "image-two")
                , (filePaths !! 0, "file-one")
                , (filePaths !! 1, "file-two")
                ]
            let request =
                    (managedTurnRequestFromText "summarize")
                        { managedTurnImages =
                            [ ManagedTurnMedia
                                { managedTurnMediaPath = imagePaths !! 0
                                , managedTurnMediaMime = "image/test"
                                , managedTurnMediaName = Nothing
                                }
                            , ManagedTurnMedia
                                { managedTurnMediaPath = imagePaths !! 1
                                , managedTurnMediaMime = "image/test"
                                , managedTurnMediaName = Nothing
                                }
                            ]
                        , managedTurnFiles =
                            [ ManagedTurnMedia
                                { managedTurnMediaPath = filePaths !! 0
                                , managedTurnMediaMime = "text/plain"
                                , managedTurnMediaName = Just "one.txt"
                                }
                            , ManagedTurnMedia
                                { managedTurnMediaPath = filePaths !! 1
                                , managedTurnMediaMime = "text/plain"
                                , managedTurnMediaName = Just "two.txt"
                                }
                            ]
                        }
            managedTurnInputs (fromText (Text.pack dir)) request
                `shouldReturn`
                    [ userMessageWithAttachments
                        "summarize"
                        [ ImageAttachmentItem
                            (ImageAttachment
                                { imageMime = "image/test"
                                , imageBytes = "image-one"
                                })
                        , ImageAttachmentItem
                            (ImageAttachment
                                { imageMime = "image/test"
                                , imageBytes = "image-two"
                                })
                        , FileAttachmentItem
                            (FileAttachment
                                { fileName = Just "one.txt"
                                , fileMime = "text/plain"
                                , fileBytes = "file-one"
                                })
                        , FileAttachmentItem
                            (FileAttachment
                                { fileName = Just "two.txt"
                                , fileMime = "text/plain"
                                , fileBytes = "file-two"
                                })
                        ]
                    ]

withManagedTempDir :: (FilePath -> IO a) -> IO a
withManagedTempDir action = do
    root <- getTemporaryDirectory
    unique <- hashUnique <$> newUnique
    let dir = root </> ("agent-cli-managed-turn-spec-" <> show unique)
    createDirectoryIfMissing True dir
    action dir `finally` removePathForcibly dir
