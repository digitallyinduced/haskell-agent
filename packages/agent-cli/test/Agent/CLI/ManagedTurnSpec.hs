module Agent.CLI.ManagedTurnSpec (spec) where

import Agent.CLI.ManagedTurn
    ( ManagedTurnMedia(..)
    , ManagedTurnRequest(..)
    , managedTurnInputs
    , managedTurnRequestFromText
    , loadManagedTurnRequest
    , renderManagedTurnPrompt
    )
import Agent.Loop
    ( FileAttachment(..)
    , ImageAttachment(..)
    , TurnInput(..)
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
import Control.Exception (finally)
import Data.Unique (newUnique, hashUnique)
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.ManagedTurn" do
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
                    [ UserMultimodalFiles
                        { userText = "summarize"
                        , userImages =
                            [ ImageAttachment
                                { imageMime = "image/test"
                                , imageBytes = "image-one"
                                }
                            , ImageAttachment
                                { imageMime = "image/test"
                                , imageBytes = "image-two"
                                }
                            ]
                        , userFiles =
                            [ FileAttachment
                                { fileName = Just "one.txt"
                                , fileMime = "text/plain"
                                , fileBytes = "file-one"
                                }
                            , FileAttachment
                                { fileName = Just "two.txt"
                                , fileMime = "text/plain"
                                , fileBytes = "file-two"
                                }
                            ]
                        }
                    ]

withManagedTempDir :: (FilePath -> IO a) -> IO a
withManagedTempDir action = do
    root <- getTemporaryDirectory
    unique <- hashUnique <$> newUnique
    let dir = root </> ("agent-cli-managed-turn-spec-" <> show unique)
    createDirectoryIfMissing True dir
    action dir `finally` removePathForcibly dir
