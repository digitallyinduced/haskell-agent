module Main (main) where

import Agent.OsPath (unsafeToFilePath)
import Agent.Runtime.Session.TempWorkspace
import Control.Exception.Safe (bracket, tryIO)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Foreign.C.Error (eACCES, eNOSPC, errnoToIOError)
import qualified System.Directory as Directory
import qualified System.Directory.OsPath as OsDirectory
import qualified System.FilePath as FilePath
import System.IO.Error
    ( ioeGetErrorString
    , ioeGetErrorType
    , ioeGetFileName
    , ioeGetLocation
    , isUserError
    )
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))
import System.Posix.Files (fileMode, getFileStatus)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

main :: IO ()
main = hspec $ describe "session temporary directory allocation" do
    it "allocates a private directory without materializing durable storage" $
        withSessionRoot \root -> do
            (sessionId, directory) <- allocateSessionTemp root
            sessionTempDirForId root sessionId `shouldBe` Right directory
            OsDirectory.doesDirectoryExist directory `shouldReturn` True
            OsDirectory.doesDirectoryExist root `shouldReturn` False
            status <- getFileStatus (unsafeToFilePath directory)
            fileMode status `mod` 0o1000 `shouldBe` 0o700

    it "retries a real name collision without removing the existing directory" $
        withSessionRoot \root -> do
            attempts <- newIORef []
            let createWithInitialCollision directory = do
                    previous <- readIORef attempts
                    modifyIORef' attempts (<> [directory])
                    case previous of
                        [] -> do
                            OsDirectory.createDirectory directory
                            writeFile
                                (unsafeToFilePath directory FilePath.</> "existing-data")
                                "retain"
                        _ -> pure ()
                    OsDirectory.createDirectory directory
            (_, directory) <-
                allocateSessionTempWithDirectoryCreation createWithInitialCollision root
            attempted <- readIORef attempts
            case attempted of
                [existing, allocated] -> do
                    existing `shouldNotBe` allocated
                    directory `shouldBe` allocated
                    readFile
                        (unsafeToFilePath existing FilePath.</> "existing-data")
                        `shouldReturn` "retain"
                    OsDirectory.doesDirectoryExist allocated `shouldReturn` True
                other -> expectationFailure ("unexpected allocation attempts: " <> show other)

    it "bounds persistent name collisions at 32 attempts" $
        withSessionRoot \root -> do
            attempts <- newIORef (0 :: Int)
            let createWithCollision directory = do
                    modifyIORef' attempts (+ 1)
                    OsDirectory.createDirectory directory
                    OsDirectory.createDirectory directory
            allocateSessionTempWithDirectoryCreation createWithCollision root
                `shouldThrow` \exception ->
                    isUserError exception
                        && ioeGetErrorString exception
                            == "could not allocate a unique session temp directory"
            readIORef attempts `shouldReturn` 32

    mapM_ (\(description, errorNumber) ->
        it ("preserves " <> description <> " and stops after the first attempt") $
            withSessionRoot \root -> do
                attempts <- newIORef []
                let createWithFailure directory = do
                        modifyIORef' attempts (<> [directory])
                        ioError (errnoToIOError
                            "createDirectory"
                            errorNumber
                            Nothing
                            (Just (unsafeToFilePath directory)))
                result <- tryIO $
                    allocateSessionTempWithDirectoryCreation createWithFailure root
                attempted <- readIORef attempts
                case (result, attempted) of
                    (Left exception, [directory]) -> do
                        let original = errnoToIOError
                                "createDirectory"
                                errorNumber
                                Nothing
                                (Just (unsafeToFilePath directory))
                        ioeGetErrorType exception `shouldBe` ioeGetErrorType original
                        ioeGetErrorString exception `shouldBe` ioeGetErrorString original
                        ioeGetFileName exception `shouldBe` ioeGetFileName original
                        ioeGetLocation exception
                            `shouldSatisfy` isInfixOf "allocateSessionTemp"
                        ioeGetLocation exception
                            `shouldSatisfy` isInfixOf "createDirectory"
                    other -> expectationFailure ("unexpected allocation result: " <> show other))
        [ ("permission errors", eACCES)
        , ("capacity errors", eNOSPC)
        ]

withSessionRoot :: (OsPath -> IO a) -> IO a
withSessionRoot action = do
    temporaryRoot <- Directory.getTemporaryDirectory
    bracket
        (mkdtemp (temporaryRoot FilePath.</> "session-allocation-XXXXXX"))
        Directory.removeDirectoryRecursive
        \directory ->
            action (unsafeEncodeUtf directory </> unsafeEncodeUtf "sessions")
