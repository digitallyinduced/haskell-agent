module Agent.CLI.ManagedTurnSpec (spec) where

import Agent.CLI.ManagedTurn
    ( managedTurnRequestFromText
    , loadManagedTurnRequest
    , renderManagedTurnPrompt
    )
import Agent.OsPath (fromText)
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

withManagedTempDir :: (FilePath -> IO a) -> IO a
withManagedTempDir action = do
    root <- getTemporaryDirectory
    unique <- hashUnique <$> newUnique
    let dir = root </> ("agent-cli-managed-turn-spec-" <> show unique)
    createDirectoryIfMissing True dir
    action dir `finally` removePathForcibly dir
