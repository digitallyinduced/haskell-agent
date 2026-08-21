module Agent.OpenAI.LoginSpec (spec) where

import Agent.OpenAI.Login (writeAuthFile)
import Agent.OsPath (fromFilePath)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileMode, getFileStatus)
import Test.Hspec

spec :: Spec
spec = describe "Agent.OpenAI.Login" do
    it "writes auth JSON with owner-only permissions" $
        withSystemTempDirectory "codex-hs-login" \directory -> do
            let path = directory </> "nested" </> "auth.json"
                osPath = fromFilePath path
                auth = Aeson.object ["auth_mode" Aeson..= ("chatgpt" :: String)]
            writeAuthFile osPath auth
            decoded <- Aeson.decode <$> LBS.readFile path
            decoded `shouldBe` Just auth
            status <- getFileStatus path
            fileMode status `shouldBe` 0o100600
