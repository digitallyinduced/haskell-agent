module Agent.OpenAI.LoginSpec (spec) where

import qualified Agent.Json.Decode as Json
import Agent.OpenAI.Login (DeviceCode(..), writeAuthFile)
import System.OsPath (unsafeEncodeUtf)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileMode, getFileStatus)
import Test.Hspec

fromFilePath = unsafeEncodeUtf

spec :: Spec
spec = describe "Agent.OpenAI.Login" do
    it "redacts device authorization credentials from Show output" do
        let code = DeviceCode
                { verificationUrl =
                    "https://auth.example/device?user_code=USER-SECRET"
                , userCode = "USER-SECRET"
                , deviceAuthId = "DEVICE-AUTH-SECRET"
                , pollIntervalSeconds = 7
                }
            rendered = show code
        rendered `shouldContain` "pollIntervalSeconds = 7"
        rendered `shouldNotContain` "USER-SECRET"
        rendered `shouldNotContain` "DEVICE-AUTH-SECRET"
        rendered `shouldNotContain` "https://auth.example"

    it "writes auth JSON with owner-only permissions" $
        withSystemTempDirectory "codex-hs-login" \directory -> do
            let path = directory </> "nested" </> "auth.json"
                osPath = fromFilePath path
                auth = Aeson.object ["auth_mode" Aeson..= ("chatgpt" :: String)]
            writeAuthFile osPath auth
            bytes <- LBS.toStrict <$> LBS.readFile path
            Json.decodeEither
                (Json.object (Json.atKey "auth_mode" Json.text))
                bytes
                `shouldBe` Right "chatgpt"
            status <- getFileStatus path
            fileMode status `shouldBe` 0o100600
