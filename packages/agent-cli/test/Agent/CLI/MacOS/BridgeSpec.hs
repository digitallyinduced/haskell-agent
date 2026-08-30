module Agent.CLI.MacOS.BridgeSpec (spec) where

import Agent.CLI.MacOS.Bridge
    ( TurnStart(..)
    , nativeTurnArguments
    )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Test.Hspec

spec :: Spec
spec = describe "native computer-use turn propagation" do
    it "defaults omitted computerUse to disabled" do
        case decodeStart
            "{\"turnId\":\"t\",\"prompt\":\"p\",\"cwd\":\"/tmp\"}" of
            Left err -> expectationFailure err
            Right start -> do
                start.turnStartComputerUse `shouldBe` False
                nativeTurnArguments start
                    `shouldContain` ["--no-computer-use"]
                nativeTurnArguments start
                    `shouldNotContain` ["--computer-use"]

    it "passes an explicit computerUse opt-in to the native runtime" do
        case decodeStart
            "{\"turnId\":\"t\",\"prompt\":\"p\",\"cwd\":\"/tmp\",\"computerUse\":true}" of
            Left err -> expectationFailure err
            Right start -> do
                start.turnStartComputerUse `shouldBe` True
                nativeTurnArguments start
                    `shouldContain` ["--computer-use"]
                nativeTurnArguments start
                    `shouldNotContain` ["--no-computer-use"]

decodeStart :: String -> Either String TurnStart
decodeStart = Aeson.eitherDecode . LBS8.pack
