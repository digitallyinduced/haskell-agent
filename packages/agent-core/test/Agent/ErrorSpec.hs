module Agent.ErrorSpec (spec) where

import Agent.Error
import qualified Data.Aeson as Aeson
import Test.Hspec

spec :: Spec
spec = describe "ErrorType" do
    it "maps wire discriminators directly" do
        errorTypeFromText "usage_limit_reached" `shouldBe` UsageLimitReached
        errorTypeText UsageLimitReached `shouldBe` "usage_limit_reached"

    it "preserves unknown discriminators" do
        let unknown = UnknownErrorType "future_error"
        errorTypeFromText "future_error" `shouldBe` unknown
        Aeson.fromJSON (Aeson.toJSON unknown) `shouldBe` Aeson.Success unknown
