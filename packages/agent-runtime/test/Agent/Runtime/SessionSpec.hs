module Agent.Runtime.SessionSpec (spec) where

import Agent.Runtime.SessionSpec.Compatibility qualified as Compatibility
import Agent.Runtime.SessionSpec.Persistence qualified as Persistence
import Agent.Runtime.SessionSpec.JsonCodec qualified as JsonCodec
import Test.Hspec

spec :: Spec
spec = describe "Agent.Runtime.Session" do
    Compatibility.spec
    Persistence.spec
    JsonCodec.spec
