module Agent.CLI.SessionSpec (spec) where

import Agent.CLI.SessionSpec.Compatibility qualified as Compatibility
import Agent.CLI.SessionSpec.Persistence qualified as Persistence
import Agent.CLI.SessionSpec.JsonCodec qualified as JsonCodec
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.Session" do
    Compatibility.spec
    Persistence.spec
    JsonCodec.spec
