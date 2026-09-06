module Agent.OpenAI.LoopBackendSpec (spec) where

import Test.Hspec (Spec)
import qualified Agent.OpenAI.LoopBackendSpec.CodecSpec as CodecSpec
import qualified Agent.OpenAI.LoopBackendSpec.StateSpec as StateSpec
import qualified Agent.OpenAI.LoopBackendSpec.RecoverySpec as RecoverySpec
import qualified Agent.OpenAI.LoopBackendSpec.TransportSpec as TransportSpec

spec :: Spec
spec = do
    CodecSpec.spec
    StateSpec.spec
    RecoverySpec.spec
    TransportSpec.spec
