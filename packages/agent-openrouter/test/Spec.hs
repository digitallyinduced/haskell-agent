module Main (main) where

import Test.Hspec (hspec)

import qualified Agent.OpenRouter.ClientSpec as ClientSpec
import qualified Agent.OpenRouter.CredentialSpec as CredentialSpec
import qualified Agent.OpenRouter.ErrorSpec as ErrorSpec
import qualified Agent.OpenRouter.FunctionalSpec as FunctionalSpec
import qualified Agent.OpenRouter.ModelsSpec as ModelsSpec
import qualified Agent.OpenRouter.RequestSpec as RequestSpec
import qualified Agent.OpenRouter.UsageSpec as UsageSpec

main :: IO ()
main = hspec do
    RequestSpec.spec
    ErrorSpec.spec
    CredentialSpec.spec
    ClientSpec.spec
    ModelsSpec.spec
    FunctionalSpec.spec
    UsageSpec.spec
