module Main (main) where

import Test.Hspec (hspec)
import qualified Agent.Gemini.AuthSpec as AuthSpec
import qualified Agent.Gemini.ClientSpec as ClientSpec
import qualified Agent.Gemini.CredentialSpec as CredentialSpec
import qualified Agent.Gemini.ErrorSpec as ErrorSpec
import qualified Agent.Gemini.RequestSpec as RequestSpec
import qualified Agent.Gemini.ResponseSpec as ResponseSpec
import qualified Agent.Gemini.StreamSpec as StreamSpec

main :: IO ()
main = hspec do
    AuthSpec.spec
    ClientSpec.spec
    CredentialSpec.spec
    ErrorSpec.spec
    RequestSpec.spec
    ResponseSpec.spec
    StreamSpec.spec
