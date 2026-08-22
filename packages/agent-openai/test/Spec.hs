module Main (main) where

import Test.Hspec (hspec)

import qualified Agent.OpenAI.AuthSpec as AuthSpec
import qualified Agent.OpenAI.ClientSpec as ClientSpec
import qualified Agent.OpenAI.CredentialSpec as CredentialSpec
import qualified Agent.OpenAI.ErrorSpec as ErrorSpec
import qualified Agent.OpenAI.FunctionalSpec as FunctionalSpec
import qualified Agent.OpenAI.LoginSpec as LoginSpec
import qualified Agent.OpenAI.LoopBackendSpec as LoopBackendSpec
import qualified Agent.OpenAI.CompactionSpec as CompactionSpec
import qualified Agent.OpenAI.ResponsesSpec as ResponsesSpec
import qualified Agent.OpenAI.ToolDSLSpec as ToolDSLSpec
import qualified Agent.OpenAI.UsageSpec as UsageSpec
import qualified Agent.OpenAI.WebSocketClientSpec as WebSocketClientSpec

main :: IO ()
main = hspec do
    AuthSpec.spec
    ClientSpec.spec
    CredentialSpec.spec
    ErrorSpec.spec
    FunctionalSpec.spec
    LoginSpec.spec
    LoopBackendSpec.spec
    CompactionSpec.spec
    ResponsesSpec.spec
    ToolDSLSpec.spec
    UsageSpec.spec
    WebSocketClientSpec.spec
