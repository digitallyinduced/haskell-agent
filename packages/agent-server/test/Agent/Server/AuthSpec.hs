module Agent.Server.AuthSpec (spec) where

import Agent.Server.Auth
import Data.Set qualified as Set
import Network.HTTP.Types (methodOptions)
import Network.Wai
    ( defaultRequest
    , queryString
    , requestHeaders
    , requestMethod
    )
import Test.Hspec

spec :: Spec
spec = describe "HTTP boundary authentication" do
    let loopback = AuthConfig
            { authMode =
                LoopbackHostAuth
                    (Set.fromList ["127.0.0.1:4096"])
            , authCorsOrigins =
                Set.fromList ["https://app.example"]
            }
        remote = AuthConfig
            { authMode = BearerTokenAuth "correct-token"
            , authCorsOrigins = Set.empty
            }

    it "accepts only an explicitly valid loopback Host" do
        authorizeRequest
            loopback
            defaultRequest
                { requestHeaders =
                    [("Host", "127.0.0.1:4096")]
                }
            `shouldSatisfy` isRight
        authorizeRequest loopback defaultRequest
            `shouldSatisfy` isLeft
        authorizeRequest
            loopback
            defaultRequest
                { requestHeaders =
                    [("Host", "attacker.example")]
                }
            `shouldSatisfy` isLeft

    it "checks bearer tokens without accepting query alternatives" do
        authorizeRequest
            remote
            defaultRequest
                { requestHeaders =
                    [("Authorization", "Bearer correct-token")]
                }
            `shouldSatisfy` isRight
        authorizeRequest
            remote
            defaultRequest
                { queryString =
                    [("token", Just "correct-token")]
                }
            `shouldSatisfy` isLeft

    it "allows only explicit Origins and supports credential-free preflight" do
        let request = defaultRequest
                { requestMethod = methodOptions
                , requestHeaders =
                    [ ("Host", "127.0.0.1:4096")
                    , ("Origin", "https://app.example")
                    ]
                }
        authorizePreflight loopback request
            `shouldSatisfy` isRight
        authorizePreflight
            loopback
            request
                { requestHeaders =
                    [ ("Host", "127.0.0.1:4096")
                    , ("Origin", "https://evil.example")
                    ]
                }
            `shouldSatisfy` isLeft

isLeft :: Either left right -> Bool
isLeft = \case
    Left _ -> True
    Right _ -> False

isRight :: Either left right -> Bool
isRight = not . isLeft
