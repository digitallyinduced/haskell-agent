module Agent.XAI.ImageBudgetSpec (spec) where

import Agent.Json (rawJsonFromEncoding)
import Agent.Responses.Types
import Agent.XAI.ImageBudget
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Test.Hspec

spec :: Spec
spec = do
    describe "imageBudgetLimits" do
        it "uses the 50 MiB proxy cap, with a 3 MiB trigger headroom" do
            imageBudgetLimits Nothing
                `shouldBe` (47 * 1024 * 1024, 25 * 1024 * 1024)
            defaultMaxRequestBytes `shouldBe` 50 * 1024 * 1024
            imageBudgetHeadroomBytes `shouldBe` 3 * 1024 * 1024

        it "scales an explicit cap and raises a cap that cannot hysteresis" do
            imageBudgetLimits (Just 30_000_000)
                `shouldBe`
                    ( 30_000_000 - imageBudgetHeadroomBytes
                    , 15_000_000
                    )
            imageBudgetLimits (Just 1)
                `shouldBe` imageBudgetLimits (Just minimumMaxRequestBytes)

    describe "applyImageBudgetWithLimits" do
        it "keeps every image below the trigger" do
            let request = imageRequest ["aaaa", "bbbb"]
            applyImageBudgetWithLimits
                (requestJsonBytes request + 1)
                0
                request
                `shouldBe` request

        it "keeps every image when the body is already under the reclaim mark" do
            let request = imageRequest ["aaaa", "bbbb"]
                bytes = requestJsonBytes request
            applyImageBudgetWithLimits bytes bytes request `shouldBe` request

        it "evicts the oldest image once the body reaches the trigger" do
            let request = imageRequest ["a", "b", "c"]
                bytes = requestJsonBytes request
                budgeted = applyImageBudgetWithLimits bytes (bytes - 1) request
                encoded = encodeText budgeted
            requestJsonBytes budgeted `shouldSatisfy` (<= bytes - 1)
            Text.isInfixOf (payload "a") encoded `shouldBe` False
            Text.isInfixOf imageBudgetPlaceholder encoded `shouldBe` True
            Text.isInfixOf (payload "b") encoded `shouldBe` True
            Text.isInfixOf (payload "c") encoded `shouldBe` True

        it "removes the oldest tool image and records that it is gone" do
            let request =
                    requestWith
                        [ toolImage "t"
                        , userImage "u"
                        ]
                bytes = requestJsonBytes request
                budgeted = applyImageBudgetWithLimits bytes (bytes - 1) request
                encoded = encodeText budgeted
            Text.isInfixOf (payload "t") encoded `shouldBe` False
            Text.isInfixOf toolImageBudgetNote encoded `shouldBe` True
            Text.isInfixOf (payload "u") encoded `shouldBe` True

        it "stops once the encoded body is under the reclaim mark" do
            let request = imageRequest ["a", "b"]
                bytes = requestJsonBytes request
                budgeted = applyImageBudgetWithLimits bytes (bytes - 1) request
            requestJsonBytes budgeted `shouldSatisfy` (< bytes)
            requestJsonBytes budgeted `shouldSatisfy` (<= bytes - 1)

    describe "omitInlineImages" do
        it "removes every inline image and does not retry an image-free body" do
            let request = requestWith [toolImage "tttt", userImage "uuuu"]
            case omitInlineImages request of
                Nothing -> expectationFailure "expected images to be omitted"
                Just (stripped, count) -> do
                    count `shouldBe` 2
                    let encoded = encodeText stripped
                    Text.isInfixOf (payload "tttt") encoded `shouldBe` False
                    Text.isInfixOf (payload "uuuu") encoded `shouldBe` False
                    Text.isInfixOf imageStripPlaceholder encoded `shouldBe` True
                    omitInlineImages stripped `shouldBe` Nothing

        it "leaves a request without images untouched" do
            omitInlineImages (imageRequest []) `shouldBe` Nothing

payload :: Text -> Text
payload marker = "data:image/png;base64," <> Text.replicate 2000 marker

imageRequest :: [Text] -> ResponseCreateParams
imageRequest = requestWith . map userImage

requestWith :: [ResponseItem] -> ResponseCreateParams
requestWith items =
    defaultResponseCreateParams
        { model = Just "grok-4.7"
        , input = Just (ResponseInputItems items)
        }

userImage :: Text -> ResponseItem
userImage marker =
    MessageItem ResponseMessage
        { messageId = Nothing
        , content = MessageContentParts
            [ InputImagePart
                { detail = Nothing
                , fileId = Nothing
                , imageUrl = Just (payload marker)
                , promptCacheBreakpoint = Nothing
                }
            , InputTextPart
                { text = "caption"
                , promptCacheBreakpoint = Nothing
                }
            ]
        , role = RoleUser
        , status = Nothing
        , phase = Nothing
        , passthrough = Nothing
        }

toolImage :: Text -> ResponseItem
toolImage marker =
    FunctionCallOutputItem FunctionCallOutput
        { localOutcome = Nothing
        , itemId = Nothing
        , callId = "call-" <> marker
        , name = Nothing
        , namespace = Nothing
        , provider = Nothing
        , output =
            rawJsonFromEncoding $ Aeson.toEncoding
                [ InputImagePart
                    { detail = Just "high"
                    , fileId = Nothing
                    , imageUrl = Just (payload marker)
                    , promptCacheBreakpoint = Nothing
                    }
                , InputTextPart
                    { text = "Viewed image"
                    , promptCacheBreakpoint = Nothing
                    }
                ]
        , status = Nothing
        , async = Nothing
        }

encodeText :: ResponseCreateParams -> Text
encodeText = Text.decodeUtf8 . LBS.toStrict . Aeson.encode
