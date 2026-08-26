{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import qualified Agent.Json.Decoder as Decoder
import qualified Agent.Json.Decoder.Hermes as Hermes
import Control.Concurrent.Async (concurrently)
import Data.Either (isLeft)
import Data.Text (Text)
import Test.Hspec

data Person = Person
    { name :: !Text
    , age :: !Int
    }
    deriving stock (Eq, Show)

data PersonState = PersonState
    { stateName :: !(Maybe Text)
    , stateAge :: !(Maybe Int)
    }

personDecoder :: Decoder.Decoder Person
personDecoder =
    Decoder.object
        (PersonState Nothing Nothing)
        [ Decoder.field "name" Decoder.text \value state ->
            Right state { stateName = Just value }
        , Decoder.field "age" Decoder.int \value state ->
            Right state { stateAge = Just value }
        ]
        (Decoder.unknownField Decoder.skip
            \_ () state -> Right state)
        \state ->
            Person
                <$> required "name" state.stateName
                <*> required "age" state.stateAge
  where
    required label =
        maybe (Left ("missing " <> label)) Right

main :: IO ()
main = hspec do
    describe "Hermes direct backend" do
        it "matches the portable backend while reusing a session" do
            let inputs =
                    [ "{\"name\":\"Ada\",\"age\":37,\"vendor\":{\"x\":1}}"
                    , "{\"age\":85,\"name\":\"Grace\"}"
                    ]
            Hermes.withDecoderSession \session ->
                mapM_
                    (\input -> do
                        actual <- Hermes.decodeIO session personDecoder input
                        actual
                            `shouldBe`
                                Decoder.decode personDecoder input)
                    inputs

        it "fully validates skipped unknown values" do
            Hermes.withDecoderSession \session -> do
                result <-
                    Hermes.decodeIO session personDecoder
                        "{\"name\":\"Ada\",\"age\":37,\"vendor\":[1,]}"
                result `shouldSatisfy` isLeft

        it "rejects trailing input after a valid object" do
            Hermes.withDecoderSession \session -> do
                result <-
                    Hermes.decodeIO session personDecoder
                        "{\"name\":\"Ada\",\"age\":37} trailing"
                result `shouldSatisfy` isLeft

        it "uses the portable direct path for scalar roots" do
            Hermes.withDecoderSession \session ->
                Hermes.decodeIO session Decoder.text "\"hello\""
                    `shouldReturn` Right "hello"

        it "decodes concurrent streams with independent sessions" do
            let decode input =
                    Hermes.withDecoderSession \session ->
                        Hermes.decodeIO session personDecoder input
            (first, second) <- concurrently
                (decode "{\"name\":\"Ada\",\"age\":37}")
                (decode "{\"name\":\"Grace\",\"age\":85}")
            first `shouldBe` Right (Person "Ada" 37)
            second `shouldBe` Right (Person "Grace" 85)
