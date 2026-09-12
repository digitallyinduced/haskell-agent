module Main (main) where

import CodeMode.Json
-- MicroHs provides this base module, but not safe-exceptions.
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM_, unless)

main :: IO ()
main = do
    forM_ examples $ \value ->
        assert ("round trip: " ++ show value)
            (decodeJson (encodeJson value) == Right value)
    forM_ invalidDocuments $ \document ->
        assert ("reject: " ++ show document)
            (case decodeJson document of Left _ -> True; Right _ -> False)
    assert "surrogate pair"
        (decodeJson "\"\\uD83D\\uDE00\"" == Right (JsonString "\x1f600"))
    assert "all short escapes"
        (decodeJson "\"\\\"\\\\\\/\\b\\f\\n\\r\\t\""
            == Right (JsonString "\"\\/\b\f\n\r\t"))
    assert "whitespace"
        (decodeJson " \r\n\t { \"value\" : null } \r\n"
            == Right (JsonObject [("value", JsonNull)]))
    assert "preserve number precision"
        (decodeJson "123456789012345678901234567890.123456789e+123"
            == Right (JsonNumber "123456789012345678901234567890.123456789e+123"))
    assert "nesting limit"
        (case decodeJson (replicate 129 '[' ++ "0" ++ replicate 129 ']') of
            Left _ -> True
            Right _ -> False)
    forM_ invalidValues $ \(label, value) -> do
        result <- try (evaluate (length (encodeJson value)))
            :: IO (Either SomeException Int)
        assert ("encoding rejects: " ++ label)
            (case result of Left _ -> True; Right _ -> False)
    putStrLn "JSON codec tests passed"

assert :: String -> Bool -> IO ()
assert label condition =
    unless condition (ioError (userError ("assertion failed: " ++ label)))

examples :: [Json]
examples =
    [ JsonNull
    , JsonBool True
    , JsonBool False
    , JsonNumber "0"
    , JsonNumber "-0"
    , JsonNumber "-12.34e-56"
    , JsonString ""
    , JsonString "\"\\\n\r\t\b\f\x00\x1f"
    , JsonString "Unicode: \xe9 \x6f22 \x1f600"
    , JsonArray [JsonNumber "42", JsonNull, JsonBool False]
    , JsonObject [("nested", JsonObject [("value", JsonArray [])])]
    ]

invalidValues :: [(String, Json)]
invalidValues =
    [ ("non-finite number", JsonNumber "NaN")
    , ("non-number suffix", JsonNumber "12 trailing")
    , ("surrogate", JsonString "\xd800")
    , ("duplicate keys", JsonObject [("value", JsonNull), ("value", JsonNull)])
    , ("deferred number failure", JsonObject
        [("prefix", JsonString "before"), ("invalid", JsonNumber "NaN")])
    , ("deferred character failure", JsonString ['a', error "deferred character"])
    ]

invalidDocuments :: [String]
invalidDocuments =
    [ ""
    , "01"
    , "+1"
    , ".1"
    , "1."
    , "1e"
    , "1e+"
    , "--1"
    , "NaN"
    , "Infinity"
    , "[1,]"
    , "{\"value\":1,}"
    , "{\"value\":1,\"value\":2}"
    , "\"\\x41\""
    , "\"\\uD800\""
    , "\"\\uDC00\""
    , "\"\\uD800\\u0041\""
    , "\"\\u12xz\""
    , "\"raw\nnewline\""
    , "null false"
    , "\xa0null"
    ]
