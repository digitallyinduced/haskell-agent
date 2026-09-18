module Main (main) where

import CodeMode

main :: IO ()
main = runCell $ do
    response <- callTool "fixture.numbers"
        (JsonObject [("count", JsonNumber "5")])
    case response of
        JsonArray values -> do
            numbers <- mapM requireInteger values
            emit (JsonNumber (show (sum numbers)))
        _ -> ioError (userError "fixture.numbers did not return an array")

requireInteger :: Json -> IO Integer
requireInteger (JsonNumber value) =
    case reads value of
        [(number, "")] -> pure number
        _ -> ioError (userError "fixture.numbers returned a non-integer")
requireInteger _ = ioError (userError "fixture.numbers returned a non-number")
