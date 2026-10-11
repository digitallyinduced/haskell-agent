module Main (main) where

import Control.Monad (unless)
import Control.Concurrent (threadDelay)
import Data.List (isInfixOf)
import JavaScriptCorePrototype

main :: IO ()
main = withRuntime $ \runtime -> do
    assertEqual "text" (Right ["ok"]) =<< executeCell runtime "text('ok');"
    assertEqual "await" (Right ["{\"answer\":42}"]) =<<
        executeCell runtime "text(await tools.echo({answer: 42}));"
    assertEqual "concurrent" (Right ["[1,2,3]"]) =<<
        executeCell runtime "text(await Promise.all([1,2,3].map(value => tools.echo(value))));"
    assertEqual "unicode" (Right ["Grüße 日本語 😀"]) =<<
        executeCell runtime "text(await tools.echo('Grüße 日本語 😀'));"
    assertEqual "embedded null" (Right ["a\0b"]) =<<
        executeCell runtime "text(await tools.echo('a\\u0000b'));"
    assertEqual "state initial" (Right []) =<<
        executeCell runtime "globalThis.previousCell = 42;"
    assertEqual "fresh state" (Right ["undefined"]) =<<
        executeCell runtime "text(typeof globalThis.previousCell);"
    assertFailure "JavaScript rejection" "refused" =<<
        executeCell runtime "await tools.reject('refused');"
    assertFailure "native rejection" "native refused" =<<
        executeCellWith runtime (\_ -> pure (Left "native refused")) "await tools.echo({});"
    assertEqual "deferred native completion" (Right ["42"]) =<<
        executeCellWith runtime (\payload -> threadDelay 1000 >> pure (Right payload))
            "text(await tools.echo(42));"
    assertFailure "handler exception" "handler failure" =<<
        executeCellWith runtime (\_ -> fail "handler failure")
            "await Promise.all([tools.echo(1), tools.echo(2)]);"
    assertFailure "syntax" "SyntaxError" =<< executeCell runtime "const = ;"
    assertFailure "unresolved promise" "suspended" =<<
        executeCell runtime "await new Promise(() => {});"
    assertEqual "recovery" (Right ["ok"]) =<< executeCell runtime "text('ok');"
    putStrLn "JavaScriptCore prototype: 14 checks passed."

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
    unless (expected == actual) (fail (label <> ": expected " <> show expected <> ", got " <> show actual))

assertFailure :: String -> String -> Either String a -> IO ()
assertFailure label fragment actual = case actual of
    Left message | fragment `isInfixOf` message -> pure ()
    _ -> fail (label <> ": expected failure containing " <> fragment)
