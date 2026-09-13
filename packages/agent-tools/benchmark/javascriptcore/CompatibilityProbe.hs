module Main (main) where

import qualified JavaScriptCorePrototype as JavaScriptCore
import System.Environment (getArgs)

-- Trusted, bounded fixtures only. This prototype is not a sandbox.
-- The caller supervises this process and imposes a wall-clock timeout.
main :: IO ()
main = do
    arguments <- getArgs
    source <- case arguments of
        [value] -> pure value
        _ -> fail "usage: compatibility-probe SOURCE"
    JavaScriptCore.withRuntime $ \runtime -> do
        result <- JavaScriptCore.executeCell runtime source
        case result of
            Left message -> putStrLn "error" >> putStrLn message
            Right output -> putStrLn "ok" >> mapM_ putStrLn output
