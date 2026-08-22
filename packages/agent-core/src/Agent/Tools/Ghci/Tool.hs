-- | Provider-neutral @run_ghci@ tool adapter.
module Agent.Tools.Ghci.Tool
    ( runGhciTool
    ) where

import Agent.ToolArgs
    ( objectArgs
    , optInt
    , optText
    , reqText
    )
import Agent.ToolDSL
    ( PropertySchema(..)
    , PropertyType(..)
    )
import Agent.ToolDispatch (ToolCall(..), typedTool)
import Agent.Tools.Ghci.Classify (GhciClass(..))
import Agent.Tools.Ghci.Runtime
    ( GhciOutcome(..)
    , GhciResult(..)
    , GhciSession
    , classifyGhci
    , evalGhci
    )
import Agent.Tools.Types
    ( AppTool
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , jsonAppToolWithExecution
    )
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON(..), Object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

data GhciArgs = GhciArgs
    { expression :: Text
    , timeout :: Maybe Int
    , description :: Text
    }

instance FromJSON GhciArgs where
    parseJSON = objectArgs \object -> GhciArgs
        <$> reqText object "expression"
        <*> optionalTimeout object
        <*> reqText object "description"

optionalTimeout :: Object -> Parser (Maybe Int)
optionalTimeout object = do
    fromInt <- optInt object "timeout"
    fromText <- optText object "timeout"
    pure (fromInt <|> (fromText >>= readTimeout))

readTimeout :: Text -> Maybe Int
readTimeout text =
    case reads (Text.unpack text) of
        [(n, "")] -> Just n
        _ -> Nothing

runGhciTool :: GhciSession -> AppTool
runGhciTool session =
    jsonAppToolWithExecution "run_ghci" ghciDescription
        [ PropertySchema "expression" PropertyString True $ Just
            "Haskell expression, statement, or GHCi :command to evaluate."
        , PropertySchema "timeout" PropertyInteger False $ Just
            "Optional timeout in milliseconds (max 300000). Default: 30000."
        , PropertySchema "description" PropertyString True $ Just
            "One sentence explanation as to why this evaluation is needed."
        ]
        (ClassifyReadOnly (isGhciReadOnlyCall session))
        TurnSequential
        (typedTool "run_ghci" (runGhci session))

ghciDescription :: Text
ghciDescription =
    "Evaluate Haskell in a persistent GHCi session for this agent.\n\
    \Bindings and loaded modules persist across calls.\n\
    \Pure expressions auto-approve; IO and side-effecting GHCi commands need approval.\n\
    \Prefer this over shell tools for calculations, type exploration, and small Haskell scripts.\n\
    \The session starts with GHC2021 plus BlockArguments, OverloadedStrings, \
    \OverloadedRecordDot, DuplicateRecordFields, NoFieldSelectors, LambdaCase, \
    \and RecordWildCards — LANGUAGE pragmas are not required for those."

isGhciReadOnlyCall :: GhciSession -> ToolCall -> IO Bool
isGhciReadOnlyCall session call =
    case decodeExpression call.arguments of
        Nothing -> pure False
        Just expression -> do
            classification <- classifyGhci session expression
            pure (classification == GhciPure)

decodeExpression :: Text -> Maybe Text
decodeExpression arguments =
    case Aeson.decodeStrict (TextEncoding.encodeUtf8 arguments) of
        Just (Aeson.Object object) ->
            case KeyMap.lookup (Key.fromText "expression") object of
                Just (Aeson.String value) -> Just value
                _ -> Nothing
        _ -> Nothing

runGhci :: GhciSession -> GhciArgs -> IO (Either Text Text)
runGhci session args
    | Text.null (Text.strip args.description) =
        pure (Left "Missing parameter: description")
    | Text.null (Text.strip args.expression) =
        pure (Left "Missing parameter: expression")
    | otherwise = do
        let timeoutMs = normalizeTimeout (fromMaybe 30000 args.timeout)
        result <- evalGhci session args.expression timeoutMs
        let classLabel = case result.ghciClass of
                GhciPure -> "pure"
                GhciEffectful -> "io"
            status = case result.ghciOutcome of
                GhciCompleted
                    | result.ghciOk -> "ok"
                    | otherwise -> "error"
                GhciTimedOut
                    | result.ghciRestarted ->
                        "timeout (session restarted; bindings lost)"
                    | otherwise -> "timeout"
                GhciCancelled
                    | result.ghciRestarted ->
                        "cancelled (session restarted; bindings lost)"
                    | otherwise -> "cancelled"
                GhciProcessFailed
                    | result.ghciRestarted ->
                        "process failed (session restarted; bindings lost)"
                    | otherwise -> "process failed"
            truncated =
                if result.ghciTruncated then "\noutput: truncated" else ""
            body
                | Text.null result.ghciOutput = ""
                | otherwise = "\n" <> result.ghciOutput
        pure $ Right $
            "class: " <> classLabel
                <> "\n"
                <> status
                <> truncated
                <> body

normalizeTimeout :: Int -> Int
normalizeTimeout = min 300000 . max 1
