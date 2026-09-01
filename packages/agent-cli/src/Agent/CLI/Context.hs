{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pure formatting and conservative accounting for the /context command.
module Agent.CLI.Context
    ( contextUsageTokens
    , formatContextReport
    ) where

import Agent.CLI.Compaction
    ( OccupancyKind(..)
    , OccupancySnapshot(..)
    )
import Agent.CLI.Request
    ( setRequestInstructions
    , setRequestInstructionsAndTools
    )
import Agent.OpenAI.Compaction (estimateRequestTokensWithItems)
import Agent.Responses.Types
    ( ResponseCreateParams(..)
    , ResponseItem
    )
import Data.Text (Text)
import qualified Data.Text as Text

-- | Prefer a provider-reported occupancy while it still describes the
-- committed history. Otherwise estimate the complete serialized request.
contextUsageTokens
    :: Maybe OccupancySnapshot
    -> ResponseCreateParams
    -> [ResponseItem]
    -> Int
contextUsageTokens occupancy params history =
    case occupancy of
        Just snapshot
            | validReportedOccupancy history snapshot ->
                snapshot.occupancyTokens
        _ -> estimateRequestTokensWithItems params history

-- | Format a context snapshot. Provider-reported occupancy is used only when
-- it still describes the supplied history; otherwise the complete serialized
-- request is estimated locally.
formatContextReport
    :: Text
    -> Maybe Int
    -> Maybe OccupancySnapshot
    -> ResponseCreateParams
    -> [ResponseItem]
    -> [Text]
    -> Text
formatContextReport model contextWindow occupancy params history activeTools =
    Text.unlines
        ( [ "Model: " <> model
          , "Used: " <> showInt used <> " tokens (" <> source <> ")"
          ]
            <> maybe unknownWindowLines windowLines contextWindow
            <> [ "Estimated breakdown:"
               , "  Instructions: " <> showInt instructionsTokens
               , "  Messages: " <> showInt messageTokens
               , "  Tool schemas: " <> showInt toolTokens
               , "  Other overhead: " <> showInt otherTokens
               , "  Total: " <> showInt estimatedTotal
               , "Active tools: " <> showInt (length activeTools)
               ]
            <> if null activeTools
                then []
                else ["  " <> Text.intercalate ", " activeTools]
        )
  where
    estimatedTotal = estimateRequestTokensWithItems params history
    used = contextUsageTokens occupancy params history
    source = case occupancy of
        Just snapshot
            | validReportedOccupancy history snapshot ->
                "provider reported"
        _ -> "estimated"

    -- Use the dialect-aware setters because Responses Lite stores instructions
    -- and tool schemas in an input prefix instead of the ordinary fields.
    noInstructions = setRequestInstructions "" params
    noInstructionsNoTools =
        setRequestInstructionsAndTools "" (Just []) params
    withoutHistory = estimateRequestTokensWithItems params []
    messageTokens = max 0 (estimatedTotal - withoutHistory)
    instructionsTokens =
        max 0
            ( withoutHistory
                - estimateRequestTokensWithItems noInstructions []
            )
    toolTokens =
        max 0
            ( estimateRequestTokensWithItems noInstructions []
                - estimateRequestTokensWithItems noInstructionsNoTools []
            )
    otherTokens =
        estimateRequestTokensWithItems noInstructionsNoTools []

    showInt = Text.pack . show
    unknownWindowLines =
        [ "Window: unknown"
        , "Usage: [unknown]"
        , "Free: unknown"
        ]
    windowLines window
        | window <= 0 = unknownWindowLines
        | otherwise =
            let percentage :: Double
                percentage =
                    fromIntegral used * 100 / fromIntegral window
                free = max 0 (window - used)
            in [ "Window: "
                    <> showInt used
                    <> " / "
                    <> showInt window
                    <> " tokens ("
                    <> Text.pack (show (round1 percentage))
                    <> "%)"
               , "Usage: " <> progressBar used window
               , "Free: " <> showInt free <> " tokens"
               ]
    round1 value =
        fromIntegral (round (value * 10) :: Integer) / 10 :: Double

progressBar :: Int -> Int -> Text
progressBar used window
    | window <= 0 = "[unknown]"
    | otherwise =
        let width = 20
            scaled = max 0 used * width
            filled =
                min width
                    ((scaled + window `div` 2) `div` window)
        in "[" <> Text.replicate filled "#" <> Text.replicate (width - filled) "-" <> "]"

validReportedOccupancy
    :: [ResponseItem]
    -> OccupancySnapshot
    -> Bool
validReportedOccupancy history snapshot =
    snapshot.occupancyKind == ReportedOccupancy
        && snapshot.occupancyTokens > 0
        && snapshot.occupancyLength == length history
