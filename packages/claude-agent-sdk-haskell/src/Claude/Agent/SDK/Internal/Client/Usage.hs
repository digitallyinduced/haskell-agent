module Claude.Agent.SDK.Internal.Client.Usage
    ( UsageAccounting(..)
    , cumulativeUsage
    , reconcileCumulativeUsage
    ) where

import Claude.Agent.SDK.Types
    ( ModelUsage
    , Usage(..)
    , addUsage
    , emptyUsage
    , modelUsageToUsage
    )
import qualified Data.Map.Strict as Map
import Data.Text (Text)

data UsageAccounting = UsageAccounting
    { usageCumulativeBaseline :: !(Maybe Usage)
    , usagePendingFallback :: !Usage
    }

data UsageComponents = UsageComponents
    { usageUncachedInput :: !Int
    , usageCachedInput :: !Int
    , usageOutput :: !Int
    }

cumulativeUsage :: Map.Map Text ModelUsage -> Maybe Usage
cumulativeUsage modelUsage =
    case Map.elems modelUsage of
        [] -> Nothing
        entries ->
            Just $
                foldl
                    addUsage
                    emptyUsage
                    (map modelUsageToUsage entries)

reconcileCumulativeUsage
    :: UsageAccounting
    -> Usage
    -> (Usage, Usage)
reconcileCumulativeUsage accounting current =
    case accounting.usageCumulativeBaseline of
        Just previous
            | not (usageIsMonotonic previous current) ->
                (current, emptyUsage)
        previous ->
            subtractReportedFallback
                accounting.usagePendingFallback
                (maybe current (`usageDelta` current) previous)

usageDelta :: Usage -> Usage -> Usage
usageDelta previous current
    | usageIsMonotonic previous current =
        fromUsageComponents $
            subtractUsageComponents
                (toUsageComponents previous)
                (toUsageComponents current)
    | otherwise =
        current

usageIsMonotonic :: Usage -> Usage -> Bool
usageIsMonotonic previous current =
    let previousComponents = toUsageComponents previous
        currentComponents = toUsageComponents current
    in
        currentComponents.usageUncachedInput
            >= previousComponents.usageUncachedInput
            && currentComponents.usageCachedInput
                >= previousComponents.usageCachedInput
            && currentComponents.usageOutput
                >= previousComponents.usageOutput

subtractReportedFallback
    :: Usage
    -> Usage
    -> (Usage, Usage)
subtractReportedFallback pending gross =
    ( fromUsageComponents $
        subtractUsageComponents
            (toUsageComponents pending)
            (toUsageComponents gross)
    , fromUsageComponents $
        subtractUsageComponents
            (toUsageComponents gross)
            (toUsageComponents pending)
    )

toUsageComponents :: Usage -> UsageComponents
toUsageComponents usage =
    let cached =
            max 0 (min usage.inputTokens usage.cachedTokens)
    in UsageComponents
        { usageUncachedInput =
            max 0 (usage.inputTokens - cached)
        , usageCachedInput = cached
        , usageOutput = max 0 usage.outputTokens
        }

fromUsageComponents :: UsageComponents -> Usage
fromUsageComponents components =
    Usage
        { inputTokens =
            components.usageUncachedInput
                + components.usageCachedInput
        , outputTokens = components.usageOutput
        , cachedTokens = components.usageCachedInput
        }

subtractUsageComponents
    :: UsageComponents
    -> UsageComponents
    -> UsageComponents
subtractUsageComponents subtrahend minuend =
    UsageComponents
        { usageUncachedInput =
            max 0
                ( minuend.usageUncachedInput
                    - subtrahend.usageUncachedInput
                )
        , usageCachedInput =
            max 0
                ( minuend.usageCachedInput
                    - subtrahend.usageCachedInput
                )
        , usageOutput =
            max 0
                ( minuend.usageOutput
                    - subtrahend.usageOutput
                )
        }
