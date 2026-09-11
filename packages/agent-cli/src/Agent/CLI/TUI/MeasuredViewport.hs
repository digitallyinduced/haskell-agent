-- | Viewport composition for fixed-height transcript chunks.
module Agent.CLI.TUI.MeasuredViewport
    ( measuredViewport
    , measuredViewportWithFooter
    ) where

import Brick
import Control.Monad.Reader (local)
import qualified Graphics.Vty as V

-- | Measure fixed-height chunks, resolve scrolling using Brick's viewport,
-- then compose only intersecting results. The reservation is the width of a
-- right-hand vertical scrollbar; horizontal/left scrollbars are not supported.
-- Measurements still visit every chunk (usually a cache hit); only image
-- composition is restricted to the visible range.
measuredViewport :: (Ord n, Show n) => n -> Int -> [Widget n] -> Widget n
measuredViewport name scrollbarWidth =
    measuredViewportWithFooter name scrollbarWidth Nothing

-- | An optional one-row footer receives the current viewport's exclusive
-- bottom row and all measured extents in content coordinates, including those
-- outside the viewport. Reserve its row before resolving scroll requests.
measuredViewportWithFooter
    :: (Ord n, Show n)
    => n
    -> Int
    -> Maybe (Int -> [Extent n] -> Widget n)
    -> [Widget n]
    -> Widget n
measuredViewportWithFooter name scrollbarWidth footer chunks
    | null chunks || any ((== Greedy) . vSize) chunks =
        viewport name Vertical (vBox chunks)
    | otherwise = Widget Greedy Greedy do
        results <- local (\c -> c
            { availHeight = 100000
            , availWidth = max 0 (availWidth c - scrollbarWidth)
            }) $
            measureChunks 100000 chunks
        let offsets = scanl (+) 0 (map (V.imageHeight . image) results)
            positioned = zip offsets results
            totalHeight = sum (map (V.imageHeight . image) results)
            totalWidth = maximum (0 : map (V.imageWidth . image) results)
            requests = concat
                [ visibilityRequests (addResultOffset (Location (0, y)) result)
                | (y, result) <- positioned
                ]
            geometry = emptyResult
                { image = V.backgroundFill totalWidth totalHeight
                , visibilityRequests = requests
                }
        context <- getContext
        let footerHeight = case footer of
                Just _ | availHeight context > 1 -> 1
                _ -> 0
        resolved <- local (\c -> c
            { availHeight = max 0 (availHeight c - footerHeight) }) $
            render $ viewport name Vertical $
                Widget Fixed Fixed (pure geometry)
        finalViewport <- unsafeLookupViewport name
        case finalViewport of
            Nothing -> pure resolved
            Just (VP left top (width, height) _) -> do
                let visibleChunks =
                        [ (y, result)
                        | (y, result) <- positioned
                        -- Include touching neighbors so dynamic borders can
                        -- join across the edge of the visible region.
                        , y <= top + height
                        , y + V.imageHeight (image result) >= top
                        ]
                    firstY = case visibleChunks of
                        [] -> top
                        (y, _) : _ -> y
                combined <- local (\c -> c { availHeight = 100000 }) $
                    render $ vBox
                        [Widget Fixed Fixed (pure result) | (_, result) <- visibleChunks]
                content <- render $
                    vLimit height $ hLimit width $
                    padBottom Max $ padRight Max $
                    translateBy (Location (-left, firstY - top)) $
                    Widget Fixed Fixed (pure combined)
                let viewportResult = content
                        { image = if scrollbarWidth == 0
                            then image content
                            else V.horizJoin (image content)
                                (V.cropLeft scrollbarWidth (image resolved))
                        -- Match viewport's outer-to-inner extent ordering so
                        -- child click targets take precedence over the viewport.
                        , extents = extents resolved <> extents content
                        , visibilityRequests = []
                        }
                case footer of
                    Just drawFooter | footerHeight > 0 ->
                        render $ vBox
                            [ Widget Fixed Fixed (pure viewportResult)
                            , vLimit 1 $ drawFooter (top + height) $
                                concat
                                    [ extents (addResultOffset (Location (0, y)) result)
                                    | (y, result) <- positioned
                                    ]
                            ]
                    _ -> pure viewportResult

-- Match vBox's decreasing height budget, including Brick's release limit.
-- Measuring every child at 100000 would incorrectly expose content which an
-- ordinary viewport truncates when the combined transcript exceeds that cap.
measureChunks :: Int -> [Widget n] -> RenderM n [Result n]
measureChunks _ [] = pure []
measureChunks remaining (chunk : rest) = do
    result <- render (vLimit remaining chunk)
    results <- measureChunks (remaining - V.imageHeight (image result)) rest
    pure (result : results)
