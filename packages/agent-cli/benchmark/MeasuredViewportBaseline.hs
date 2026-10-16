{-# OPTIONS_GHC -O1 #-}

-- | Preserve the viewport implementation before deferred metadata clipping.
-- Match the production library's optimization level rather than the benchmark
-- driver's -O2 setting.
module MeasuredViewportBaseline (measuredViewportWithFooterBaseline) where

import Brick
import Control.Monad.Reader (local)
import qualified Graphics.Vty as V

measuredViewportWithFooterBaseline
    :: (Ord n, Show n)
    => n
    -> Int
    -> Maybe (Int -> [Extent n] -> Widget n)
    -> [Widget n]
    -> Widget n
measuredViewportWithFooterBaseline name scrollbarWidth footer chunks
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

measureChunks :: Int -> [Widget n] -> RenderM n [Result n]
measureChunks _ [] = pure []
measureChunks remaining (chunk : rest) = do
    result <- render (vLimit remaining chunk)
    results <- measureChunks (remaining - V.imageHeight (image result)) rest
    pure (result : results)
