-- | Pure status and thinking-block formatting for the linear renderer.
module Agent.CLI.Render.Status
    ( formatActivityLine
    , formatElapsed
    , formatThinkingBlock
    , formatTurnStatus
    , thinkingMaxWidth
    , wrapThinkingLines
    ) where

import Agent.CLI.Status (formatTokensPerSecond)
import Agent.CLI.Style
    ( glyphCancel
    , glyphErr
    , glyphOk
    , glyphThink
    , glyphToolAccent
    , roleError
    , roleMuted
    , roleSuccess
    , roleThinking
    )
import Data.Text (Text)
import qualified Data.Text as Text

thinkingMaxWidth :: Int
thinkingMaxWidth = 120

formatThinkingBlock :: Bool -> Bool -> Double -> Text -> Text
formatThinkingBlock color streaming elapsed raw =
    let header =
            if streaming
                then roleThinking color (glyphThink <> "Thinking…")
                    <> roleMuted color ("  " <> formatElapsed elapsed)
                else
                    roleThinking color (glyphThink <> "Thought")
                        <> roleMuted color (" for " <> formatElapsed elapsed)
        wrapped = wrapThinkingLines thinkingMaxWidth (Text.strip raw)
        preview
            | streaming = take thinkingPreviewLines wrapped
            | otherwise = wrapped
        hidden = length wrapped - length preview
        more
            | streaming && hidden > 0 =
                [roleMuted color ("  … " <> Text.pack (show hidden) <> " more")]
            | otherwise = []
        body =
            map (\line -> roleMuted color (glyphToolAccent <> line)) preview
                <> more
    in Text.intercalate "\n" (header : body)

thinkingPreviewLines :: Int
thinkingPreviewLines = 3

wrapThinkingLines :: Int -> Text -> [Text]
wrapThinkingLines width text
    | Text.null text = []
    | otherwise =
        concatMap (wrapOne (max 1 width)) (Text.splitOn "\n" text)

wrapOne :: Int -> Text -> [Text]
wrapOne width line
    | Text.null line = [""]
    | Text.length line <= width = [line]
    | otherwise = go (Text.words line) ""
  where
    go [] acc
        | Text.null acc = []
        | otherwise = [acc]
    go (word : rest) acc
        | Text.null acc && Text.length word > width =
            let (chunk, leftover) = Text.splitAt width word
            in chunk : go (leftover : rest) ""
        | Text.null acc = go rest word
        | Text.length acc + 1 + Text.length word <= width =
            go rest (acc <> " " <> word)
        | otherwise = acc : go (word : rest) ""

formatActivityLine :: Bool -> Text -> Text -> Double -> Maybe Double -> Text
formatActivityLine color glyph activity seconds rate =
    roleThinking color (glyph <> " " <> activity)
        <> roleMuted color ("  " <> formatElapsed seconds <> rateSuffix)
  where
    rateSuffix = case rate of
        Just value -> " · " <> formatTokensPerSecond value
        Nothing -> ""

formatElapsed :: Double -> Text
formatElapsed seconds
    | seconds < 0 = "0.0s"
    | seconds < 60 =
        let tenths = round (seconds * 10) :: Int
            whole = tenths `div` 10
            frac = tenths `mod` 10
        in Text.pack (show whole <> "." <> show frac <> "s")
    | otherwise =
        let total = round seconds :: Int
            m = total `div` 60
            s = total `mod` 60
        in Text.pack (show m <> "m" <> pad2 s <> "s")
  where
    pad2 n
        | n < 10 = "0" <> show n
        | otherwise = show n

formatTurnStatus :: Bool -> Text -> Text -> Text
formatTurnStatus color outcome detail =
    let mark
            | outcome == "ok" = roleSuccess color glyphOk
            | outcome == "cancelled" = roleMuted color glyphCancel
            | otherwise = roleError color glyphErr
        body
            | Text.null detail = outcome
            | otherwise = outcome <> " · " <> detail
    in mark <> roleMuted color body
