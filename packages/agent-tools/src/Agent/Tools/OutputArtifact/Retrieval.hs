{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Storage-independent, bounded retrieval. Cursors count Unicode code points
-- in the original document, never encoded bytes or case-folded characters.
module Agent.Tools.OutputArtifact.Retrieval
    ( readArtifactChunk
    , searchArtifactOccurrences
    ) where

import Data.Aeson (Value, object, (.=), encode)
import qualified Data.ByteString.Lazy as ByteString
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText

readArtifactChunk :: LazyText.Text -> Int -> Int -> Either Text Value
readArtifactChunk content requestedCursor requestedLength
    | requestedCursor < 0 = Left "cursor must not be negative"
    | requestedLength <= 0 = Left "chunk length must be positive"
    | otherwise = Right $
        let cursor = requestedCursor
            count = min 4096 requestedLength
            remaining = LazyText.drop (fromIntegral cursor) content
            (selected, rest) = LazyText.splitAt (fromIntegral count) remaining
            text = LazyText.toStrict selected
            next = if LazyText.null rest
                then Nothing
                else Just (cursor + Text.length text)
        in object
            [ "start" .= cursor
            , "text" .= text
            , "next_cursor" .= next
            , "cursor_unit" .= ("unicode_code_points" :: Text)
            ]

-- | Return non-overlapping occurrences, including occurrences within one long
-- line. The encoded result budget is enforced before adding each occurrence;
-- continuation therefore never silently discards a match.
searchArtifactOccurrences
    :: LazyText.Text
    -> Text
    -> Bool
    -> Int
    -> Int
    -> Int
    -> Either Text Value
searchArtifactOccurrences content pattern insensitive requestedCursor requestedLimit requestedContext
    | Text.null pattern = Left "search pattern must not be empty"
    | Text.length pattern > 1024 = Left "search pattern must not exceed 1024 Unicode code points"
    | requestedCursor < 0 = Left "cursor must not be negative"
    | requestedLimit <= 0 = Left "search limit must be positive"
    | requestedContext < 0 = Left "search context must not be negative"
    | otherwise = Right (collect cursor initialPrevious remaining [] 0 0)
  where
    cursor = max 0 requestedCursor
    limit = min 200 (max 1 requestedLimit)
    context = fromIntegral (min 256 (max 0 requestedContext))
    remaining = LazyText.drop (fromIntegral cursor) content
    initialPrevious = LazyText.toStrict $
        LazyText.take (min context (fromIntegral cursor))
            (LazyText.drop (max 0 (fromIntegral cursor - context)) content)
    needle = LazyText.fromStrict (if insensitive then Text.toCaseFold pattern else pattern)

    finish matches next = object
        [ "matches" .= reverse matches
        , "next_cursor" .= next
        , "cursor_unit" .= ("unicode_code_points" :: Text)
        ]

    collect !position previous suffix matches !count !bytes
        | count >= limit = finish matches
            (if LazyText.null suffix then Nothing else Just position)
        | otherwise = case findOccurrence position previous suffix of
            Nothing -> finish matches (Nothing :: Maybe Int)
            Just (start, beforeText, width, atMatch) ->
                let (matched, rest) = LazyText.splitAt width atMatch
                    before = LazyText.fromStrict beforeText
                    after = LazyText.take context rest
                    end = start + fromIntegral width
                    entry = object
                        [ "start" .= start
                        , "end" .= end
                        , "context_start" .= (start - fromIntegral (LazyText.length before) :: Int)
                        , "text" .= LazyText.toStrict (before <> matched <> after)
                        ]
                    entryBytes = fromIntegral (ByteString.length (encode entry)) :: Int
                in if bytes + entryBytes > 38 * 1024
                    then finish matches (Just start)
                    else collect end
                        (LazyText.toStrict (LazyText.takeEnd context (before <> matched)))
                        rest (entry : matches) (count + 1) (bytes + entryBytes + 1)

    -- Search bounded windows with sufficient overlap for any accepted pattern.
    -- Retain only the requested context when advancing past a window.
    findOccurrence !position !previous suffix
        | LazyText.null suffix = Nothing
        | otherwise =
            let window = LazyText.take (32768 + LazyText.length needle) suffix
            in case locate needle insensitive window of
                Just (distance, width) | distance < 32768 ->
                    let before = Text.copy $ LazyText.toStrict $
                            LazyText.takeEnd context
                                (LazyText.fromStrict previous <> LazyText.take distance suffix)
                    in Just (position + fromIntegral distance, before, width, LazyText.drop distance suffix)
                _ ->
                    let (consumed, rest) = LazyText.splitAt 32768 suffix
                        previous' = Text.copy (LazyText.toStrict (LazyText.takeEnd context consumed))
                    in findOccurrence (position + 32768) previous' rest

locate :: LazyText.Text -> Bool -> LazyText.Text -> Maybe (Int64, Int64)
locate needle False content =
    let (prefix, suffix) = LazyText.breakOn needle content
    in if LazyText.null suffix
        then Nothing
        else Just (LazyText.length prefix, LazyText.length needle)
locate needle True content =
    let folded = LazyText.toCaseFold content
        (prefix, suffix) = LazyText.breakOn needle folded
    in if LazyText.null suffix
        then Nothing
        else
            let foldedStart = LazyText.length prefix
                (start, residual, atStart) = originalStart foldedStart content
                width = originalWidth (residual + LazyText.length needle) atStart
            in if LazyText.length folded == LazyText.length content
                then Just (foldedStart, LazyText.length needle)
                else Just (start, width)

-- A folded match can begin inside an expanded character (e.g. sharp s).
-- Include that entire original character, and similarly round the end up.
originalStart :: Int64 -> LazyText.Text -> (Int64, Int64, LazyText.Text)
originalStart = go 0
  where
    go !position !remaining content
        | remaining <= 0 = (position, 0, content)
        | otherwise = case LazyText.uncons content of
            Nothing -> (position, 0, content)
            Just (character, rest) ->
                let width = foldedWidth character
                in if remaining < width
                    then (position, remaining, content)
                    else go (position + 1) (remaining - width) rest

originalWidth :: Int64 -> LazyText.Text -> Int64
originalWidth = go 0
  where
    go !position !remaining content
        | remaining <= 0 = position
        | otherwise = case LazyText.uncons content of
            Nothing -> position
            Just (character, rest) ->
                go (position + 1) (remaining - foldedWidth character) rest

foldedWidth :: Char -> Int64
foldedWidth = fromIntegral . Text.length . Text.toCaseFold . Text.singleton
