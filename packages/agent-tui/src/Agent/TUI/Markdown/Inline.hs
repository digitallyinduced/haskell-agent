-- | Shared, composable inline Markdown parsing.
module Agent.TUI.Markdown.Inline
    ( Inline(..)
    , inlinePlainText
    , parseInline
    ) where

import Control.Applicative ((<|>))
import Data.Char
    ( isAlphaNum
    , isAscii
    , isControl
    , isSpace
    )
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text

data Inline
    = InlineText !Text
    | InlineCode !Text
    | InlineStrong ![Inline]
    | InlineEmphasis ![Inline]
    | InlineLink !Text ![Inline]
    deriving (Eq, Show)

-- | Parse line-local inline Markdown. Malformed constructs remain literal.
parseInline :: Text -> [Inline]
parseInline =
    coalesceText
        . concat
        . List.intersperse [InlineText "\n"]
        . map (parseSequence Nothing Nothing)
        . Text.splitOn "\n"

-- | Text displayed by either renderer, including the visible destination
-- appended to links whose label differs from their destination.
inlinePlainText :: [Inline] -> Text
inlinePlainText = foldMap inlineText
  where
    inlineText = \case
        InlineText text -> text
        InlineCode text -> text
        InlineStrong children -> inlinePlainText children
        InlineEmphasis children -> inlinePlainText children
        InlineLink url children ->
            let label = inlinePlainText children
            in label
                <> if Text.null url || label == url
                    then ""
                    else " (" <> url <> ")"

-- The optional delimiter belongs to the caller. It is consumed only when it
-- can close at the current position; otherwise it is parsed as ordinary input.
parseSequence :: Maybe Text -> Maybe Char -> Text -> [Inline]
parseSequence closing initialPrevious = go initialPrevious
  where
    go previous text
        | Text.null text = []
        | Just marker <- closing
        , marker `Text.isPrefixOf` text
        , canClose marker previous (Text.drop (Text.length marker) text) =
            []
        | Just (escaped, rest) <- escapedPunctuation text =
            InlineText (Text.singleton escaped)
                : go (Just escaped) rest
        | Just (code, rest) <- codeSpan text =
            InlineCode code : go (lastCharacter code <|> previous) rest
        | Just (url, label, rest) <- linkSpan text =
            let children = parseInline label
                visible = inlinePlainText children
                previous' =
                    lastCharacter
                        (visible
                            <> if Text.null url || visible == url
                                then ""
                                else " (" <> url <> ")")
                        <|> previous
            in InlineLink url children : go previous' rest
        | maybe True (not . isWordCharacter) previous
        , Just (url, rest) <- bareUrlSpan text =
            InlineLink url [InlineText url]
                : go (lastCharacter url <|> previous) rest
        | Just (node, visible, rest) <- styledSpan previous "**" InlineStrong text =
            node : go (lastCharacter visible <|> previous) rest
        | Just (node, visible, rest) <- styledSpan previous "__" InlineStrong text =
            node : go (lastCharacter visible <|> previous) rest
        | Just (node, visible, rest) <- styledSpan previous "*" InlineEmphasis text =
            node : go (lastCharacter visible <|> previous) rest
        | Just (node, visible, rest) <- styledSpan previous "_" InlineEmphasis text =
            node : go (lastCharacter visible <|> previous) rest
        | otherwise =
            let (plain, rest) = takePlain closing text
                literal
                    | Text.null plain = Text.take 1 text
                    | otherwise = plain
                remaining
                    | Text.null plain = Text.drop 1 text
                    | otherwise = rest
            in InlineText literal
                : go (lastCharacter literal <|> previous) remaining

styledSpan
    :: Maybe Char
    -> Text
    -> ([Inline] -> Inline)
    -> Text
    -> Maybe (Inline, Text, Text)
styledSpan previous marker constructor text = do
    afterOpen <- Text.stripPrefix marker text
    if canOpen marker previous afterOpen
        then do
            (bodySource, rest) <- findClosing marker previous afterOpen
            if Text.null bodySource
                then Nothing
                else
                    let children = parseInline bodySource
                    in pure
                        (constructor children, inlinePlainText children, rest)
        else Nothing

-- Find a closing delimiter by parsing nested constructs in source order. This
-- lets the first star of a final @***@ close an inner emphasis span before the
-- remaining @**@ closes its surrounding strong span.
findClosing :: Text -> Maybe Char -> Text -> Maybe (Text, Text)
findClosing marker initialPrevious input =
    scan initialPrevious "" input
  where
    scan previous consumed remaining
        | Text.null remaining = Nothing
        | Text.any (== '\n') (Text.take 1 remaining) = Nothing
        | marker `Text.isPrefixOf` remaining
        , canClose marker previous (Text.drop (Text.length marker) remaining) =
            Just
                ( consumed
                , Text.drop (Text.length marker) remaining
                )
        | Just (_, rest) <- escapedPunctuation remaining =
            consumeThrough rest
        | Just (_, rest) <- codeSpan remaining =
            consumeThrough rest
        | Just (_, _, rest) <- linkSpan remaining =
            consumeThrough rest
        | Just rest <- completeStyled "**" remaining =
            consumeThrough rest
        | Just rest <- completeStyled "__" remaining =
            consumeThrough rest
        | Just rest <- completeStyled "*" remaining =
            consumeThrough rest
        | Just rest <- completeStyled "_" remaining =
            consumeThrough rest
        | otherwise =
            consumeThrough (Text.drop 1 remaining)
      where
        consumeThrough rest =
            let consumedLength = Text.length remaining - Text.length rest
                chunk = Text.take consumedLength remaining
            in scan
                (lastCharacter chunk <|> previous)
                (consumed <> chunk)
                rest

        completeStyled nestedMarker source = do
            afterOpen <- Text.stripPrefix nestedMarker source
            if canOpen nestedMarker previous afterOpen
                then do
                    (_, rest) <- findClosing nestedMarker previous afterOpen
                    pure rest
                else Nothing

codeSpan :: Text -> Maybe (Text, Text)
codeSpan text =
    let (ticks, afterOpen) = Text.span (== '`') text
        tickCount = Text.length ticks
    in if tickCount == 0
        then Nothing
        else findClose tickCount "" afterOpen
  where
    findClose tickCount body remaining
        | Text.null remaining = Nothing
        | Text.isPrefixOf "\n" remaining = Nothing
        | otherwise =
            let (before, atTicks) = Text.break (== '`') remaining
            in if Text.null atTicks
                then Nothing
                else
                    let (run, afterRun) = Text.span (== '`') atTicks
                    in if Text.length run == tickCount
                        then Just (body <> before, afterRun)
                        else findClose
                            tickCount
                            (body <> before <> run)
                            afterRun

linkSpan :: Text -> Maybe (Text, Text, Text)
linkSpan text = do
    afterOpen <- Text.stripPrefix "[" text
    (label, afterLabel) <- takeLinkLabel 0 "" afterOpen
    afterDestinationOpen <- Text.stripPrefix "(" afterLabel
    (url, rest) <- takeDestination 0 "" afterDestinationOpen
    if Text.null label
        then Nothing
        else Just (url, label, rest)

takeLinkLabel :: Int -> Text -> Text -> Maybe (Text, Text)
takeLinkLabel depth consumed remaining =
    case Text.uncons remaining of
        Nothing -> Nothing
        Just ('\n', _) -> Nothing
        Just ('\\', afterSlash) ->
            case Text.uncons afterSlash of
                Just (escaped, rest)
                    | isAsciiPunctuation escaped ->
                        takeLinkLabel depth
                            (consumed <> Text.pack ['\\', escaped])
                            rest
                _ -> takeLinkLabel depth (consumed <> "\\") afterSlash
        Just ('[', rest) ->
            takeLinkLabel (depth + 1) (consumed <> "[") rest
        Just (']', rest)
            | depth == 0
            , Text.isPrefixOf "(" rest ->
                Just (consumed, rest)
            | depth > 0 ->
                takeLinkLabel (depth - 1) (consumed <> "]") rest
        Just (character, rest) ->
            takeLinkLabel depth
                (consumed <> Text.singleton character)
                rest

takeDestination :: Int -> Text -> Text -> Maybe (Text, Text)
takeDestination depth consumed remaining =
    case Text.uncons remaining of
        Nothing -> Nothing
        Just ('\n', _) -> Nothing
        Just ('\\', afterSlash) ->
            case Text.uncons afterSlash of
                Just (escaped, rest)
                    | isAsciiPunctuation escaped ->
                        takeDestination depth
                            (consumed <> Text.singleton escaped)
                            rest
                _ -> takeDestination depth (consumed <> "\\") afterSlash
        Just ('(', rest) ->
            takeDestination (depth + 1) (consumed <> "(") rest
        Just (')', rest)
            | depth == 0 -> Just (consumed, rest)
            | otherwise ->
                takeDestination (depth - 1) (consumed <> ")") rest
        Just (character, rest) ->
            takeDestination depth
                (consumed <> Text.singleton character)
                rest

bareUrlSpan :: Text -> Maybe (Text, Text)
bareUrlSpan text = do
    afterScheme <-
        Text.stripPrefix "https://" text
            <|> Text.stripPrefix "http://" text
    let schemeLength = Text.length text - Text.length afterScheme
        candidate = Text.takeWhile isUrlCharacter text
        url = trimBareUrl candidate
    if Text.length url > schemeLength
        then Just (url, Text.drop (Text.length url) text)
        else Nothing
  where
    isUrlCharacter character =
        not (isSpace character)
            && not (isControl character)

trimBareUrl :: Text -> Text
trimBareUrl candidate =
    case Text.unsnoc candidate of
        Just (prefix, character)
            | character `elem` (".,;:!?\"'" :: String) ->
                trimBareUrl prefix
            | character == ')'
            , unmatchedClosing '(' ')' candidate ->
                trimBareUrl prefix
            | character == ']'
            , unmatchedClosing '[' ']' candidate ->
                trimBareUrl prefix
            | character == '}'
            , unmatchedClosing '{' '}' candidate ->
                trimBareUrl prefix
        _ -> candidate
  where
    unmatchedClosing opening closing text =
        Text.count (Text.singleton closing) text
            > Text.count (Text.singleton opening) text

escapedPunctuation :: Text -> Maybe (Char, Text)
escapedPunctuation text = do
    afterSlash <- Text.stripPrefix "\\" text
    (character, rest) <- Text.uncons afterSlash
    if isAsciiPunctuation character
        then Just (character, rest)
        else Nothing

isAsciiPunctuation :: Char -> Bool
isAsciiPunctuation character =
    isAscii character
        && character >= '!'
        && character <= '~'
        && not (isAlphaNum character)

canOpen :: Text -> Maybe Char -> Text -> Bool
canOpen marker previous after =
    case Text.uncons after of
        Nothing -> False
        Just (first, _)
            | isSpace first -> False
            | isUnderscore marker ->
                maybe True (not . isWordCharacter) previous
            | otherwise -> True

canClose :: Text -> Maybe Char -> Text -> Bool
canClose marker previous after =
    case previous of
        Nothing -> False
        Just last_
            | isSpace last_ -> False
            | isUnderscore marker ->
                maybe True (not . isWordCharacter . fst) (Text.uncons after)
            | otherwise -> True

isUnderscore :: Text -> Bool
isUnderscore = Text.all (== '_')

isWordCharacter :: Char -> Bool
isWordCharacter character = isAlphaNum character || character == '_'

takePlain :: Maybe Text -> Text -> (Text, Text)
takePlain closing text =
    case
        [ index
        | Just index <-
            [ Text.findIndex isMarkupStart text
            , prefixIndex "https://" text
            , prefixIndex "http://" text
            ]
        ] of
        [] -> (text, "")
        indexes -> Text.splitAt (minimum indexes) text
  where
    isMarkupStart character =
        character == '\\'
            || character == '`'
            || character == '['
            || character == '*'
            || character == '_'
            || maybe False
                (\marker -> character == Text.head marker)
                closing

    prefixIndex prefix source =
        let (before, match) = Text.breakOn prefix source
        in if Text.null match
            then Nothing
            else Just (Text.length before)

lastCharacter :: Text -> Maybe Char
lastCharacter text = snd <$> Text.unsnoc text

coalesceText :: [Inline] -> [Inline]
coalesceText = foldr step []
  where
    step (InlineText left) (InlineText right : rest) =
        InlineText (left <> right) : rest
    step inline rest = inline : rest
