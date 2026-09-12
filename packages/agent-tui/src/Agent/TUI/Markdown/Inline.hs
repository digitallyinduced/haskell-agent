-- | Shared, composable inline Markdown parsing.
module Agent.TUI.Markdown.Inline
    ( Inline(..)
    , inlinePlainText
    , parseInline
    , InlineStreamState
    , emptyInlineStreamState
    , feedInlineStream
    , inlineStreamSnapshot
    , finishInlineStream
    , inlineRetainedBytes
    , inlineStreamRetainedBytes
    ) where

import Control.Applicative ((<|>))
import Data.Char
    ( isAlphaNum
    , isAscii
    , isControl
    , isSpace
    )
import qualified Data.List as List
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
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

-- | Parsed syntax is retained independently of display width. Only the suffix
-- whose interpretation can change remains source text. In particular, an
-- unmatched opener can keep an arbitrarily long suffix provisional.
--
-- Pending scanners resume code runs, links, and URL boundaries during feeds;
-- they do not make repeated snapshots linear in the total input size.
data InlineStreamState = InlineStreamState
    !(Seq Inline)
    !(Maybe Char)
    !PendingInline
    deriving (Eq, Show)

data PendingInline = PendingInline !(Seq Text) !PendingScan
    deriving (Eq, Show)

-- These states locate a syntax boundary, not a provisional presentation. A
-- snapshot still renders malformed constructs with the batch parser's literal
-- fallback. Keeping this distinction avoids committing a provisional closer.
data PendingScan
    = ScanGeneral
    | ScanCodeOpening !Int
    | ScanCodeLiteral !Int
    | ScanCodeBody !Int !Int
    | ScanLinkLabel !Int !Bool !Bool
    | ScanLinkDestination !Int !Bool
    | ScanBareUrl
    | ScanStyleOpening !Char !Int
    | ScanUnclosedStyle !Char !Int
    | ScanStyleBody
    | ScanReconsider
    deriving (Eq, Show)

emptyInlineStreamState :: InlineStreamState
emptyInlineStreamState = InlineStreamState Seq.empty Nothing emptyPendingInline

-- | Conservative logical storage charge for syntax, including nested labels.
-- Text slices are charged separately even when their storage may be shared;
-- this is a logical estimate rather than exact heap residency.
inlineRetainedBytes :: Inline -> Integer
inlineRetainedBytes node = 64 + case node of
    InlineText text -> textBytes text
    InlineCode text -> textBytes text
    InlineStrong children -> childrenBytes children
    InlineEmphasis children -> childrenBytes children
    InlineLink destination children ->
        textBytes destination + childrenBytes children
  where
    textBytes text = 64 + 4 * toInteger (Text.length text)
    childrenBytes = foldl' (\size child -> size + inlineRetainedBytes child) 0

-- | Count the retained scanner state directly, without parsing its suffix.
inlineStreamRetainedBytes :: InlineStreamState -> Integer
inlineStreamRetainedBytes (InlineStreamState committed _ (PendingInline chunks _)) =
    192
        + foldl' (\size node -> size + inlineRetainedBytes node) 0 committed
        + foldl' (\size text -> size + 64 + 4 * toInteger (Text.length text)) 0 chunks

emptyPendingInline :: PendingInline
emptyPendingInline = PendingInline Seq.empty ScanGeneral

pendingInlineText :: PendingInline -> Text
pendingInlineText (PendingInline chunks _) = Text.concat (toList chunks)

-- | Append a delta. Newlines terminate all inline constructs, so even malformed
-- syntax becomes permanent at a line boundary. Empty deltas are identities.
feedInlineStream :: InlineStreamState -> Text -> InlineStreamState
feedInlineStream state delta
    | Text.null delta = state
feedInlineStream (InlineStreamState committed previous pending) delta =
    commitLines committed previous pending delta
  where
    commitLines nodes preceding provisional source =
        let (lineDelta, remainder) = Text.breakOn "\n" source
        in if Text.null remainder
            then appendInlinePending nodes preceding provisional lineDelta
            else
                let line = pendingInlineText provisional <> lineDelta
                    parsed = parseSequence Nothing preceding line
                    completed = (nodes <> Seq.fromList parsed) |> InlineText "\n"
                in commitLines completed Nothing emptyPendingInline (Text.drop 1 remainder)

appendInlinePending :: Seq Inline -> Maybe Char -> PendingInline -> Text -> InlineStreamState
appendInlinePending committed previous (PendingInline chunks scanner) delta
    | Seq.null chunks = retainInlinePrefix committed previous delta
    | otherwise =
        let scanner' = Text.foldl' advancePendingScan scanner delta
            chunks' = if Text.null delta then chunks else chunks |> delta
            pending = PendingInline chunks' scanner'
        in case scanner' of
            ScanGeneral -> retainInlinePrefix committed previous (pendingInlineText pending)
            ScanReconsider -> retainInlinePrefix committed previous (pendingInlineText pending)
            _ -> InlineStreamState committed previous pending

retainPendingInline :: Seq Inline -> Maybe Char -> Text -> InlineStreamState
retainPendingInline committed previous source =
    let scanner = case Text.uncons source of
            Just ('`', _) -> Text.foldl' advancePendingScan (ScanCodeOpening 0) source
            Just ('[', rest) -> Text.foldl' advancePendingScan (ScanLinkLabel 0 False False) rest
            Just (marker, _) | marker == '*' || marker == '_' ->
                case Text.foldl' advancePendingScan (ScanStyleOpening marker 0) source of
                    ScanReconsider -> case Text.unsnoc source of
                        Just (_, last_) | last_ == '*' || last_ == '_' -> ScanReconsider
                        _ -> ScanStyleBody
                    scanned -> scanned
            _ | "http://" `Text.isPrefixOf` source || "https://" `Text.isPrefixOf` source ->
                    Text.foldl' advancePendingScan ScanBareUrl source
              | otherwise -> ScanGeneral
    in InlineStreamState committed previous
        (PendingInline (Seq.singleton source) scanner)

advancePendingScan :: PendingScan -> Char -> PendingScan
advancePendingScan scanner character = case scanner of
    ScanCodeOpening count
        | character == '`' -> ScanCodeOpening (count + 1)
        | literalCodeCharacter character -> ScanCodeLiteral count
        | otherwise -> ScanCodeBody count 0
    ScanCodeLiteral count
        | character == '`' -> ScanCodeBody count 1
        | literalCodeCharacter character -> ScanCodeLiteral count
        | otherwise -> ScanCodeBody count 0
    ScanCodeBody openingCount closingCount
        | character == '`' -> ScanCodeBody openingCount (closingCount + 1)
        | closingCount == openingCount -> ScanReconsider
        | otherwise -> ScanCodeBody openingCount 0
    ScanLinkLabel depth escaped bracketPending
        | escaped && isAsciiPunctuation character -> ScanLinkLabel depth False False
        | escaped -> advancePendingScan (ScanLinkLabel depth False False) character
        | bracketPending && character == '(' -> ScanLinkDestination 0 False
        | bracketPending -> advancePendingScan (ScanLinkLabel depth False False) character
        | character == '\\' -> ScanLinkLabel depth True False
        | character == '[' -> ScanLinkLabel (depth + 1) False False
        | character == ']' && depth > 0 -> ScanLinkLabel (depth - 1) False False
        | character == ']' -> ScanLinkLabel depth False True
        | otherwise -> ScanLinkLabel depth False False
    ScanLinkDestination depth escaped
        | escaped && isAsciiPunctuation character -> ScanLinkDestination depth False
        | escaped -> advancePendingScan (ScanLinkDestination depth False) character
        | character == '\\' -> ScanLinkDestination depth True
        | character == '(' -> ScanLinkDestination (depth + 1) False
        | character == ')' && depth == 0 -> ScanReconsider
        | character == ')' -> ScanLinkDestination (depth - 1) False
        | otherwise -> ScanLinkDestination depth False
    ScanBareUrl
        | isSpace character || isControl character -> ScanReconsider
        | otherwise -> ScanBareUrl
    ScanStyleOpening marker count
        | character == marker -> ScanStyleOpening marker (count + 1)
        | otherwise -> ScanUnclosedStyle marker count
    ScanUnclosedStyle marker count
        | character == marker -> ScanReconsider
        | otherwise -> ScanUnclosedStyle marker count
    ScanStyleBody
        | Text.any (== character) "\\`[*_" -> ScanReconsider
        | otherwise -> ScanStyleBody
    ScanGeneral -> ScanGeneral
    ScanReconsider -> ScanReconsider

-- Without another tick or any inline markup (including a URL's colon), a
-- pending code opener and its body can only render as one literal text node.
literalCodeCharacter :: Char -> Bool
literalCodeCharacter character = not (Text.any (== character) "\\`[*_:")

-- | Preview literal fallbacks without committing them. This reparses the
-- unresolved suffix to preserve the batch parser's interpretation of malformed
-- and nested syntax. A long unfinished construct therefore still has growing
-- snapshot cost. Adjacent source chunks are concatenated together once rather
-- than through repeated strict appends.
inlineStreamSnapshot :: InlineStreamState -> [Inline]
inlineStreamSnapshot (InlineStreamState committed previous pending) =
    coalesceStreamText
        (toList committed <> pendingInlineSnapshot previous pending)

pendingInlineSnapshot :: Maybe Char -> PendingInline -> [Inline]
pendingInlineSnapshot previous pending@(PendingInline _ scanner) =
    let source = pendingInlineText pending
    in case scanner of
        ScanCodeOpening _ -> [InlineText source]
        ScanCodeLiteral _ -> [InlineText source]
        ScanStyleOpening _ count | count <= 2 -> [InlineText source]
        -- No matching marker exists after the initial one/two-character run.
        -- Longer runs can close themselves (e.g. "***"), so must fall back.
        -- Both strong and
        -- emphasis searches must fail, even when other nested syntax exists.
        -- Skip those searches, but still parse the remainder's literal fallback.
        ScanUnclosedStyle marker count | count <= 2 ->
            InlineText (Text.take count source)
                : parseSequence Nothing (Just marker) (Text.drop count source)
        _ -> parseSequence Nothing previous source

finishInlineStream :: InlineStreamState -> [Inline]
finishInlineStream = inlineStreamSnapshot

retainInlinePrefix :: Seq Inline -> Maybe Char -> Text -> InlineStreamState
retainInlinePrefix committed previous source
    | Text.null source = InlineStreamState committed previous emptyPendingInline
    | Just (escaped, rest) <- escapedPunctuation source =
        continue (InlineText (Text.singleton escaped)) (Just escaped) rest
    | Just (code, rest) <- codeSpan source
    , not (Text.null rest) =
        continue (InlineCode code) (lastCharacter code <|> previous) rest
    | Just (url, label, rest) <- linkSpan source =
        let node = InlineLink url (parseInline label)
        in continue node (lastCharacter (inlinePlainText [node]) <|> previous) rest
    | maybe True (not . isWordCharacter) previous
    , Just (url, rest) <- bareUrlSpan source
    , Text.any (\character -> isSpace character || isControl character) rest =
        continue (InlineLink url [InlineText url]) (lastCharacter url <|> previous) rest
    | Just (node, rest) <- stableSimpleStyle previous source =
        continue node (lastCharacter (inlinePlainText [node]) <|> previous) rest
    | otherwise =
        let (plain, remainder) = takePlain Nothing source
            retainedLength = Text.length plain - partialSchemeLength plain
            (stable, provisional) = Text.splitAt retainedLength plain
        in if Text.null stable
            then retainPendingInline committed previous source
            else retainInlinePrefix
                (committed |> InlineText stable)
                (lastCharacter stable <|> previous)
                (provisional <> remainder)
  where
    continue node preceding rest =
        retainInlinePrefix (committed |> node) preceding rest

-- A completed simple style is stable once both delimiter runs and right-hand
-- context are known. Nested syntax is kept provisional: a presently unmatched
-- code/link opener inside a style can later swallow its apparent closer.
stableSimpleStyle :: Maybe Char -> Text -> Maybe (Inline, Text)
stableSimpleStyle previous source = do
    (marker, constructor) <-
        List.find (\(marker, _) -> marker `Text.isPrefixOf` source)
            [("**", InlineStrong), ("__", InlineStrong),
             ("*", InlineEmphasis), ("_", InlineEmphasis)]
    afterOpen <- Text.stripPrefix marker source
    if canOpen marker previous afterOpen
        then do
            let (body, atClosing) = Text.break isAmbiguous afterOpen
            rest <- Text.stripPrefix marker atClosing
            (following, _) <- Text.uncons rest
            if not (Text.null body)
                && canClose marker (lastCharacter body) rest
                && following /= Text.head marker
                then Just (constructor (parseInline body), rest)
                else Nothing
        else Nothing
  where
    isAmbiguous character = Text.any (== character) "\\`[*_"

partialSchemeLength :: Text -> Int
partialSchemeLength source =
    maximum
        (0 :
            [ count
            | scheme <- ["http://", "https://"]
            , count <- [1 .. Text.length scheme - 1]
            , Text.take count scheme `Text.isSuffixOf` source
            ])

coalesceStreamText :: [Inline] -> [Inline]
coalesceStreamText [] = []
coalesceStreamText (InlineText first : rest) =
    let (texts, remaining) = span isText rest
    in InlineText (Text.concat (first : map textContent texts))
        : coalesceStreamText remaining
  where
    isText (InlineText _) = True
    isText _ = False
    textContent (InlineText value) = value
    textContent _ = ""
coalesceStreamText (node : rest) = node : coalesceStreamText rest

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
