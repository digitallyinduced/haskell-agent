-- | Tolerant structure discovery and advisory validation for plan Markdown.
module Agent.Tools.PlanMode.Document
    ( PlanSectionKind(..)
    , PlanSection(..)
    , PlanWarningCode(..)
    , PlanValidationWarning(..)
    , PlanDocument(..)
    , parsePlanDocument
    ) where

import Data.Char (isAlphaNum, isDigit, toLower)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

data PlanSectionKind
    = PlanContext
    | PlanApproach
    | PlanChanges
    | PlanVerification
    | PlanRisks
    | PlanNonGoals
    deriving (Eq, Ord, Show)

data PlanSection = PlanSection
    { planSectionKind :: !PlanSectionKind
    , planSectionTitle :: !Text
    , planSectionBody :: !Text
    , planSectionStartLine :: !Int
    , planSectionEndLine :: !Int
    } deriving (Eq, Show)

data PlanWarningCode
    = PlanIsEmpty
    | PlanMissingApproach
    | PlanMissingChangeScope
    | PlanMissingVerification
    | PlanEmptySection
    | PlanMalformedSectionHeading
    | PlanVerificationNotActionable
    deriving (Eq, Ord, Show)

data PlanValidationWarning = PlanValidationWarning
    { planWarningCode :: !PlanWarningCode
    , planWarningMessage :: !Text
    , planWarningLine :: !(Maybe Int)
    } deriving (Eq, Show)

data PlanDocument = PlanDocument
    { planDocumentMarkdown :: !Text
    , planDocumentSections :: ![PlanSection]
    , planDocumentWarnings :: ![PlanValidationWarning]
    , planDocumentVerification :: ![Text]
    } deriving (Eq, Show)

parsePlanDocument :: Text -> PlanDocument
parsePlanDocument markdown =
    let Parsed sections malformed = parseSections markdown
        verification = extractVerification sections
        warnings =
            validationWarnings markdown sections malformed verification
    in PlanDocument
        { planDocumentMarkdown = markdown
        , planDocumentSections = sections
        , planDocumentWarnings = warnings
        , planDocumentVerification = verification
        }

data Parsed = Parsed ![PlanSection] ![(Int, Text)]

data OpenSection = OpenSection
    { openKind :: !PlanSectionKind
    , openTitle :: !Text
    , openStart :: !Int
    , openLinesRev :: ![Text]
    }

data ParserState = ParserState
    { parserSectionsRev :: ![PlanSection]
    , parserOpen :: !(Maybe OpenSection)
    , parserMalformedRev :: ![(Int, Text)]
    , parserFence :: !(Maybe Char)
    , parserLastLine :: !Int
    }

parseSections :: Text -> Parsed
parseSections markdown =
    let initial = ParserState [] Nothing [] Nothing 0
        final = foldl' parseLine initial (zip [1 ..] (Text.lines markdown))
        closed = closeOpen (final.parserLastLine) final
    in Parsed
        (reverse closed.parserSectionsRev)
        (reverse closed.parserMalformedRev)

parseLine :: ParserState -> (Int, Text) -> ParserState
parseLine state (lineNumber, line) =
    let state' = state { parserLastLine = lineNumber }
    in case state.parserFence of
        Just marker ->
            appendOpen line $
                if closesFence marker line
                    then state' { parserFence = Nothing }
                    else state'
        Nothing -> case startsFence line of
            Just marker ->
                appendOpen line (state' { parserFence = Just marker })
            Nothing -> case parseHeading line of
                Just title
                    | Just kind <- sectionKind title ->
                        openSection lineNumber kind title state'
                _ ->
                    let malformed = malformedSectionHeading line
                        withWarning = case malformed of
                            Nothing -> state'
                            Just title ->
                                state'
                                    { parserMalformedRev =
                                        (lineNumber, title)
                                            : state'.parserMalformedRev
                                    }
                    in appendOpen line withWarning

openSection
    :: Int
    -> PlanSectionKind
    -> Text
    -> ParserState
    -> ParserState
openSection lineNumber kind title state =
    let closed = closeOpen (lineNumber - 1) state
    in closed
        { parserOpen = Just OpenSection
            { openKind = kind
            , openTitle = title
            , openStart = lineNumber
            , openLinesRev = []
            }
        }

appendOpen :: Text -> ParserState -> ParserState
appendOpen line state =
    state
        { parserOpen = fmap
            (\section ->
                section { openLinesRev = line : section.openLinesRev })
            state.parserOpen
        }

closeOpen :: Int -> ParserState -> ParserState
closeOpen endLine state = case state.parserOpen of
    Nothing -> state
    Just section ->
        state
            { parserSectionsRev =
                PlanSection
                    { planSectionKind = section.openKind
                    , planSectionTitle = section.openTitle
                    , planSectionBody =
                        Text.unlines (reverse section.openLinesRev)
                    , planSectionStartLine = section.openStart
                    , planSectionEndLine = max section.openStart endLine
                    }
                    : state.parserSectionsRev
            , parserOpen = Nothing
            }

parseHeading :: Text -> Maybe Text
parseHeading line = do
    let stripped = Text.stripStart line
        (hashes, suffix) = Text.span (== '#') stripped
        level = Text.length hashes
    if level < 1 || level > 6
        then Nothing
        else case Text.uncons suffix of
            Just (separator, rest)
                | separator == ' ' || separator == '\t' ->
                    nonEmpty (stripClosingHashes rest)
            _ -> Nothing

malformedSectionHeading :: Text -> Maybe Text
malformedSectionHeading line = do
    let stripped = Text.stripStart line
        (hashes, suffix) = Text.span (== '#') stripped
        level = Text.length hashes
    if level < 1 || level > 6 || Text.null suffix
        then Nothing
        else case Text.uncons suffix of
            Just (separator, _)
                | separator == ' ' || separator == '\t' -> Nothing
            _ -> do
                title <- nonEmpty (stripClosingHashes suffix)
                _ <- sectionKind title
                pure title

stripClosingHashes :: Text -> Text
stripClosingHashes =
    Text.strip . Text.dropWhileEnd (== '#') . Text.strip

nonEmpty :: Text -> Maybe Text
nonEmpty value
    | Text.null (Text.strip value) = Nothing
    | otherwise = Just (Text.strip value)

sectionKind :: Text -> Maybe PlanSectionKind
sectionKind title
    | normalized `Set.member` contextAliases = Just PlanContext
    | normalized `Set.member` approachAliases = Just PlanApproach
    | normalized `Set.member` changeAliases = Just PlanChanges
    | normalized `Set.member` verificationAliases = Just PlanVerification
    | normalized `Set.member` riskAliases = Just PlanRisks
    | normalized `Set.member` nonGoalAliases = Just PlanNonGoals
    | otherwise = Nothing
  where
    normalized = normalizeTitle title

normalizeTitle :: Text -> Text
normalizeTitle =
    Text.unwords
        . Text.words
        . Text.map
            (\character ->
                if isAlphaNum character
                    then toLower character
                    else ' ')

contextAliases, approachAliases, changeAliases, verificationAliases
    , riskAliases, nonGoalAliases :: Set.Set Text
contextAliases = Set.fromList
    ["context", "summary", "overview", "objective", "goals"]
approachAliases = Set.fromList
    [ "approach"
    , "design"
    , "implementation approach"
    , "strategy"
    , "proposed approach"
    ]
changeAliases = Set.fromList
    [ "changes"
    , "affected files"
    , "affected areas"
    , "changes and affected files"
    , "changes affected files"
    , "changes and affected areas"
    , "changes affected areas"
    , "implementation"
    , "implementation steps"
    , "proposed changes"
    ]
verificationAliases = Set.fromList
    ["verification", "validation", "testing", "tests", "test plan"]
riskAliases = Set.fromList
    ["risks", "risk", "risks and mitigations", "considerations"]
nonGoalAliases = Set.fromList
    ["non goals", "non goal", "out of scope", "non objectives"]

startsFence :: Text -> Maybe Char
startsFence line =
    let stripped = Text.stripStart line
    in if "```" `Text.isPrefixOf` stripped
        then Just '`'
        else if "~~~" `Text.isPrefixOf` stripped
            then Just '~'
            else Nothing

closesFence :: Char -> Text -> Bool
closesFence marker line =
    Text.replicate 3 (Text.singleton marker)
        `Text.isPrefixOf` Text.stripStart line

validationWarnings
    :: Text
    -> [PlanSection]
    -> [(Int, Text)]
    -> [Text]
    -> [PlanValidationWarning]
validationWarnings markdown sections malformed verification =
    emptyPlan
        <> missing PlanApproach PlanMissingApproach
            "Plan is missing an Approach section."
        <> missing PlanChanges PlanMissingChangeScope
            "Plan is missing a Changes, Affected Files, or Affected Areas section."
        <> missing PlanVerification PlanMissingVerification
            "Plan is missing a Verification section."
        <> emptySections
        <> malformedWarnings
        <> nonActionableVerification
  where
    emptyPlan =
        [ warning PlanIsEmpty
            "Plan is empty." Nothing
        | Text.null (Text.strip markdown)
        ]
    missing kind code message =
        [warning code message Nothing | not (hasSection kind)]
    hasSection kind = any ((== kind) . (.planSectionKind)) sections
    emptySections =
        [ warning PlanEmptySection
            ("Plan section “" <> section.planSectionTitle <> "” is empty.")
            (Just section.planSectionStartLine)
        | section <- sections
        , Text.null (Text.strip section.planSectionBody)
        ]
    malformedWarnings =
        [ warning PlanMalformedSectionHeading
            ("Malformed Markdown section heading: " <> title)
            (Just line)
        | (line, title) <- malformed
        ]
    nonActionableVerification =
        [ warning PlanVerificationNotActionable
            "Verification has no actionable checks, commands, or checklist items."
            (firstSectionLine PlanVerification sections)
        | hasSection PlanVerification
        , null verification
        ]

warning
    :: PlanWarningCode
    -> Text
    -> Maybe Int
    -> PlanValidationWarning
warning planWarningCode planWarningMessage planWarningLine =
    PlanValidationWarning{..}

firstSectionLine :: PlanSectionKind -> [PlanSection] -> Maybe Int
firstSectionLine kind sections =
    case
        [ section.planSectionStartLine
        | section <- sections
        , section.planSectionKind == kind
        ]
    of
        line : _ -> Just line
        [] -> Nothing

extractVerification :: [PlanSection] -> [Text]
extractVerification =
    deduplicate
        . concatMap (extractChecks . (.planSectionBody))
        . filter ((== PlanVerification) . (.planSectionKind))

extractChecks :: Text -> [Text]
extractChecks body =
    reverse checks
  where
    (_, checks) = foldl' step (Nothing, []) (Text.lines body)
    step (fence, found) rawLine =
        let line = Text.strip rawLine
        in case fence of
            Just marker
                | closesFence marker line -> (Nothing, found)
                | Text.null line -> (fence, found)
                | otherwise -> (fence, line : found)
            Nothing -> case startsFence line of
                Just marker -> (Just marker, found)
                Nothing -> case actionableLine line of
                    Nothing -> (Nothing, found)
                    Just check -> (Nothing, check : found)

actionableLine :: Text -> Maybe Text
actionableLine line =
    stripListMarker line
        <|> stripInlineCommand line
        <|> commandLike line

stripListMarker :: Text -> Maybe Text
stripListMarker line =
    let stripped = Text.stripStart line
    in case Text.uncons stripped of
        Just (marker, rest)
            | marker `elem` ("-*+" :: String)
            , Just (separator, content) <- Text.uncons rest
            , separator == ' ' || separator == '\t' ->
                nonEmpty (stripCheckbox content)
        _ ->
            let (digits, rest) = Text.span isDigit stripped
            in if Text.null digits
                then Nothing
                else case Text.uncons rest of
                    Just (marker, afterMarker)
                        | marker == '.' || marker == ')'
                        , Just (separator, content) <- Text.uncons afterMarker
                        , separator == ' ' || separator == '\t' ->
                            nonEmpty (stripCheckbox content)
                    _ -> Nothing

stripCheckbox :: Text -> Text
stripCheckbox content =
    let stripped = Text.stripStart content
    in case Text.uncons stripped of
        Just ('[', rest)
            | Text.length rest >= 2
            , Text.index rest 1 == ']' ->
                Text.stripStart (Text.drop 2 rest)
        _ -> stripped

stripInlineCommand :: Text -> Maybe Text
stripInlineCommand line
    | Text.length line >= 2
    , Text.head line == '`'
    , Text.last line == '`' =
        nonEmpty (Text.dropEnd 1 (Text.drop 1 line))
    | otherwise = Nothing

commandLike :: Text -> Maybe Text
commandLike line
    | any (`Text.isPrefixOf` folded) commandPrefixes = nonEmpty line
    | otherwise = Nothing
  where
    folded = Text.toCaseFold line
    commandPrefixes =
        [ "$ "
        , "run "
        , "verify "
        , "validate "
        , "test "
        , "check "
        , "confirm "
        , "ensure "
        , "cabal "
        , "nix "
        , "ghci"
        , "stack "
        , "tmux "
        , "pytest"
        , "npm "
        , "pnpm "
        , "yarn "
        , "make "
        , "cargo "
        , "go test"
        ]

deduplicate :: [Text] -> [Text]
deduplicate = reverse . snd . foldl' step (Set.empty, [])
  where
    step (seen, values) value
        | value `Set.member` seen = (seen, values)
        | otherwise = (Set.insert value seen, value : values)

infixr 3 <|>
(<|>) :: Maybe a -> Maybe a -> Maybe a
Nothing <|> right = right
left <|> _ = left
