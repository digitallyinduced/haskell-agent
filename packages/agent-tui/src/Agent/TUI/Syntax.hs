-- | Syntax highlighting for fenced code blocks.
--
-- The public representation deliberately contains semantic token classes
-- rather than colors so each TUI theme can choose its own palette.
module Agent.TUI.Syntax
    ( HighlightedLine
    , SyntaxClass(..)
    , SyntaxHighlighter
    , SyntaxSpan(..)
    , highlightCode
    , loadSyntaxHighlighter
    , loadSyntaxHighlighterFrom
    , resolveFenceLanguage
    , syntaxClassForTokenType
    ) where

import Data.Char (ord)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Skylighting.Core
    ( SyntaxMap
    , TokenType(..)
    , TokenizerConfig(..)
    , loadValidSyntaxesFromDir
    , lookupSyntax
    , tokenize
    )
import System.Environment (lookupEnv)
import System.FilePath (takeExtension, takeFileName)
import System.IO.Error (tryIOError)

-- | A small, stable set of semantic classes understood by the TUI themes.
data SyntaxClass
    = SyntaxNormal
    | SyntaxKeyword
    | SyntaxType
    | SyntaxFunction
    | SyntaxVariable
    | SyntaxString
    | SyntaxNumber
    | SyntaxComment
    | SyntaxOperator
    | SyntaxAnnotation
    | SyntaxPreprocessor
    | SyntaxWarning
    | SyntaxError
    deriving (Bounded, Enum, Eq, Ord, Show)

data SyntaxSpan = SyntaxSpan
    { syntaxClass :: !SyntaxClass
    , syntaxText :: !Text
    }
    deriving (Eq, Show)

type HighlightedLine = [SyntaxSpan]

newtype SyntaxHighlighter = SyntaxHighlighter SyntaxMap

maxHighlightBytes :: Int
maxHighlightBytes = 256 * 1024

maxHighlightLines :: Int
maxHighlightLines = 5000

-- | Load the curated syntax definitions installed with @agent-tui@.
--
-- Loading failure is represented as a value so syntax highlighting can always
-- remain an optional enhancement to the fullscreen UI.
loadSyntaxHighlighter :: IO (Either Text SyntaxHighlighter)
loadSyntaxHighlighter =
    lookupEnv "AGENT_TUI_SYNTAX_DIR" >>= \case
        Nothing ->
            pure (Left "AGENT_TUI_SYNTAX_DIR is not configured")
        Just syntaxDirectory ->
            loadSyntaxHighlighterFrom syntaxDirectory

loadSyntaxHighlighterFrom :: FilePath -> IO (Either Text SyntaxHighlighter)
loadSyntaxHighlighterFrom syntaxDirectory = do
    result <- tryIOError (loadValidSyntaxesFromDir syntaxDirectory)
    pure case result of
        Left exception ->
            Left ("Could not load syntax definitions: " <> Text.pack (show exception))
        Right (loadErrors, syntaxMap)
            | not (Map.null loadErrors) ->
                Left
                    ( "Could not load syntax definitions: "
                        <> Text.intercalate
                            "; "
                            [ Text.pack path <> ": " <> Text.pack message
                            | (path, message) <- Map.toList loadErrors
                            ]
                    )
            | Map.null syntaxMap ->
                Left "No syntax definitions were installed"
            | otherwise ->
                Right (SyntaxHighlighter syntaxMap)

-- | Highlight a fenced code block.
--
-- The first 'Text' is the complete fence info string. Unknown languages,
-- explicit plain-text fences, oversized inputs, and tokenizer failures return
-- 'Left'; callers should render those blocks with the ordinary code attribute.
highlightCode
    :: SyntaxHighlighter
    -> Text
    -> Text
    -> Either Text [HighlightedLine]
highlightCode (SyntaxHighlighter syntaxMap) fenceInfo source
    | utf8Length source > maxHighlightBytes =
        Left "Code block exceeds the syntax-highlighting byte limit"
    | sourceLineCount source > maxHighlightLines =
        Left "Code block exceeds the syntax-highlighting line limit"
    | otherwise = do
        language <-
            maybe
                (Left "Fence does not specify a highlighted language")
                Right
                (resolveFenceLanguage fenceInfo)
        syntax <-
            maybe
                (Left ("Unknown syntax language: " <> language))
                Right
                (lookupSyntax language syntaxMap)
        tokenLines <-
            either (Left . Text.pack) Right $
                tokenize
                    TokenizerConfig
                        { syntaxMap
                        , traceOutput = False
                        }
                    syntax
                    source
        preserveSourceLines source (map (map tokenSpan) tokenLines)
  where
    tokenSpan (tokenType, tokenText) =
        SyntaxSpan
            { syntaxClass = syntaxClassForTokenType tokenType
            , syntaxText = tokenText
            }

-- | Resolve a Markdown fence info string to the identifier used for syntax
-- lookup. The result is normalized but is not guaranteed to name an installed
-- syntax.
resolveFenceLanguage :: Text -> Maybe Text
resolveFenceLanguage info = do
    firstToken <- case Text.words info of
        token : _ -> Just token
        [] -> Nothing
    let normalized = Text.toLower (Text.strip firstToken)
        candidate =
            maybe normalized languageFromPath (lineRangePath normalized)
        aliased = Map.findWithDefault candidate candidate languageAliases
    if aliased `elem` plainTextAliases || Text.null aliased
        then Nothing
        else Just aliased

lineRangePath :: Text -> Maybe Text
lineRangePath value =
    case Text.splitOn ":" value of
        startLine : endLine : pathParts
            | not (null pathParts)
            , isDecimal startLine
            , isDecimal endLine ->
                Just (Text.intercalate ":" pathParts)
        _ -> Nothing
  where
    isDecimal text =
        not (Text.null text)
            && Text.all (\character -> character >= '0' && character <= '9') text

languageFromPath :: Text -> Text
languageFromPath path =
    let fileName = takeFileName (Text.unpack path)
        extension = takeExtension fileName
    in if null extension
        then Text.toLower (Text.pack fileName)
        else Text.toLower (Text.pack (drop 1 extension))

plainTextAliases :: [Text]
plainTextAliases =
    [ "plain"
    , "plaintext"
    , "text"
    , "txt"
    ]

languageAliases :: Map.Map Text Text
languageAliases =
    Map.fromList
        [ ("c++", "cpp")
        , ("cs", "csharp")
        , ("docker", "dockerfile")
        , ("hs", "haskell")
        , ("js", "javascript")
        , ("patch", "diff")
        , ("py", "python")
        , ("rs", "rust")
        , ("sh", "bash")
        , ("shell", "bash")
        , ("ts", "typescript")
        , ("tsx", "jsx")
        , ("yml", "yaml")
        ]

-- | Map Skylighting's complete token vocabulary into the smaller semantic set
-- used by the retained TUI.
syntaxClassForTokenType :: TokenType -> SyntaxClass
syntaxClassForTokenType = \case
    KeywordTok -> SyntaxKeyword
    DataTypeTok -> SyntaxType
    DecValTok -> SyntaxNumber
    BaseNTok -> SyntaxNumber
    FloatTok -> SyntaxNumber
    ConstantTok -> SyntaxNumber
    CharTok -> SyntaxString
    SpecialCharTok -> SyntaxString
    StringTok -> SyntaxString
    VerbatimStringTok -> SyntaxString
    SpecialStringTok -> SyntaxString
    ImportTok -> SyntaxType
    CommentTok -> SyntaxComment
    DocumentationTok -> SyntaxComment
    AnnotationTok -> SyntaxAnnotation
    CommentVarTok -> SyntaxComment
    OtherTok -> SyntaxNormal
    FunctionTok -> SyntaxFunction
    VariableTok -> SyntaxVariable
    ControlFlowTok -> SyntaxKeyword
    OperatorTok -> SyntaxOperator
    BuiltInTok -> SyntaxFunction
    ExtensionTok -> SyntaxType
    PreprocessorTok -> SyntaxPreprocessor
    AttributeTok -> SyntaxAnnotation
    RegionMarkerTok -> SyntaxPreprocessor
    InformationTok -> SyntaxWarning
    WarningTok -> SyntaxWarning
    AlertTok -> SyntaxError
    ErrorTok -> SyntaxError
    NormalTok -> SyntaxNormal

preserveSourceLines
    :: Text
    -> [HighlightedLine]
    -> Either Text [HighlightedLine]
preserveSourceLines source highlightedLines =
    let sourceLines = Text.splitOn "\n" source
        highlightedTexts =
            map (Text.concat . map (.syntaxText)) highlightedLines
        tokenizedSourceLines = take (length highlightedTexts) sourceLines
    in if length highlightedTexts > length sourceLines
        || highlightedTexts /= tokenizedSourceLines
        then Left "Syntax tokenizer did not preserve the original source text"
        else
            Right
                ( highlightedLines
                    <> replicate
                        (length sourceLines - length highlightedLines)
                        []
                )

sourceLineCount :: Text -> Int
sourceLineCount source
    | Text.null source = 1
    | otherwise = Text.count "\n" source + 1

utf8Length :: Text -> Int
utf8Length = Text.foldl' (\total character -> total + utf8CharLength character) 0

utf8CharLength :: Char -> Int
utf8CharLength character
    | code <= 0x7f = 1
    | code <= 0x7ff = 2
    | code <= 0xffff = 3
    | otherwise = 4
  where
    code = ord character
