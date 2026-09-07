-- | Renderer-independent syntax highlighting for fenced code blocks.
--
-- The public representation deliberately contains semantic token classes
-- rather than colors so callers can choose their own presentation.
module Agent.Syntax
    ( HighlightedLine
    , SyntaxClass(..)
    , SyntaxHighlighter
    , SyntaxSpan(..)
    , highlightCode
    , loadSyntaxLanguage
    , loadSyntaxHighlighter
    , loadSyntaxHighlighterFrom
    , newSyntaxHighlighter
    , newSyntaxHighlighterFrom
    , resolveFenceLanguage
    , resolvePathLanguage
    ) where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Char (isAlphaNum)
import Data.List (find, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError
import qualified Data.Text.Unsafe as TextUnsafe
import Skylighting.Core
    ( Context(..)
    , ContextSwitch(..)
    , ListItem(..)
    , Matcher(..)
    , Rule(..)
    , Syntax(..)
    , SyntaxMap
    , TokenType(..)
    , TokenizerConfig(..)
    , loadValidSyntaxesFromDir
    , lookupSyntax
    , tokenize
    )
import Skylighting.Loader (loadSyntaxFromFile)
import Skylighting.Parser
    ( addSyntaxDefinition
    , resolveKeywords
    )
import System.Directory (listDirectory)
import System.Environment (lookupEnv)
import System.FilePath
    ( (</>)
    , dropExtension
    , takeExtension
    , takeFileName
    )
import System.IO (IOMode(ReadMode), withBinaryFile)
import System.IO.Error (tryIOError)

-- | A small, stable set of semantic token classes.
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

data SyntaxHighlighter = SyntaxHighlighter
    { syntaxFiles :: !(Map.Map Text FilePath)
    , syntaxHeadersIndexed :: !Bool
    , syntaxRawMap :: !SyntaxMap
    , syntaxResolvedMap :: !SyntaxMap
    }

maxHighlightBytes :: Int
maxHighlightBytes = 256 * 1024

maxHighlightLines :: Int
maxHighlightLines = 5000

-- | Load the pinned syntax definitions supplied by the runtime environment.
--
-- Loading failure is represented as a value so syntax highlighting can always
-- remain an optional enhancement.
loadSyntaxHighlighter :: IO (Either Text SyntaxHighlighter)
loadSyntaxHighlighter =
    lookupEnv "AGENT_SYNTAX_DIR" >>= \case
        Nothing ->
            pure (Left "AGENT_SYNTAX_DIR is not configured")
        Just syntaxDirectory ->
            loadSyntaxHighlighterFrom syntaxDirectory

loadSyntaxHighlighterFrom :: FilePath -> IO (Either Text SyntaxHighlighter)
loadSyntaxHighlighterFrom syntaxDirectory = do
    filesResult <- syntaxFilesFrom syntaxDirectory
    result <- tryIOError (loadValidSyntaxesFromDir syntaxDirectory)
    pure case (filesResult, result) of
        (Left message, _) ->
            Left message
        (_, Left exception) ->
            Left ("Could not load syntax definitions: " <> Text.pack (show exception))
        (Right _, Right (loadErrors, _))
            | not (Map.null loadErrors) ->
                Left
                    ( "Could not load syntax definitions: "
                        <> Text.intercalate
                            "; "
                            [ Text.pack path <> ": " <> Text.pack message
                            | (path, message) <- Map.toList loadErrors
                            ]
                    )
        (Right _, Right (_, syntaxMap))
            | Map.null syntaxMap ->
                Left "No syntax definitions were installed"
        (Right files, Right (_, syntaxMap)) ->
            Right
                SyntaxHighlighter
                    { syntaxFiles = files
                    , syntaxHeadersIndexed = False
                    , syntaxRawMap = syntaxMap
                    , syntaxResolvedMap = syntaxMap
                    }

-- | Discover the installed syntax files without parsing their XML bodies.
--
-- This is the lightweight initializer used by interactive clients. Individual
-- definitions are loaded later with 'loadSyntaxLanguage'.
newSyntaxHighlighter :: IO (Either Text SyntaxHighlighter)
newSyntaxHighlighter =
    lookupEnv "AGENT_SYNTAX_DIR" >>= \case
        Nothing ->
            pure (Left "AGENT_SYNTAX_DIR is not configured")
        Just syntaxDirectory ->
            newSyntaxHighlighterFrom syntaxDirectory

newSyntaxHighlighterFrom :: FilePath -> IO (Either Text SyntaxHighlighter)
newSyntaxHighlighterFrom syntaxDirectory =
    fmap
        ( fmap
            \files ->
                SyntaxHighlighter
                    { syntaxFiles = files
                    , syntaxHeadersIndexed = False
                    , syntaxRawMap = Map.empty
                    , syntaxResolvedMap = Map.empty
                    }
        )
        (syntaxFilesFrom syntaxDirectory)

syntaxFilesFrom :: FilePath -> IO (Either Text (Map.Map Text FilePath))
syntaxFilesFrom syntaxDirectory = do
    result <- tryIOError (listDirectory syntaxDirectory)
    pure case result of
        Left exception ->
            Left ("Could not load syntax definitions: " <> Text.pack (show exception))
        Right entries
            | null syntaxEntries ->
                Left "No syntax definitions were installed"
            | otherwise ->
                Right $
                    Map.fromList
                        [ ( Text.toLower
                                (Text.pack (dropExtension entry))
                          , syntaxDirectory </> entry
                          )
                        | entry <- syntaxEntries
                        ]
          where
            syntaxEntries =
                sort
                    [ entry
                    | entry <- entries
                    , takeExtension entry == ".xml"
                    ]

-- | Add one requested syntax and the transitive definitions it includes.
--
-- The returned value is an immutable cache snapshot. Callers can safely keep
-- rendering with the old value while this IO action runs on a worker thread.
loadSyntaxLanguage
    :: SyntaxHighlighter
    -> Text
    -> IO (Either Text SyntaxHighlighter)
loadSyntaxLanguage highlighter fenceInfo =
    case resolveFenceLanguage fenceInfo of
        Nothing ->
            pure (Left "Fence does not specify a highlighted language")
        Just language
            | Just _ <-
                lookupLoadedSyntax
                    highlighter
                    highlighter.syntaxResolvedMap
                    language ->
                pure (Right highlighter)
            | otherwise ->
                loadSyntaxClosure highlighter language

loadSyntaxClosure
    :: SyntaxHighlighter
    -> Text
    -> IO (Either Text SyntaxHighlighter)
loadSyntaxClosure highlighter firstLanguage =
    go
        Set.empty
        highlighter
        highlighter.syntaxRawMap
        [firstLanguage]
  where
    go _ current rawMap [] =
        let resolvedMap = Map.map (resolveKeywords rawMap) rawMap
        in pure $
            Right
                current
                    { syntaxRawMap = rawMap
                    , syntaxResolvedMap = resolvedMap
                    }
    go attemptedFiles current rawMap (language : remaining)
        | Just _ <- lookupLoadedSyntax current rawMap language =
            go attemptedFiles current rawMap remaining
        | otherwise = do
            (indexed, fileResult) <- findSyntaxFile current language
            case fileResult of
                Nothing ->
                    pure (Left ("Unknown syntax language: " <> language))
                Just syntaxFile
                    | Set.member syntaxFile attemptedFiles ->
                        pure
                            (Left
                                ("Syntax definition did not provide language: "
                                    <> language))
                    | otherwise -> do
                        syntaxResult <-
                            tryIOError (loadSyntaxFromFile syntaxFile)
                        case syntaxResult of
                            Left exception ->
                                pure $
                                    Left
                                        ( "Could not load syntax definition "
                                            <> Text.pack syntaxFile
                                            <> ": "
                                            <> Text.pack (show exception)
                                        )
                            Right (Left message) ->
                                pure $
                                    Left
                                        ( "Could not load syntax definition "
                                            <> Text.pack syntaxFile
                                            <> ": "
                                            <> Text.pack message
                                        )
                            Right (Right syntax) ->
                                let
                                    nextRawMap =
                                        addSyntaxDefinition syntax rawMap
                                    dependencies =
                                        [ dependency
                                        | dependency <-
                                            syntaxDependencies syntax
                                        , lookupLoadedSyntax
                                            indexed
                                            nextRawMap
                                            dependency
                                            == Nothing
                                        , Set.notMember
                                            dependency
                                            (Set.fromList remaining)
                                        ]
                                in go
                                    (Set.insert syntaxFile attemptedFiles)
                                    indexed
                                    nextRawMap
                                    (dependencies <> remaining)

syntaxDependencies :: Syntax -> [Text]
syntaxDependencies syntax =
    Set.toList $
        Set.delete Text.empty $
            Set.delete syntax.sName $
            Set.unions
                [ foldMap listItemDependencies $
                    concat (Map.elems syntax.sLists)
                , foldMap contextDependencies $
                    Map.elems syntax.sContexts
                ]

listItemDependencies :: ListItem -> Set.Set Text
listItemDependencies = \case
    Item _ -> Set.empty
    IncludeList (language, _) ->
        Set.singleton language

contextDependencies :: Context -> Set.Set Text
contextDependencies context =
    foldMap ruleDependencies context.cRules
        <> foldMap contextSwitchDependencies context.cLineEmptyContext
        <> foldMap contextSwitchDependencies context.cLineEndContext
        <> foldMap contextSwitchDependencies context.cLineBeginContext
        <> foldMap contextSwitchDependencies context.cFallthroughContext

ruleDependencies :: Rule -> Set.Set Text
ruleDependencies rule =
    matcherDependencies rule.rMatcher
        <> foldMap contextSwitchDependencies rule.rContextSwitch
        <> foldMap ruleDependencies rule.rChildren

matcherDependencies :: Matcher -> Set.Set Text
matcherDependencies = \case
    IncludeRules (language, _) ->
        Set.singleton language
    _ -> Set.empty

contextSwitchDependencies :: ContextSwitch -> Set.Set Text
contextSwitchDependencies = \case
    Pop -> Set.empty
    Push (language, _) ->
        Set.singleton language

findSyntaxFile
    :: SyntaxHighlighter
    -> Text
    -> IO (SyntaxHighlighter, Maybe FilePath)
findSyntaxFile highlighter language =
    case directSyntaxFile highlighter language of
        Just syntaxFile ->
            pure (highlighter, Just syntaxFile)
        Nothing
            | highlighter.syntaxHeadersIndexed ->
                pure (highlighter, Nothing)
            | otherwise -> do
                identifierFiles <-
                    syntaxHeaderIndex $
                        Set.toList $
                            Set.fromList $
                                Map.elems highlighter.syntaxFiles
                let indexed =
                        highlighter
                            { syntaxFiles =
                                Map.union
                                    highlighter.syntaxFiles
                                    identifierFiles
                            , syntaxHeadersIndexed = True
                            }
                pure (indexed, directSyntaxFile indexed language)

syntaxHeaderIndex :: [FilePath] -> IO (Map.Map Text FilePath)
syntaxHeaderIndex syntaxFiles =
    fmap (Map.fromList . concat) $
        mapM
            (\syntaxFile ->
                map (,syntaxFile)
                    <$> syntaxHeaderIdentifiersFromFile syntaxFile)
            syntaxFiles

syntaxHeaderIdentifiersFromFile :: FilePath -> IO [Text]
syntaxHeaderIdentifiersFromFile syntaxFile = do
    result <-
        tryIOError $
            withBinaryFile syntaxFile ReadMode \handle ->
                ByteString.hGet handle syntaxHeaderByteLimit
    pure case result of
        Left _ -> []
        Right header -> syntaxHeaderIdentifiers header

directSyntaxFile :: SyntaxHighlighter -> Text -> Maybe FilePath
directSyntaxFile highlighter language =
    let
        key = syntaxLanguageKey language
    in Map.lookup key highlighter.syntaxFiles
        <|> Map.lookup (filenameKey key) highlighter.syntaxFiles

lookupLoadedSyntax
    :: SyntaxHighlighter
    -> SyntaxMap
    -> Text
    -> Maybe Syntax
lookupLoadedSyntax highlighter syntaxMap language =
    lookupSyntax normalized syntaxMap
        <|> lookupSyntax (syntaxLanguageKey normalized) syntaxMap
        <|> do
            syntaxFile <- directSyntaxFile highlighter normalized
            find
                ((== takeFileName syntaxFile) . (.sFilename))
                (Map.elems syntaxMap)
  where
    normalized = Text.toLower (Text.strip language)

syntaxLanguageKey :: Text -> Text
syntaxLanguageKey language =
    let normalized = Text.toLower (Text.strip language)
    in Map.findWithDefault normalized normalized syntaxFilenameAliases

syntaxFilenameAliases :: Map.Map Text Text
syntaxFilenameAliases =
    Map.fromList
        [ ("actionscript 2.0", "actionscript")
        , ("alerts", "alert")
        , ("apache configuration", "apache")
        , ("c#", "cs")
        , ("csharp", "cs")
        , ("c++", "cpp")
        , ("coffeescript", "coffee")
        , ("common lisp", "commonlisp")
        , ("fortran (fixed format)", "fortran-fixed")
        , ("fortran (free format)", "fortran-free")
        , ("gccextensions", "gcc")
        , ("godot", "gd-script")
        , ("ini files", "ini")
        , ("intel x86 (nasm)", "nasm")
        , ("iso c++", "isocpp")
        , ("javascript react (jsx)", "javascript-react")
        , ("jsx", "javascript-react")
        , ("mustache/handlebars (html)", "mustache")
        , ("objective caml", "ocaml")
        , ("objective-c", "objectivec")
        , ("objective-c++", "objectivecpp")
        , ("php/php", "php")
        , ("pov-ray", "povray")
        , ("r script", "r")
        , ("relaxng-compact", "relaxngcompact")
        , ("restructuredtext", "rest")
        , ("ruby/rails/rhtml", "rhtml")
        , ("scilab", "sci")
        , ("tcl/tk", "tcl")
        , ("x.org configuration", "xorg")
        ]

filenameKey :: Text -> Text
filenameKey =
    Text.dropAround (== '-')
        . Text.map
            (\character ->
                if isAlphaNum character
                    then character
                    else '-')
        . Text.toLower

syntaxHeaderByteLimit :: Int
syntaxHeaderByteLimit = 32 * 1024

syntaxHeaderIdentifiers :: ByteString -> [Text]
syntaxHeaderIdentifiers =
    go
        . TextEncoding.decodeUtf8With TextEncodingError.lenientDecode
  where
    go source =
        case Text.breakOn "<language" source of
            (_, rest)
                | Text.null rest -> []
                | otherwise ->
                    let
                        afterStart = Text.drop (Text.length "<language") rest
                        (tag, afterTag) = Text.breakOn ">" afterStart
                        identifiers =
                            case xmlAttribute "name" tag of
                                Nothing -> []
                                Just name ->
                                    name
                                        : maybe
                                            []
                                            (Text.splitOn ";")
                                            (xmlAttribute
                                                "alternativeNames"
                                                tag)
                    in map
                        (Text.toLower . Text.strip . decodeXmlAttribute)
                        identifiers
                        <> go (Text.drop 1 afterTag)

xmlAttribute :: Text -> Text -> Maybe Text
xmlAttribute attribute tag =
    let needle = attribute <> "=\""
        (_, match) = Text.breakOn needle tag
    in if Text.null match
        then Nothing
        else
            Just $
                Text.takeWhile (/= '"') $
                    Text.drop (Text.length needle) match

decodeXmlAttribute :: Text -> Text
decodeXmlAttribute value =
    foldl
        (\current (encoded, decoded) ->
            Text.replace encoded decoded current)
        value
        [ ("&quot;", "\"")
        , ("&apos;", "'")
        , ("&lt;", "<")
        , ("&gt;", ">")
        , ("&amp;", "&")
        ]

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
highlightCode highlighter fenceInfo source
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
                (lookupLoadedSyntax
                    highlighter
                    highlighter.syntaxResolvedMap
                    language)
        tokenLines <-
            either (Left . Text.pack) Right $
                tokenize
                    TokenizerConfig
                        { syntaxMap = highlighter.syntaxResolvedMap
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
            case lineRangePath normalized of
                Just path -> languageFromPath path
                Nothing
                    | Text.any (`elem` ['/', '\\']) normalized ->
                        languageFromPath (normalizePathSeparators normalized)
                    | otherwise -> normalized
    normalizeLanguage candidate

-- | Resolve a complete file path to the identifier used for syntax lookup.
--
-- Unlike Markdown fence info, file paths may contain whitespace and must not
-- be truncated to their first whitespace-delimited token.
resolvePathLanguage :: Text -> Maybe Text
resolvePathLanguage =
    normalizeLanguage
        . languageFromPath
        . normalizePathSeparators
        . Text.toLower
        . Text.strip

normalizePathSeparators :: Text -> Text
normalizePathSeparators =
    Text.map
        (\character ->
            if character == '\\'
                then '/'
                else character)

normalizeLanguage :: Text -> Maybe Text
normalizeLanguage candidate =
    let aliased = Map.findWithDefault candidate candidate languageAliases
    in if aliased `elem` plainTextAliases || Text.null aliased
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
        , ("udiff", "diff")
        , ("yml", "yaml")
        ]

-- | Map Skylighting's complete token vocabulary into the smaller semantic set
-- exposed to renderers.
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
utf8Length = TextUnsafe.lengthWord8
