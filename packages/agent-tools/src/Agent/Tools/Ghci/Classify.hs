-- | Pure/static classification for persistent GHCi tool input.
module Agent.Tools.Ghci.Classify
    ( GhciClass(..)
    , classifyGhciInput
    , defaultGhciExtensions
    , typeLooksEffectful
    ) where

import Data.Char (isAlphaNum, isSpace)
import Data.Text (Text)
import qualified Data.Text as Text

data GhciClass
    = GhciPure
    | GhciEffectful
    deriving (Eq, Show)

-- | Static gate. @Nothing@ means the caller needs a dynamic @:type@ probe.
classifyGhciInput :: Text -> Maybe GhciClass
classifyGhciInput raw =
    let text = Text.strip raw
    in if Text.null text
        then Just GhciEffectful
        else if mentionsUnsafe text
            then Just GhciEffectful
            else case ghciCommandName text of
                Just cmd
                    | cmd `elem` safeInfoCommands -> Just GhciPure
                    | cmd `elem` effectfulCommands -> Just GhciEffectful
                    | cmd == "set" && isPromptSet text -> Just GhciEffectful
                    | otherwise -> Just GhciEffectful
                Nothing
                    | looksLikeDoBlock text -> Just GhciEffectful
                    | isBinding text -> Just GhciPure
                    | otherwise -> Nothing

safeInfoCommands :: [Text]
safeInfoCommands =
    [ "type", "t", "kind", "k", "info", "i", "browse", "show", "doc"
    , "hoogle", "instances", "module"
    ]

effectfulCommands :: [Text]
effectfulCommands =
    [ "!", "cd", "def", "script", "load", "l", "reload", "r"
    , "add", "unadd", "main", "run", "edit", "e", "sprint"
    , "force", "print", "quit", "q", "issafe", "ctags", "etags"
    ]

mentionsUnsafe :: Text -> Bool
mentionsUnsafe text =
    any (`Text.isInfixOf` text)
        [ "unsafePerformIO"
        , "unsafeInterleaveIO"
        , "accursedUnutterablePerformIO"
        , "inlinePerformIO"
        , "unsafeDupablePerformIO"
        ]

ghciCommandName :: Text -> Maybe Text
ghciCommandName text =
    case Text.uncons (Text.dropWhile isSpace text) of
        Just (':', rest) ->
            let name = Text.takeWhile (\c -> isAlphaNum c || c == '!' || c == '-') rest
            in if Text.null name then Just "!" else Just (Text.toLower name)
        _ -> Nothing

isPromptSet :: Text -> Bool
isPromptSet text = "prompt" `Text.isInfixOf` Text.toLower text

looksLikeDoBlock :: Text -> Bool
looksLikeDoBlock text =
    let stripped = Text.strip text
    in "do" == stripped
        || "do\n" `Text.isPrefixOf` stripped
        || "do " `Text.isPrefixOf` stripped
        || "\ndo " `Text.isInfixOf` stripped
        || "\ndo\n" `Text.isInfixOf` stripped

isBinding :: Text -> Bool
isBinding text =
    let stripped = Text.strip text
        lowered = Text.toLower stripped
    in "let " `Text.isPrefixOf` lowered
        || "let\n" `Text.isPrefixOf` lowered
        || (hasEqualsBinding stripped && not (Text.isPrefixOf "data " lowered)
            && not (Text.isPrefixOf "type " lowered)
            && not (Text.isPrefixOf "newtype " lowered)
            && not (Text.isPrefixOf "class " lowered)
            && not (Text.isPrefixOf "instance " lowered))

hasEqualsBinding :: Text -> Bool
hasEqualsBinding text =
    case Text.breakOn "=" text of
        (before, after)
            | Text.null after -> False
            | ":" `Text.isInfixOf` before -> False
            | otherwise ->
                let name = Text.strip before
                in not (Text.null name)
                    && Text.all (\c -> isAlphaNum c || c `elem` ("_' " :: String)) name

-- | True when a @:type@ reply denotes an IO or clearly effectful result.
typeLooksEffectful :: Text -> Bool
typeLooksEffectful output =
    let cleaned = Text.unwords (Text.words (stripTypeErrors output))
        typePart = case Text.breakOnEnd "::" cleaned of
            (prefix, rest)
                | Text.null prefix -> cleaned
                | otherwise -> Text.strip rest
        afterConstraints = case Text.breakOnEnd "=>" typePart of
            (prefix, rest)
                | Text.null prefix -> typePart
                | otherwise -> Text.strip rest
        resultSide = case Text.breakOnEnd "->" afterConstraints of
            (prefix, rest)
                | Text.null prefix -> afterConstraints
                | otherwise -> Text.strip rest
        tokens = tokenizeType resultSide
        allTokens = tokenizeType afterConstraints
    in case tokens of
        (headTok : _) -> headTok == "IO" || "MonadIO" `elem` allTokens
        [] -> "MonadIO" `elem` allTokens

stripTypeErrors :: Text -> Text
stripTypeErrors =
    Text.unlines
        . filter (\line -> not ("error:" `Text.isInfixOf` line)
            && not ("<interactive>" `Text.isPrefixOf` Text.strip line))
        . Text.lines

tokenizeType :: Text -> [Text]
tokenizeType =
    filter (not . Text.null)
        . Text.split (\c -> isSpace c || c == '(' || c == ')' || c == ',')

-- | Extra extensions on top of GHC2021, matching the agent packages.
defaultGhciExtensions :: [String]
defaultGhciExtensions =
    [ "BlockArguments"
    , "OverloadedStrings"
    , "OverloadedRecordDot"
    , "DuplicateRecordFields"
    , "NoFieldSelectors"
    , "LambdaCase"
    , "RecordWildCards"
    ]
