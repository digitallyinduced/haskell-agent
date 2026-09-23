{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

-- Run with: nix develop .#docs -c runghc docs/scripts/VerifyDocumentationCoverage.hs
-- Presence checks do not establish instructional quality or runtime availability.
import Control.Exception.Safe (bracket, catch, IOException)
import Control.Monad (forM, forM_, unless, when)
import Data.Aeson (Value, eitherDecodeStrict', encode, object, toJSON, (.=))
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Char (isAlpha, isAlphaNum, isLower, isSpace)
import Data.Either (isLeft)
import Data.List (tails)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.Directory
import System.Environment (getArgs, getEnv, lookupEnv, setEnv, withArgs)
import System.Exit (ExitCode (..), die)
import System.FilePath
import System.IO.Temp (withTempDirectory)
import Text.HTML.TagSoup (Tag (..), innerText, parseTags)
import Test.Hspec (describe, hspec, it, shouldBe, shouldReturn, shouldSatisfy, shouldThrow)

data Token = Identifier Text | Literal Text | Symbol Char deriving (Eq, Show)

-- A small lexical inventory, not a Haskell parser. Unsupported registry shapes
-- fail closed. Comments and string contents cannot introduce fake identifiers.
tokens :: Text -> [Token]
tokens source
    | Text.null source = []
    | "--" `Text.isPrefixOf` source = tokens (Text.dropWhile (/= '\n') source)
    | "{-" `Text.isPrefixOf` source = tokens (skipComment 1 (Text.drop 2 source))
    | isSpace first = tokens rest
    | first == '"' = let (value, following) = stringLiteral rest in Literal value : tokens following
    | isAlpha first || first == '_' =
        let (name, following) = Text.span (\c -> isAlphaNum c || c `elem` ("_.'" :: String)) source
        in Identifier name : tokens following
    | otherwise = Symbol first : tokens rest
  where
    first = Text.head source
    rest = Text.tail source
    stringLiteral value = case Text.uncons value of
        Nothing -> ("", "")
        Just ('"', following) -> ("", following)
        Just ('\\', following) -> case Text.uncons following of
            Nothing -> ("", "")
            Just (character, remaining) -> let (suffix, end) = stringLiteral remaining in (Text.cons character suffix, end)
        Just (character, following) -> let (suffix, end) = stringLiteral following in (Text.cons character suffix, end)
    skipComment :: Int -> Text -> Text
    skipComment depth value
        | Text.null value = ""
        | "{-" `Text.isPrefixOf` value = skipComment (depth + 1) (Text.drop 2 value)
        | "-}" `Text.isPrefixOf` value = if depth == 1 then Text.drop 2 value else skipComment (depth - 1) (Text.drop 2 value)
        | otherwise = skipComment depth (Text.tail value)

topLevel :: Text -> Bool
topLevel line = case Text.uncons line of
    Just (first, _) | isLower first -> case tokens line of
        Identifier _ : Symbol ':' : Symbol ':' : _ -> True
        Identifier _ : Symbol '=' : _ -> True
        _ -> False
    _ -> False

declaration :: Text -> Text -> Either Text Text
declaration source name =
    case dropWhile (not . starts) (Text.lines source) of
        [] -> Left ("Cannot locate declaration " <> name <> "; update registry extractor")
        line : remaining -> Right $ Text.unlines (Text.drop 1 (snd (Text.breakOn "=" line)) : takeWhile (not . topLevel) remaining)
  where
    starts line = case tokens line of
        Identifier actual : Symbol '=' : _ -> actual == name && not (Text.isPrefixOf " " line)
        _ -> False

nonempty :: Text -> Set Text -> Either Text (Set Text)
nonempty label names
    | Set.null names = Left (label <> " inventory is empty or changed shape")
    | otherwise = Right names

slashNames :: Text -> Either Text (Set Text)
slashNames source = do
    body <- tokens <$> declaration source "slashCommands"
    entries <- traverse parseEntry
        [following | Symbol punctuation : following@(Identifier _ : Literal _ : _) <- tails body, punctuation `elem` ("[," :: String)]
    nonempty "Slash command" (Set.fromList (concat entries))
  where
    parseEntry (Identifier "grokToolCmd" : Literal _ : remaining) = parseCommand remaining
    parseEntry (Identifier name : remaining) | name `elem` ["cmd", "codexCmd"] = parseCommand remaining
    parseEntry _ = Left "Unrecognized slash registry entry; update registry extractor"
    parseCommand (Literal name : Symbol '[' : remaining) =
        let (aliases, end) = break (== Symbol ']') remaining
        in if null end || any (not . validAlias) aliases
            then Left "Unrecognized slash command aliases"
            else Right (map ("/" <>) (name : [alias | Literal alias <- aliases]))
    parseCommand _ = Left "Unrecognized slash registry entry; update registry extractor"
    validAlias (Literal _) = True
    validAlias (Symbol ',') = True
    validAlias _ = False

optionNames :: Text -> Either Text (Set Text)
optionNames source = do
    body <- tokens <$> declaration source "optionUpdateParser"
    nonempty "Run option" $ Set.fromList
        ["--" <> name | Identifier constructor : Literal name : _ <- tails body,
         constructor `elem` ["optionUpdate", "flagUpdate", "boolFlagUpdate", "codeModeFlagUpdate", "screenFlagUpdate", "Options.long"]]

decoderNames :: Text -> Either Text (Set Text)
decoderNames source = do
    let sections = decoderSections (Text.lines source)
    names <- traverse key
        [following | section <- sections, Identifier name : following <- tails (tokens section),
         name `elem` ["defaultKey", "optionalKey", "Hermes.atKey"]]
    nonempty "Configuration decoder" (Set.fromList names)
  where
    decoderSections [] = []
    decoderSections (line : remaining) = case tokens line of
        Identifier name : Symbol ':' : Symbol ':' : _ | topLevel line && "Decoder" `Text.isSuffixOf` name ->
            let (body, following) = break isSignature remaining in Text.unlines body : decoderSections following
        _ -> decoderSections remaining
    isSignature line = topLevel line && "::" `Text.isInfixOf` line
    key following =
        let (arguments, decoder) = break isDecoder following
            literals = [name | Literal name <- arguments]
        in case (reverse literals, decoder) of
            (name : _, _ : _) | validName name -> Right name
            _ -> Left ("Unrecognized decoder key application: " <> Text.pack (show (take 12 following)))
    isDecoder (Identifier name) = "Hermes." `Text.isPrefixOf` name || "Decoder" `Text.isSuffixOf` name
    isDecoder _ = False
    validName value = maybe False (isAlpha . fst) (Text.uncons value)
        && Text.all (\c -> isAlphaNum c || c == '_') value

toolNames :: [Text] -> Either Text (Set Text)
toolNames sources = do
    names <- concat <$> traverse sourceNames sources
    nonempty "Built-in JSON descriptor" (Set.fromList names)
  where
    sourceNames source = traverse (resolve source)
        [name | Identifier constructor : name : _ <- tails (tokens source),
         constructor `elem` ["jsonTool", "jsonAppToolWithExecution"], isArgument name]
    isArgument (Literal _) = True
    isArgument (Identifier name) = maybe False (isLower . fst) (Text.uncons name)
    isArgument _ = False
    resolve _ (Literal name) = Right name
    resolve source (Identifier name) = do
        body <- declaration source name
        case tokens body of
            [Literal value] -> Right value
            _ -> Left ("Unrecognized built-in tool name: " <> name)
    resolve _ _ = Left "Unrecognized built-in tool name"

missingNames :: Set Text -> Text -> [Text]
missingNames names body = filter (not . present) (Set.toAscList names)
  where
    present name = any valid (Text.breakOnAll name body)
      where
        valid (before, suffix) =
            maybe True (not . leftBoundary . snd) (Text.unsnoc before)
            && maybe True (not . rightBoundary . fst) (Text.uncons (Text.drop (Text.length name) suffix))
    rightBoundary character = isAlphaNum character || character `elem` ("_-" :: String)
    leftBoundary character = rightBoundary character || character == '/'

data Document = Document { documentText :: Text, documentLinks :: [Text], documentExamples :: [(Text, Text)] }
    deriving (Show, Eq)

parseDocument :: Text -> Document
parseDocument source = Document (Text.concat (reverse fragments)) links examples
  where
    tags = parseTags source
    links = [value | TagOpen "a" attributes <- tags, Just value <- [lookup "href" attributes]]
    (fragments, examples) = parseBody False tags
    parseBody _ [] = ([], [])
    parseBody _ (TagOpen "article" _ : remaining) = parseBody True remaining
    parseBody _ (TagClose "article" : remaining) = parseBody False remaining
    parseBody True (TagOpen "code" attributes : remaining)
        | Just schema <- lookup "data-config-schema" attributes =
            let (contents, end) = break (== TagClose "code") remaining
                text = innerText contents
                (following, found) = parseBody True (drop 1 end)
            in (following ++ [text], (schema, text) : found)
    parseBody active (TagText value : remaining) =
        let (following, found) = parseBody active remaining in (following ++ [value | active], found)
    parseBody active (TagClose name : remaining)
        | active && name `elem` ["p", "td", "th", "li", "h2", "h3"] =
            let (following, found) = parseBody active remaining in (following ++ ["\n"], found)
    parseBody active (_ : remaining) = parseBody active remaining

fetchDocument :: Manager -> Text -> IO Document
fetchDocument manager address = do
    request <- parseRequest (Text.unpack address)
    response <- httpLbs request { responseTimeout = responseTimeoutMicro 20000000 } manager
    unless (statusCode (responseStatus response) == 200) (die ("Unexpected HTTP status for " <> Text.unpack address))
    either (die . show) (pure . parseDocument) (Text.decodeUtf8' (LazyByteString.toStrict (responseBody response)))

sourceFiles :: FilePath -> IO [FilePath]
sourceFiles directory = do
    names <- listDirectory directory
    concat <$> forM names (\name -> do
        let path = directory </> name
        directoryEntry <- doesDirectoryExist path
        if directoryEntry then sourceFiles path else pure [path | takeExtension path == ".hs"])

exportExamples :: FilePath -> [(Text, Document)] -> IO ()
exportExamples destination documents = do
    root <- getEnv "TMPDIR" >>= canonicalizePath
    target <- canonicalizePath destination
    unless (addTrailingPathSeparator root `isPathPrefixOf` target) (die "Example export must use a subdirectory of TMPDIR")
    let examples = [(route, schema, contents) | (route, document) <- documents,
                    (schema, contents) <- documentExamples document, schema /= "illustration"]
    when (null examples) (die "No decoder-tagged examples found; refusing an empty validation")
    forM_ examples $ \(route, schema, contents) -> do
        unless (schema `elem` ["harness", "models", "settings"]) (die (Text.unpack ("Unknown configuration schema " <> schema <> " at " <> route)))
        either die (const (pure ())) (eitherDecodeStrict' (Text.encodeUtf8 contents) :: Either String Value)
    createDirectoryIfMissing True target
    manifest <- forM (zip [0 :: Int ..] examples) $ \(index, (route, schema, contents)) -> do
        let number = show index
            filename = replicate (max 0 (4 - length number)) '0' <> number <> "-" <> Text.unpack schema <> ".json"
        -- Reject pre-existing symlink destinations rather than following them.
        linked <- pathIsSymbolicLink (target </> filename) `catch` (\(_ :: IOException) -> pure False)
        when linked (die ("Refusing symbolic-link example destination: " <> filename))
        Text.writeFile (target </> filename) contents
        pure (object ["schema" .= schema, "file" .= filename, "route" .= route])
    manifestLinked <- pathIsSymbolicLink (target </> "manifest.json") `catch` (\(_ :: IOException) -> pure False)
    when manifestLinked (die "Refusing symbolic-link manifest destination")
    LazyByteString.writeFile (target </> "manifest.json") (encode manifest <> "\n")
    putStrLn ("Exported " <> show (length manifest) <> " examples to " <> target)
  where
    isPathPrefixOf prefix value = Text.pack prefix `Text.isPrefixOf` Text.pack value

verify :: Text -> Maybe FilePath -> IO ()
verify address output = do
    root <- takeDirectory . takeDirectory . takeDirectory <$> canonicalizePath __FILE__
    manager <- newManager tlsManagerSettings
    home <- fetchDocument manager (address <> "/")
    let routes = Set.toAscList $ Set.fromList
            [link | link <- documentLinks home, "/" `Text.isPrefixOf` link, "/" `Text.isSuffixOf` link,
             not ("//" `Text.isPrefixOf` link), not ("/text/" `Text.isPrefixOf` link), not ("#" `Text.isInfixOf` link)]
    when (null routes) (die "No documentation routes found")
    documents <- forM routes (\route -> (,) route <$> fetchDocument manager (address <> route))
    let allText = Text.intercalate "\n" (map (documentText . snd) documents)
        load extractor path = Text.readFile (root </> path) >>= either (die . Text.unpack) pure . extractor
    registries <- sequence
        [ (,) "slash commands and aliases" <$> load slashNames "packages/agent-cli/src/Agent/CLI/Command/Catalog.hs"
        , (,) "launch options" <$> load optionNames "packages/agent-cli/src/Agent/CLI/Options.hs"
        , (,) "machine configuration keys" <$> load decoderNames "packages/agent-runtime/src/Agent/Runtime/Config.hs"
        , (,) "model catalog keys" <$> load decoderNames "packages/agent-runtime/src/Agent/Runtime/ModelConfig.hs"
        , (,) "persisted settings keys" <$> load decoderNames "packages/agent-runtime/src/Agent/Runtime/Project.hs"
        , (,) "agent-tools JSON descriptor names" <$> (sourceFiles (root </> "packages/agent-tools/src") >>= traverse Text.readFile >>= either (die . Text.unpack) pure . toolNames)
        ]
    failures <- forM registries $ \(label, names) -> do
        let absent = missingNames names allText
        putStrLn (label <> ": " <> show (Set.size names) <> " unique names, " <> show (length absent) <> " missing")
        pure [Text.pack label <> ": " <> Text.intercalate ", " absent | not (null absent)]
    forM_ output (`exportExamples` documents)
    unless (null (concat failures)) (die (Text.unpack (Text.intercalate "\n" (concat failures))))
    putStrLn "Registry presence checks passed; prose and runtime behavior require separate review."

selfTest :: IO ()
selfTest = hspec $ describe "Documentation coverage inventory" $ do
    it "resolves literal and constant tool names and rejects unknown expressions" $ do
        toolNames ["jsonTool \"read_file\" description\njsonAppToolWithExecution chartName description\nchartName = \"render_chart\"\n"] `shouldBe` Right (Set.fromList ["read_file", "render_chart"])
        toolNames ["jsonTool unknownName description"] `shouldSatisfy` isLeft
        toolNames [] `shouldSatisfy` isLeft
    it "requires documentation for new commands and aliases" $ do
        let source = "slashCommands = [cmd \"help\" [\"h\"] \"/help\" \"Help\" True, grokToolCmd \"scheduler_create\" \"loop\" [] \"/loop\" \"Repeat\" True]\nnext :: Int\n"
        slashNames source `shouldBe` Right (Set.fromList ["/help", "/h", "/loop"])
        missingNames (Set.fromList ["/help", "/h", "/loop", "/new-command"]) "/help /h /loop" `shouldBe` ["/new-command"]
    it "requires exact option boundaries" $ do
        missingNames (Set.singleton "--model") "--model-id" `shouldBe` ["--model"]
        missingNames (Set.singleton "--model") "Use --model NAME" `shouldBe` []
    it "extracts decoder keys without confusing default strings" $
        decoderNames "testDecoder :: Hermes.Decoder A\ntestDecoder = do\n x <- defaultKey \"stdio\" \"transport\" Hermes.text\n y <- defaultKey 12\n \"timeout\" Hermes.int\n z <- optionalKey \"token\" Hermes.text\n pure (x,y,z)\nnext :: Int\n"
            `shouldBe` Right (Set.fromList ["transport", "timeout", "token"])
    it "excludes navigation and preserves example text" $ do
        let document = parseDocument "<nav>/hidden</nav><article>/shown<code data-config-schema=\"harness\">{\"theme\":\"midnight\"}</code></article>"
        documentText document `shouldBe` "/shown{\"theme\":\"midnight\"}"
        documentExamples document `shouldBe` [("harness", "{\"theme\":\"midnight\"}")]
    it "rejects moved or unsupported registry shapes" $ do
        slashNames "commandsHaveMoved = []" `shouldSatisfy` isLeft
        slashNames "slashCommands = [cmd \"help\" [] \"/help\" \"Help\" True, newCmd \"missing\" []]" `shouldSatisfy` isLeft
        decoderNames "fieldDecoder :: Hermes.Decoder A\nfieldDecoder = optionalKey dynamic Hermes.text" `shouldSatisfy` isLeft
    it "rejects export to the temporary root or its parent without writing" $
        withExportFixture $ \root -> do
            exportExamples root validDocuments `shouldThrow` (== ExitFailure 1)
            exportExamples (takeDirectory root) validDocuments `shouldThrow` (== ExitFailure 1)
            listDirectory root `shouldReturn` []
            listDirectory (takeDirectory root) `shouldReturn` [takeFileName root]
    it "rejects an export directory symlink outside the permitted root" $
        withExportFixture $ \root -> do
            let destination = root </> "export"
            createDirectoryLink (takeDirectory root) destination
            exportExamples destination validDocuments `shouldThrow` (== ExitFailure 1)
            listDirectory (takeDirectory root) `shouldReturn` [takeFileName root]
    it "rejects an example file symlink without changing its target" $
        withExportFixture $ \root -> do
            let destination = root </> "export"
                retained = root </> "retained.json"
            createDirectory destination
            Text.writeFile retained "retained"
            createFileLink retained (destination </> "0000-harness.json")
            exportExamples destination validDocuments `shouldThrow` (== ExitFailure 1)
            Text.readFile retained `shouldReturn` "retained"
    it "rejects a manifest symlink without changing its target" $
        withExportFixture $ \root -> do
            let destination = root </> "export"
                retained = root </> "retained.json"
            createDirectory destination
            Text.writeFile retained "retained"
            createFileLink retained (destination </> "manifest.json")
            exportExamples destination validDocuments `shouldThrow` (== ExitFailure 1)
            Text.readFile retained `shouldReturn` "retained"
    it "rejects unknown schemas before creating an export directory" $
        withExportFixture $ \root -> do
            exportExamples (root </> "export") [("/", Document "" [] [("unknown", "{}")])]
                `shouldThrow` (== ExitFailure 1)
            listDirectory root `shouldReturn` []
    it "rejects invalid JSON before creating an export directory" $
        withExportFixture $ \root -> do
            exportExamples (root </> "export") [("/", Document "" [] [("harness", "{invalid")])]
                `shouldThrow` (== ExitFailure 1)
            listDirectory root `shouldReturn` []
    it "rejects empty and illustration-only exports without creating directories" $
        withExportFixture $ \root -> do
            exportExamples (root </> "export") [] `shouldThrow` (== ExitFailure 1)
            exportExamples (root </> "export") [("/", Document "" [] [("illustration", "{}")])]
                `shouldThrow` (== ExitFailure 1)
            listDirectory root `shouldReturn` []
    it "writes exact example contents and the matching manifest" $
        withExportFixture $ \root -> do
            let destination = root </> "export"
            exportExamples destination validDocuments
            Text.readFile (destination </> "0000-harness.json") `shouldReturn` "{\"enabled\": true}"
            manifest <- LazyByteString.readFile (destination </> "manifest.json")
            (eitherDecodeStrict' (LazyByteString.toStrict manifest) :: Either String Value)
                `shouldBe` Right (toJSON [object ["schema" .= ("harness" :: Text), "file" .= ("0000-harness.json" :: Text), "route" .= ("/fixture/" :: Text)]])

validDocuments :: [(Text, Document)]
validDocuments = [("/fixture/", Document "" [] [("harness", "{\"enabled\": true}")])]

-- Even the rejected "outside" paths remain inside the real session TMPDIR:
-- only the temporarily scoped export-policy root is narrowed for these tests.
withExportFixture :: (FilePath -> IO ()) -> IO ()
withExportFixture action = do
    original <- getEnv "TMPDIR"
    withTempDirectory original "documentation-coverage-export" $ \fixture -> do
        let root = fixture </> "permitted"
        createDirectory root
        bracket (setEnv "TMPDIR" root) (const (setEnv "TMPDIR" original)) (const (action root))

main :: IO ()
main = do
    arguments <- getArgs
    defaultUrl <- maybe "http://127.0.0.1:4321" Text.pack <$> lookupEnv "DOCUMENTATION_URL"
    case arguments of
        ["--self-test"] -> withArgs [] selfTest
        ["--help"] -> putStrLn "VerifyDocumentationCoverage.hs [--url URL] [--export-examples DIRECTORY] | --self-test"
        _ -> do
            (url, destination) <- parseArguments defaultUrl Nothing arguments
            verify (Text.dropWhileEnd (== '/') url) destination
  where
    parseArguments url output [] = pure (url, output)
    parseArguments _ output ("--url" : value : remaining) = parseArguments (Text.pack value) output remaining
    parseArguments url _ ("--export-examples" : value : remaining) = parseArguments url (Just value) remaining
    parseArguments _ _ _ = die "Expected --url URL, --export-examples DIRECTORY, or --self-test"
