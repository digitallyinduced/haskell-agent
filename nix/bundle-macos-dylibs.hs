{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- Copy and relocate the non-system dylibs used by Mach-O files.
--
-- Unlike dylibbundler, this program gives each source path a stable hashed
-- name. That distinction matters for Nix closures: two ABI-incompatible
-- libraries can have the same basename (for example Apple's and GNU's
-- libiconv.2.dylib).
module Main (main) where

import Control.Monad (foldM, unless, when)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isSpace)
import Data.List (dropWhileEnd, findIndex, isPrefixOf, tails)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric (showHex)
import System.Directory (
    canonicalizePath,
    copyFile,
    createDirectoryIfMissing,
    doesFileExist,
    getPermissions,
    setPermissions,
    writable,
 )
import System.Environment (getArgs)
import System.Exit (ExitCode (..), die)
import System.FilePath (takeFileName, (</>))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

data Command
    = Relocate FilePath String [FilePath]
    | ReplacePadded FilePath String String

data RelocateArguments = RelocateArguments
    { destinationArgument :: Maybe FilePath
    , installPrefixArgument :: Maybe String
    , targetArguments :: [FilePath]
    }

data WorkItem = WorkItem FilePath (Maybe String)

data LoadCommands = LoadCommands
    { loadedLibraries :: [FilePath]
    , loadRpaths :: [FilePath]
    , hasInstallId :: Bool
    }

type CopiedLibraries = Map FilePath (FilePath, String)

type BundledNames = Map FilePath FilePath

loadDylibCommands :: Set String
loadDylibCommands =
    Set.fromList
        [ "LC_LAZY_LOAD_DYLIB"
        , "LC_LOAD_DYLIB"
        , "LC_LOAD_UPWARD_DYLIB"
        , "LC_LOAD_WEAK_DYLIB"
        , "LC_REEXPORT_DYLIB"
        ]

systemPrefixes :: [FilePath]
systemPrefixes =
    [ "/System/Library/"
    , "/Library/Apple/System/Library/"
    , "/usr/lib/"
    ]

usage :: String
usage =
    unlines
        [ "usage:"
        , "  bundle-macos-dylibs relocate \\"
        , "    --destination DIR --install-prefix PREFIX TARGET..."
        , "  bundle-macos-dylibs replace-padded FILE OLD NEW"
        ]

main :: IO ()
main =
    getArgs >>= either die execute . parseCommand

execute :: Command -> IO ()
execute = \case
    Relocate destination installPrefix targets ->
        relocate destination installPrefix targets
    ReplacePadded path old new ->
        replacePadded path old new

parseCommand :: [String] -> Either String Command
parseCommand = \case
    "relocate" : arguments ->
        parseRelocateArguments
            RelocateArguments
                { destinationArgument = Nothing
                , installPrefixArgument = Nothing
                , targetArguments = []
                }
            arguments
            >>= finalizeRelocateArguments
    ["replace-padded", path, old, new] ->
        Right (ReplacePadded path old new)
    _ -> Left usage

parseRelocateArguments ::
    RelocateArguments ->
    [String] ->
    Either String RelocateArguments
parseRelocateArguments arguments = \case
    [] -> Right arguments
    "--destination" : value : rest
        | Nothing <- destinationArgument arguments ->
            parseRelocateArguments
                arguments{destinationArgument = Just value}
                rest
        | otherwise -> Left "--destination may only be specified once"
    ["--destination"] -> Left "--destination requires a value"
    "--install-prefix" : value : rest
        | Nothing <- installPrefixArgument arguments ->
            parseRelocateArguments
                arguments{installPrefixArgument = Just value}
                rest
        | otherwise -> Left "--install-prefix may only be specified once"
    ["--install-prefix"] -> Left "--install-prefix requires a value"
    option : _
        | "--" `isPrefixOf` option ->
            Left ("unknown option: " <> option)
    target : rest ->
        parseRelocateArguments
            arguments{targetArguments = target : targetArguments arguments}
            rest

finalizeRelocateArguments :: RelocateArguments -> Either String Command
finalizeRelocateArguments RelocateArguments{destinationArgument, installPrefixArgument, targetArguments} = do
    destination <-
        maybe (Left "--destination is required") Right destinationArgument
    installPrefix <-
        maybe (Left "--install-prefix is required") Right installPrefixArgument
    let targets = reverse targetArguments
    when (null targets) (Left "at least one TARGET is required")
    Right (Relocate destination installPrefix targets)

relocate :: FilePath -> String -> [FilePath] -> IO ()
relocate destination installPrefix initialTargets = do
    createDirectoryIfMissing True destination
    copied <-
        relocatePending
            destination
            installPrefix
            (Seq.fromList (map (`WorkItem` Nothing) initialTargets))
            Set.empty
            Map.empty
            Map.empty
    hPutStrLn stderr $
        "relocated "
            <> show (length initialTargets)
            <> " Mach-O target(s) with "
            <> show (Map.size copied)
            <> " uniquely named dylib(s)"

relocatePending ::
    FilePath ->
    String ->
    Seq WorkItem ->
    Set FilePath ->
    CopiedLibraries ->
    BundledNames ->
    IO CopiedLibraries
relocatePending destination installPrefix pending processed copied bundledNames =
    case Seq.viewl pending of
        Seq.EmptyL -> pure copied
        WorkItem unresolvedTarget copiedInstallName Seq.:< remaining -> do
            targetExists <- doesFileExist unresolvedTarget
            unless targetExists $
                die ("Mach-O target does not exist: " <> unresolvedTarget)
            target <- canonicalizePath unresolvedTarget
            if target `Set.member` processed
                then
                    relocatePending
                        destination
                        installPrefix
                        remaining
                        processed
                        copied
                        bundledNames
                else do
                    makeWritable target
                    LoadCommands{loadedLibraries, loadRpaths, hasInstallId} <-
                        readLoadCommands target
                    when hasInstallId $
                        runInstallNameTool
                            [ "-id"
                            , fromMaybe
                                ("@loader_path/" <> takeFileName target)
                                copiedInstallName
                            , target
                            ]
                    (nextPending, nextCopied, nextBundledNames) <-
                        foldM
                            ( relocateDependency
                                destination
                                installPrefix
                                target
                            )
                            (remaining, copied, bundledNames)
                            loadedLibraries
                    mapM_ (deleteStoreRpath target) (Set.toList (Set.fromList loadRpaths))
                    relocatePending
                        destination
                        installPrefix
                        nextPending
                        (Set.insert target processed)
                        nextCopied
                        nextBundledNames

relocateDependency ::
    FilePath ->
    String ->
    FilePath ->
    (Seq WorkItem, CopiedLibraries, BundledNames) ->
    FilePath ->
    IO (Seq WorkItem, CopiedLibraries, BundledNames)
relocateDependency destination installPrefix target state@(pending, copied, bundledNames) dependency
    | any (`isPrefixOf` dependency) systemPrefixes = pure state
    | not ("/nix/store/" `isPrefixOf` dependency) =
        die $
            "unsupported non-system dependency in "
                <> target
                <> ": "
                <> dependency
    | otherwise = do
        dependencyExists <- doesFileExist dependency
        unless dependencyExists $
            die $
                "dependency does not exist for "
                    <> target
                    <> ": "
                    <> dependency
        case Map.lookup dependency copied of
            Just (_, installName) -> do
                rewriteDependency target dependency installName
                pure state
            Nothing -> do
                let name = bundledName dependency
                    bundledPath = destination </> name
                    installName = installPrefix <> name
                case Map.lookup name bundledNames of
                    Just otherSource
                        | otherSource /= dependency ->
                            die $
                                "hashed dylib name collision between "
                                    <> otherSource
                                    <> " and "
                                    <> dependency
                    _ -> pure ()
                copyFile dependency bundledPath
                makeWritable bundledPath
                rewriteDependency target dependency installName
                pure
                    ( pending Seq.|> WorkItem bundledPath (Just installName)
                    , Map.insert dependency (bundledPath, installName) copied
                    , Map.insert name dependency bundledNames
                    )

rewriteDependency :: FilePath -> FilePath -> String -> IO ()
rewriteDependency target dependency installName =
    runInstallNameTool ["-change", dependency, installName, target]

deleteStoreRpath :: FilePath -> FilePath -> IO ()
deleteStoreRpath target rpath =
    when ("/nix/store/" `isPrefixOf` rpath) $
        runInstallNameTool ["-delete_rpath", rpath, target]

readLoadCommands :: FilePath -> IO LoadCommands
readLoadCommands path = do
    output <- readChecked "otool" ["-l", path]
    pure (parseLoadCommands output)

parseLoadCommands :: String -> LoadCommands
parseLoadCommands output =
    let (_, libraries, rpaths, installId) =
            foldl parseLine (Nothing, [], [], False) (lines output)
     in LoadCommands
            { loadedLibraries = reverse libraries
            , loadRpaths = reverse rpaths
            , hasInstallId = installId
            }
  where
    parseLine (command, libraries, rpaths, installId) rawLine =
        let line = trim rawLine
         in case stripPrefix "cmd " line of
                Just nextCommand ->
                    ( Just nextCommand
                    , libraries
                    , rpaths
                    , installId || nextCommand == "LC_ID_DYLIB"
                    )
                Nothing
                    | Just activeCommand <- command
                    , activeCommand `Set.member` loadDylibCommands
                    , Just name <- stripPrefix "name " line ->
                        (Nothing, beforeOffset name : libraries, rpaths, installId)
                    | command == Just "LC_RPATH"
                    , Just path <- stripPrefix "path " line ->
                        (Nothing, libraries, beforeOffset path : rpaths, installId)
                    | otherwise ->
                        (command, libraries, rpaths, installId)

runInstallNameTool :: [String] -> IO ()
runInstallNameTool = runChecked_ "install_name_tool"

runChecked_ :: FilePath -> [String] -> IO ()
runChecked_ executable arguments = do
    _ <- readChecked executable arguments
    pure ()

readChecked :: FilePath -> [String] -> IO String
readChecked executable arguments = do
    (exitCode, output, errorOutput) <-
        readProcessWithExitCode executable arguments ""
    case exitCode of
        ExitSuccess -> pure output
        ExitFailure code ->
            die $
                executable
                    <> " failed with exit code "
                    <> show code
                    <> " for arguments "
                    <> show arguments
                    <> if null errorOutput
                        then ""
                        else ":\n" <> errorOutput

makeWritable :: FilePath -> IO ()
makeWritable path = do
    permissions <- getPermissions path
    setPermissions path permissions{writable = True}

bundledName :: FilePath -> FilePath
bundledName source =
    take 16 (concatMap byteHex (BS.unpack (SHA256.hash (BS8.pack source))))
        <> "-"
        <> takeFileName source
  where
    byteHex byte =
        let encoded = showHex byte ""
         in replicate (2 - length encoded) '0' <> encoded

replacePadded :: FilePath -> String -> String -> IO ()
replacePadded path oldString newString = do
    let old = BS8.pack oldString
        new = BS8.pack newString
    when (BS.null old) $
        die "the old value for replace-padded must not be empty"
    when (BS.length new > BS.length old) $
        die "replacement is longer than the original value"
    contents <- BS.readFile path
    let count = countOccurrences old contents
    unless (count == 1) $
        die ("expected one occurrence of the old value, found " <> show count)
    let (prefix, matchingAndSuffix) = BS.breakSubstring old contents
        suffix = BS.drop (BS.length old) matchingAndSuffix
        padding = BS.replicate (BS.length old - BS.length new) 32
    BS.writeFile path (prefix <> new <> padding <> suffix)

countOccurrences :: BS.ByteString -> BS.ByteString -> Int
countOccurrences needle = go 0
  where
    go count remaining =
        let (_, matchingAndSuffix) = BS.breakSubstring needle remaining
         in if BS.null matchingAndSuffix
                then count
                else
                    go
                        (count + 1)
                        (BS.drop (BS.length needle) matchingAndSuffix)

stripPrefix :: String -> String -> Maybe String
stripPrefix prefix value
    | prefix `isPrefixOf` value = Just (drop (length prefix) value)
    | otherwise = Nothing

beforeOffset :: String -> String
beforeOffset value =
    maybe value (`take` value) $
        findIndex (" (offset" `isPrefixOf`) (tails value)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
