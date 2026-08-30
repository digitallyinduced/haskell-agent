module Agent.Tools.DangerousSpec (spec) where

import Agent.Tools.Dangerous
    ( commandLooksLikeRmRf
    , commandEscapesSessionTempVariable
    , commandUsesHardcodedSystemTmp
    , commandUsesHardcodedSystemTmpAt
    , forbiddenRmRfReason
    , hardcodedSystemTmpReason
    , shellCommandBlocked
    )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Control.Exception.Safe (tryAny)
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Files (deviceID, fileID, getFileStatus)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
    ( Arbitrary(..)
    , choose
    , conjoin
    , elements
    , (===)
    )

newtype DangerousCommand = DangerousCommand Text.Text
    deriving (Show)

instance Arbitrary DangerousCommand where
    arbitrary = DangerousCommand <$> do
        prefix <- elements
            [ ""
            , "true; "
            , "ls && "
            , "echo hi | "
            ]
        environment <- elements
            [ ""
            , "FOO=1 "
            , "FOO=1 BAR=2 "
            ]
        wrapper <- elements
            [ ""
            , "sudo "
            , "env "
            , "command "
            , "nice "
            , "nohup "
            , "time "
            , "stdbuf "
            ]
        command <- elements ["rm", "RM", "/bin/rm", "rm.exe"]
        flags <- elements
            [ "-rf"
            , "-fr"
            , "-Rf"
            , "-fR"
            , "-r -f"
            , "-f -r"
            , "--recursive --force"
            , "--force --recursive"
            , "--recursive=true --force=false"
            ]
        spacing <- elements [" ", "  ", "\t"]
        target <- elements ["tmp", "./build", "/tmp/x", "C:\\Temp\\x"]
        suffix <- elements ["", " || true", "; true"]
        pure
            ( prefix
                <> environment
                <> wrapper
                <> command
                <> spacing
                <> flags
                <> spacing
                <> target
                <> suffix
            )

newtype BenignCommand = BenignCommand Text.Text
    deriving (Show)

instance Arbitrary BenignCommand where
    arbitrary = BenignCommand <$> do
        choice <- choose (0 :: Int, 4)
        case choice of
            0 -> do
                flag <- elements ["", "-r", "-f", "--recursive", "--force"]
                pure ("rm " <> flag <> " build")
            1 -> elements
                [ "ls -la"
                , "git status"
                , "rmdir empty-dir"
                , "rclone sync --force"
                ]
            2 -> elements
                [ "echo 'rm -rf tmp'"
                , "echo \"rm -rf tmp\""
                , "bash -lc 'rm -rf tmp'"
                ]
            3 -> do
                wrapper <- elements ["sudo ", "env ", "command "]
                pure (wrapper <> "rm -r build")
            _ -> pure "rm -r -x file"

spec :: Spec
spec = do
    describe "commandLooksLikeRmRf" do
        it "detects clustered and split short flags" do
            commandLooksLikeRmRf "rm -rf /tmp/x" `shouldBe` True
            commandLooksLikeRmRf "rm -fr ./build" `shouldBe` True
            commandLooksLikeRmRf "rm -r -f out" `shouldBe` True
            commandLooksLikeRmRf "rm -f -r out" `shouldBe` True
            commandLooksLikeRmRf "rm -Rf /tmp/x" `shouldBe` True
            commandLooksLikeRmRf "rm -vfr cache" `shouldBe` True

        it "detects long options" do
            commandLooksLikeRmRf "rm --recursive --force out" `shouldBe` True
            commandLooksLikeRmRf "rm --force --recursive out" `shouldBe` True

        it "detects absolute rm paths and .exe" do
            commandLooksLikeRmRf "/bin/rm -rf /tmp/x" `shouldBe` True
            commandLooksLikeRmRf "rm.exe -rf C:\\Temp\\x" `shouldBe` True

        it "detects chained and piped forms" do
            commandLooksLikeRmRf "ls && rm -rf tmp" `shouldBe` True
            commandLooksLikeRmRf "true; rm -rf tmp" `shouldBe` True
            commandLooksLikeRmRf "echo hi | rm -rf tmp" `shouldBe` True
            commandLooksLikeRmRf "rm -rf a || true" `shouldBe` True

        it "detects common wrappers and env assignments" do
            commandLooksLikeRmRf "sudo rm -rf /var/tmp/x" `shouldBe` True
            commandLooksLikeRmRf "env rm -rf ./cache" `shouldBe` True
            commandLooksLikeRmRf "FOO=1 BAR=2 rm -rf ./cache" `shouldBe` True
            commandLooksLikeRmRf "nohup rm -rf ./cache" `shouldBe` True
            commandLooksLikeRmRf "command rm -rf ./cache" `shouldBe` True

        it "allows safe rm and unrelated commands" do
            commandLooksLikeRmRf "rm -r build" `shouldBe` False
            commandLooksLikeRmRf "rm -f file.txt" `shouldBe` False
            commandLooksLikeRmRf "rm --recursive build" `shouldBe` False
            commandLooksLikeRmRf "rm --force build" `shouldBe` False
            commandLooksLikeRmRf "rm file.txt" `shouldBe` False
            commandLooksLikeRmRf "ls -la" `shouldBe` False
            commandLooksLikeRmRf "git status" `shouldBe` False
            commandLooksLikeRmRf "echo 'rm -rf tmp'" `shouldBe` False
            commandLooksLikeRmRf "echo \"rm -rf tmp\"" `shouldBe` False
            commandLooksLikeRmRf "rmdir empty-dir" `shouldBe` False
            commandLooksLikeRmRf "rclone sync --force" `shouldBe` False
            -- Nested shells are out of scope for this best-effort gate.
            commandLooksLikeRmRf "bash -lc 'rm -rf tmp'" `shouldBe` False

        modifyMaxSuccess (const 500) $
            prop "detects generated equivalent recursive-force forms" $
                \(DangerousCommand command) ->
                    commandLooksLikeRmRf command === True

        modifyMaxSuccess (const 500) $
            prop "does not classify generated benign near-misses as dangerous" $
                \(BenignCommand command) ->
                    commandLooksLikeRmRf command === False

    describe "commandUsesHardcodedSystemTmp" do
        it "recognizes system temp paths in common shell positions" do
            commandUsesHardcodedSystemTmp "touch /tmp/result.png"
                `shouldBe` True
            commandUsesHardcodedSystemTmp "OUT=/tmp/result.png make"
                `shouldBe` True
            commandUsesHardcodedSystemTmp "cat '/private/tmp/input.txt'"
                `shouldBe` True
            commandUsesHardcodedSystemTmp "cd /tmp"
                `shouldBe` True
            commandUsesHardcodedSystemTmp "touch ///tmp/result.png"
                `shouldBe` True
            commandUsesHardcodedSystemTmp "cat /private//tmp/input.txt"
                `shouldBe` True

        it "normalizes absolute aliases of the shared temp roots" do
            map commandUsesHardcodedSystemTmp
                [ "touch /usr/../tmp/output"
                , "cat /var/../private/tmp/input"
                , "touch /var/cache/../../tmp/output"
                , "cat '/usr/local/../../private//tmp/input'"
                ]
                `shouldBe` replicate 4 True

        it "resolves relative aliases against the shell working directory" do
            let cwd = unsafeEncodeUtf "/usr/local"
            mapM (commandUsesHardcodedSystemTmpAt cwd)
                [ "cat ../../tmp/other-session"
                , "cat ../../private/tmp/other-session"
                , "cat ../local/../../tmp/other-session"
                ]
                `shouldReturn` replicate 3 True
            mapM (commandUsesHardcodedSystemTmpAt cwd)
                [ "cat ../tmp/project-file"
                , "cat ../../tmpfile"
                , "curl https://example.test/../../tmp/file"
                ]
                `shouldReturn` replicate 3 False

        it "matches case variants only when the host filesystem aliases them" do
            aliasesCase <- pathsReferToSameFile "/tmp" "/TMP"
            let cwd = unsafeEncodeUtf "/usr/local"
            mapM (commandUsesHardcodedSystemTmpAt cwd)
                [ "cat /TMP/other-session"
                , "cat /PRIVATE/TMP/other-session"
                , "cat /private/TmP/other-session"
                , "curl file:///TMP/other-session"
                , "cat /usr/../TmP/other-session"
                ]
                `shouldReturn` replicate 5 aliasesCase
            commandUsesHardcodedSystemTmp "cat /TMP/other-session"
                `shouldBe` False
            commandUsesHardcodedSystemTmp "cat /TMPfile"
                `shouldBe` False

        it "follows simple shell cwd changes before checking relative aliases" do
            let cwd = unsafeEncodeUtf "/usr/local"
            commandUsesHardcodedSystemTmpAt cwd
                "cd ../..; cat tmp/other-session"
                `shouldReturn` True
            commandUsesHardcodedSystemTmpAt cwd
                "cd /; cat private/tmp/other-session"
                `shouldReturn` True

        it "preserves symlink semantics in normalized shell paths" do
            aliases <- pathsReferToSameFile "/bin/../tmp" "/tmp"
            commandUsesHardcodedSystemTmpAt
                (unsafeEncodeUtf "/")
                "cat /bin/../tmp/other-session"
                `shouldReturn` aliases

        it "blocks local file URLs targeting shared temp" do
            map commandUsesHardcodedSystemTmp
                [ "curl file:///tmp/other-session"
                , "curl file:/tmp/other-session"
                , "curl FILE:///private/tmp/other-session"
                , "curl file://localhost/tmp/other-session"
                , "curl file:///usr/../tmp/other-session"
                , "curl file:///%74mp/other-session"
                , "curl file:///%2fprivate%2ftmp/other-session"
                , "curl file:%2ftmp/other-session"
                , "curl file:%2Fprivate%2Ftmp/other-session"
                , "curl file:%2fusr%2f..%2ftmp/other-session"
                ]
                `shouldBe` replicate 10 True

        it "allows session temp variables and unrelated tmp path components" do
            commandUsesHardcodedSystemTmp "touch \"$TMPDIR/result.png\""
                `shouldBe` False
            commandUsesHardcodedSystemTmp
                "touch \"$HASKELL_AGENT_TMPDIR/result.png\""
                `shouldBe` False
            commandUsesHardcodedSystemTmp "echo /tmpfile"
                `shouldBe` False
            commandUsesHardcodedSystemTmp "cat build/tmp/result.png"
                `shouldBe` False
            commandUsesHardcodedSystemTmp "curl https://example.test/tmp/file"
                `shouldBe` False
            commandUsesHardcodedSystemTmp
                "curl https://example.test/usr/../tmp/file"
                `shouldBe` False
            commandUsesHardcodedSystemTmp
                "curl https://example.test/%74mp/file"
                `shouldBe` False
            commandUsesHardcodedSystemTmp "curl file:///tmpfile"
                `shouldBe` False
            commandUsesHardcodedSystemTmp "curl file:///var/tmp/file"
                `shouldBe` False
            commandUsesHardcodedSystemTmp "cat /usr/../tmpfile"
                `shouldBe` False
            commandUsesHardcodedSystemTmp "cat /usr/../private/tmpfile"
                `shouldBe` False

        it "rejects traversal outside session temp variables" do
            map commandEscapesSessionTempVariable
                [ "ls \"$TMPDIR/..\""
                , "cat ${TMPDIR}/../other-session/secret"
                , "cat \"$HASKELL_AGENT_TMPDIR/a/../../secret\""
                , "cat \"$TMPDIR-other-session/secret\""
                , "cat \"${TMPDIR}\"../other-session/secret"
                , "cd \"$TMPDIR\"; cat ../other-session/secret"
                , "cd \"$TMPDIR\"/nested; cat ../../other-session/secret"
                , "cd \"${HASKELL_AGENT_TMPDIR}/nested\"; cd ../.."
                ]
                `shouldBe` replicate 8 True
            map commandEscapesSessionTempVariable
                [ "cat \"$TMPDIR/result\""
                , "cat \"$TMPDIR\"/result"
                , "cat \"$TMPDIR/build/../result\""
                , "cd \"$TMPDIR/nested\"; cat ../result"
                ]
                `shouldBe` replicate 4 False

    describe "shellCommandBlocked" do
        it "blocks shell tools with a clear reason" do
            shouldBlock "run_terminal_cmd" "{\"command\":\"rm -rf /tmp/x\"}"
            shouldBlock "run_terminal_command" "{\"command\":\"rm -rf /tmp/x\"}"
            shouldBlock "shell_command" "{\"command\":\"rm -fr ./build\"}"
            shouldBlock "run_terminal_cmd" "{\"command\":\"ls && rm -rf tmp\"}"

        it "includes a truncated command snippet in the reason" do
            let msg = forbiddenRmRfReason "rm -rf /tmp/very-important"
            msg `shouldSatisfy` Text.isInfixOf "Blocked dangerous shell command"
            msg `shouldSatisfy` Text.isInfixOf "rm -rf /tmp/very-important"

        it "rejects hardcoded system temp paths before approval" do
            let args = "{\"command\":\"convert in.svg /tmp/out.png\"}"
            case shellCommandBlocked "shell_command" args of
                Just msg -> do
                    msg `shouldSatisfy`
                        Text.isInfixOf "Blocked hardcoded system temp path"
                    msg `shouldSatisfy` Text.isInfixOf "$TMPDIR"
                Nothing -> expectationFailure "expected hardcoded temp block"
            shellCommandBlocked "monitor"
                "{\"command\":\"tail -f /private/tmp/events\"}"
                `shouldSatisfy`
                    maybe False
                        (Text.isInfixOf "Blocked hardcoded system temp path")

        it "includes a truncated command snippet in the temp-path reason" do
            let msg = hardcodedSystemTmpReason "cat /tmp/session.log"
            msg `shouldSatisfy` Text.isInfixOf "cat /tmp/session.log"

        it "parses JSON with whitespace around the command value" do
            shouldBlock "run_terminal_cmd" "{\"command\" : \"rm -rf x\"}"

        modifyMaxSuccess (const 500) $
            prop "blocks dangerous generated commands for every shell tool name" $
                \(DangerousCommand command) ->
                    let arguments = encodeCommand command
                    in conjoin
                        [ isBlocked (shellCommandBlocked tool arguments)
                            === True
                        | tool <-
                            [ "monitor"
                            , "run_terminal_cmd"
                            , "run_terminal_command"
                            , "shell_command"
                            ]
                        ]

        it "allows non-matching shell commands" do
            shouldAllow "run_terminal_cmd" "{\"command\":\"rm -r build\"}"
            shouldAllow "run_terminal_cmd" "{\"command\":\"rm file.txt\"}"
            shouldAllow "shell_command" "{\"command\":\"ls -la\"}"

        it "ignores non-shell tools even if arguments look dangerous" do
            shellCommandBlocked "search_replace" "{\"command\":\"rm -rf x\"}"
                `shouldBe` Nothing
            shellCommandBlocked "read_file" "{\"command\":\"rm -rf x\"}"
                `shouldBe` Nothing

        it "does nothing when the command field is missing" do
            shellCommandBlocked "run_terminal_cmd" "{\"description\":\"nope\"}"
                `shouldBe` Nothing

  where
    shouldBlock tool args =
        case shellCommandBlocked tool args of
            Just msg -> msg `shouldSatisfy` Text.isInfixOf "Blocked dangerous shell command"
            Nothing -> expectationFailure ("expected block for " <> Text.unpack args)
    shouldAllow tool args =
        shellCommandBlocked tool args `shouldBe` Nothing

    isBlocked (Just message) =
        Text.isInfixOf "Blocked dangerous shell command" message
    isBlocked Nothing = False

    encodeCommand command =
        TextEncoding.decodeUtf8
            (LazyByteString.toStrict
                (Aeson.encode (Aeson.object ["command" Aeson..= command])))

pathsReferToSameFile :: FilePath -> FilePath -> IO Bool
pathsReferToSameFile left right =
    tryAny
        ((,)
            <$> getFileStatus left
            <*> getFileStatus right) >>= \case
        Left _ -> pure False
        Right (leftStatus, rightStatus) ->
            pure $
                deviceID leftStatus == deviceID rightStatus
                    && fileID leftStatus == fileID rightStatus
