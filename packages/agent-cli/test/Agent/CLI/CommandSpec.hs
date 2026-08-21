module Agent.CLI.CommandSpec (spec) where

import Agent.CLI.Command
import Agent.Responses.Types
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (isInfixOf)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
    describe "parseReplLine" do
        it "keeps :q and :quit as quit" do
            parseReplLine ":q" `shouldBe` ReplQuit
            parseReplLine ":quit" `shouldBe` ReplQuit
            parseReplLine "  :quit  " `shouldBe` ReplQuit

        it "treats :reload as a GHCi reload request" do
            parseReplLine ":reload" `shouldBe` ReplReload
            parseReplLine "  :reload  " `shouldBe` ReplReload

        it "sends ordinary lines to the model" do
            parseReplLine "list the files" `shouldBe` ReplPrompt "list the files"
            parseReplLine ":status" `shouldBe` ReplPrompt ":status"

        it "preserves ordinary prompt whitespace exactly" do
            parseReplLine "  indented prompt  "
                `shouldBe` ReplPrompt "  indented prompt  "
            parseReplLine "\n    code\n"
                `shouldBe` ReplPrompt "\n    code\n"
            parseReplLine "  :status  "
                `shouldBe` ReplPrompt "  :status  "

        it "shows the current effort with a bare /effort" do
            parseReplLine "/effort" `shouldBe` ReplShowEffort
            parseReplLine "  /Effort  " `shouldBe` ReplShowEffort

        it "sets a valid effort level" do
            parseReplLine "/effort none" `shouldBe` ReplSetEffort "none"
            parseReplLine "/effort high" `shouldBe` ReplSetEffort "high"
            parseReplLine "/effort XHIGH" `shouldBe` ReplSetEffort "xhigh"
            parseReplLine "/effort MAX" `shouldBe` ReplSetEffort "max"
            parseReplLine "/effort medium" `shouldBe` ReplSetEffort "medium"

        it "toggles always-approve from slash and colon aliases" do
            parseReplLine "/always-approve" `shouldBe` ReplToggleAlwaysApprove
            parseReplLine "/Always-Approve" `shouldBe` ReplToggleAlwaysApprove
            parseReplLine "/yolo" `shouldBe` ReplToggleAlwaysApprove
            parseReplLine ":yolo" `shouldBe` ReplToggleAlwaysApprove
            parseReplLine ":always-approve" `shouldBe` ReplToggleAlwaysApprove

        it "rejects extra args on /always-approve" do
            parseReplLine "/always-approve now"
                `shouldBe` ReplCommandError "usage: /always-approve"
            parseReplLine "/yolo on"
                `shouldBe` ReplCommandError "usage: /always-approve"

        it "prints the current session id" do
            parseReplLine "/session" `shouldBe` ReplShowSession
            parseReplLine "/session now"
                `shouldBe` ReplCommandError "usage: /session"

        it "renames sessions or restores automatic titles" do
            parseReplLine "/rename Fix auth races"
                `shouldBe` ReplRename "Fix auth races"
            parseReplLine "/title   keep  spaces"
                `shouldBe` ReplRename "keep  spaces"
            parseReplLine "/rename --auto" `shouldBe` ReplRenameAuto
            parseReplLine "/rename"
                `shouldBe` ReplCommandError "usage: /rename <TITLE>|--auto"

        it "reloads auth from disk/env" do
            parseReplLine "/reload-auth" `shouldBe` ReplReloadAuth
            parseReplLine "  /Reload-Auth  " `shouldBe` ReplReloadAuth
            parseReplLine "/reload-auth now"
                `shouldBe` ReplCommandError "usage: /reload-auth"

        it "opens the credential manager" do
            parseReplLine "/login" `shouldBe` ReplLogin
            parseReplLine "/accounts" `shouldBe` ReplLogin
            parseReplLine "/login openai"
                `shouldBe` ReplCommandError "usage: /login"

        it "clears or starts a new session" do
            parseReplLine "/clear" `shouldBe` ReplClear
            parseReplLine "/new" `shouldBe` ReplNew
            parseReplLine "/clear now"
                `shouldBe` ReplCommandError "usage: /clear"
            parseReplLine "/new now"
                `shouldBe` ReplCommandError "usage: /new"

        it "compacts with optional focus text" do
            parseReplLine "/compact" `shouldBe` ReplCompact Nothing
            parseReplLine "/compact focus auth"
                `shouldBe` ReplCompact (Just "focus auth")

        it "shows account usage without arguments" do
            parseReplLine "/usage" `shouldBe` ReplUsage
            parseReplLine "/usage extra"
                `shouldBe` ReplCommandError "usage: /usage"

        it "pastes clipboard images with an optional caption" do
            parseReplLine "/paste"
                `shouldBe` ReplPaste { pasteImmediate = False, pasteCaption = "" }
            parseReplLine "  /Paste  "
                `shouldBe` ReplPaste { pasteImmediate = False, pasteCaption = "" }
            parseReplLine "/paste what is this?"
                `shouldBe` ReplPaste
                    { pasteImmediate = False, pasteCaption = "what is this?" }
            parseReplLine "/paste   keep  spaces"
                `shouldBe` ReplPaste
                    { pasteImmediate = False, pasteCaption = "keep  spaces" }
            parseReplLine "/paste --send"
                `shouldBe` ReplPaste { pasteImmediate = True, pasteCaption = "" }
            parseReplLine "/paste --send look"
                `shouldBe` ReplPaste
                    { pasteImmediate = True, pasteCaption = "look" }
            parseReplLine "/attachments" `shouldBe` ReplShowAttachments
            parseReplLine "/clear-attachments" `shouldBe` ReplClearAttachments

        it "parses terminal clipboard commands" do
            parseReplLine "/copy" `shouldBe` ReplCopyLast
            parseReplLine "/copy-last" `shouldBe` ReplCopyLast
            parseReplLine "/copy-code" `shouldBe` ReplCopyCode 1
            parseReplLine "/copy-code 3" `shouldBe` ReplCopyCode 3
            parseReplLine "/copy-diff" `shouldBe` ReplCopyDiff
            parseReplLine "/copy-path" `shouldBe` ReplCopyPath
            parseReplLine "/copy-session" `shouldBe` ReplCopySession
            parseReplLine "/terminal" `shouldBe` ReplShowTerminal
            parseReplLine "/ghostty" `shouldBe` ReplShowTerminal
            parseReplLine "/copy-code nope"
                `shouldBe` ReplCommandError "usage: /copy-code [N]"

        it "opens the model picker with a bare /model" do
            parseReplLine "/model" `shouldBe` ReplShowModel
            parseReplLine "  /Model  " `shouldBe` ReplShowModel
            parseReplLine "/m" `shouldBe` ReplShowModel

        it "sets a model name" do
            parseReplLine "/model grok-4.6" `shouldBe` ReplSetModel "grok-4.6"
            parseReplLine "/m openai/gpt-5.1"
                `shouldBe` ReplSetModel "openai/gpt-5.1"
            parseReplLine "/model openai/gpt-5.1"
                `shouldBe` ReplSetModel "openai/gpt-5.1"

        it "opens the resume picker" do
            parseReplLine "/resume" `shouldBe` ReplResume Nothing
            parseReplLine "/resume abc-123" `shouldBe` ReplResume (Just "abc-123")
            parseReplLine "/resume a b"
                `shouldBe` ReplCommandError "usage: /resume [ID]"

        it "opens the agent hierarchy" do
            parseReplLine "/agents" `shouldBe` ReplAgents
            parseReplLine "/a" `shouldBe` ReplAgents
            parseReplLine "/agents now"
                `shouldBe` ReplCommandError "usage: /agents"

        it "lists slash commands with /help" do
            parseReplLine "/help" `shouldBe` ReplHelp Nothing
            parseReplLine "/help model" `shouldBe` ReplHelp (Just "model")
            parseReplLine "/help /m" `shouldBe` ReplHelp (Just "model")
            parseReplLine "/help bogus"
                `shouldBe` ReplCommandError "unknown command: bogus (try /help)"

        it "rejects extra args on /model" do
            parseReplLine "/model grok-4.6 extra"
                `shouldBe` ReplCommandError "usage: /model [NAME]"

        it "enters plan mode with optional description" do
            parseReplLine "/plan" `shouldBe` ReplPlan Nothing
            parseReplLine "  /Plan  " `shouldBe` ReplPlan Nothing
            parseReplLine "/plan redesign auth"
                `shouldBe` ReplPlan (Just "redesign auth")
            parseReplLine "/plan   keep  spaces"
                `shouldBe` ReplPlan (Just "keep  spaces")

        it "asks a side question with the full suffix" do
            parseReplLine "/btw why this file?"
                `shouldBe` ReplBtw "why this file?"
            parseReplLine "/BTW   keep  spaces"
                `shouldBe` ReplBtw "keep  spaces"
            parseReplLine "/btw"
                `shouldBe` ReplCommandError "usage: /btw <QUESTION>"

        it "rejects unknown levels, extra args, and unknown commands" do
            parseReplLine "/effort bogus"
                `shouldBe` ReplCommandError
                    "effort must be none, low, medium, high, xhigh, or max (got bogus)"
            parseReplLine "/effort high extra"
                `shouldBe` ReplCommandError
                    "usage: /effort [none|low|medium|high|xhigh|max]"
            parseReplLine "/bogus"
                `shouldBe` ReplCommandError "unknown command: /bogus (try /help)"
            parseReplLine "/"
                `shouldBe` ReplCommandError "unknown command: / (try /help)"

    describe "slashCommands" do
        it "covers every slash name the parser accepts" do
            let names = map (.slashName) slashCommands
            names
                `shouldBe`
                    [ "help"
                    , "model"
                    , "effort"
                    , "plan"
                    , "btw"
                    , "session"
                    , "rename"
                    , "login"
                    , "resume"
                    , "compact"
                    , "clear"
                    , "new"
                    , "usage"
                    , "reload-auth"
                    , "paste"
                    , "attachments"
                    , "clear-attachments"
                    , "copy"
                    , "copy-code"
                    , "copy-diff"
                    , "copy-path"
                    , "copy-session"
                    , "terminal"
                    , "agents"
                    , "skills"
                    , "always-approve"
                    ]

        it "looks up aliases" do
            fmap (.slashName) (lookupSlashCommand "m") `shouldBe` Just "model"
            fmap (.slashName) (lookupSlashCommand "/yolo")
                `shouldBe` Just "always-approve"
            fmap (.slashName) (lookupSlashCommand "/accounts")
                `shouldBe` Just "login"
            fmap (.slashName) (lookupSlashCommand "/a")
                `shouldBe` Just "agents"
            fmap (.slashName) (lookupSlashCommand "/title")
                `shouldBe` Just "rename"

        it "completes command names from a leading slash" do
            slashCompletionCandidates "" "/"
                `shouldSatisfy` (\xs ->
                    "/help" `elem` xs
                        && "/model" `elem` xs
                        && "/m" `elem` xs
                        && "/agents" `elem` xs
                        && "/btw" `elem` xs)
            slashCompletionCandidates "" "/mo" `shouldBe` ["/model"]
            slashCompletionCandidates "ledom/" "high" `shouldBe` []

        it "completes effort and model arguments" do
            slashCompletionCandidates "troffe/" "h" `shouldBe` ["high"]
            slashCompletionCandidates "troffe/" "m" `shouldBe` ["medium", "max"]
            slashCompletionCandidates "troffe/" "n" `shouldBe` ["none"]
            slashCompletionCandidates "m/" "grok-4"
                `shouldSatisfy` (\xs ->
                    "grok-4.6" `elem` xs
                        && "grok-4.5" `elem` xs
                        && "grok-4.5-mini" `elem` xs)
            slashCompletionCandidates "emaner/" "-"
                `shouldBe` ["--auto"]

        it "does not complete ordinary prompts" do
            slashCompletionCandidates "" "help" `shouldBe` []
            slashCompletionCandidates (reverse "list the ") "files" `shouldBe` []

        it "opens a live menu on slash and fuzzy-filters command names" do
            let displays text cursor =
                    maybe [] (map (.slashSuggestionDisplay) . (.slashMenuSuggestions))
                        (slashMenuFor text cursor)
            displays "/" 1 `shouldBe` map (("/" <>) . (.slashName)) slashCommands
            displays "/mo" 3 `shouldBe` ["/model"]
            displays "/ra" 3 `shouldSatisfy` ("/reload-auth" `elem`)
            displays "look at /mo" 11 `shouldBe` []

        it "offers argument rows" do
            let menu = slashMenuFor "/effort h" 9
            fmap (.slashMenuReplaceStart) menu `shouldBe` Just 8
            fmap (map (.slashSuggestionDisplay) . (.slashMenuSuggestions)) menu
                `shouldBe` Just ["high", "xhigh"]

        it "replaces the whole token when completing from the middle" do
            fmap (\menu -> (menu.slashMenuReplaceStart, menu.slashMenuReplaceEnd))
                (slashMenuFor "/mofoo" 3)
                `shouldBe` Just (0, 6)
            fmap (\menu -> (menu.slashMenuReplaceStart, menu.slashMenuReplaceEnd))
                (slashMenuFor "/effort hi" 9)
                `shouldBe` Just (8, 10)

        it "does not offer single-argument completions in later slots" do
            slashMenuFor "/effort high " 13 `shouldBe` Nothing
            slashMenuFor "/help model " 12 `shouldBe` Nothing
            slashMenuFor "/paste --send " 14 `shouldBe` Nothing

        it "renders /help with usage and summary" do
            let listing = Text.unpack (formatSlashHelp False Nothing)
            listing `shouldSatisfy` ("/model [NAME]" `isInfixOf`)
            listing `shouldSatisfy` ("Open the model picker" `isInfixOf`)
            listing `shouldSatisfy` ("preview it in the terminal" `isInfixOf`)
            listing `shouldSatisfy` ("(/m)" `isInfixOf`)
            listing `shouldSatisfy` ("/btw <QUESTION>" `isInfixOf`)
            listing `shouldSatisfy` ("/agents" `isInfixOf`)
            listing `shouldSatisfy` ("/usage" `isInfixOf`)
            Text.unpack (formatSlashHelp False (Just "effort"))
                `shouldSatisfy`
                    ("/effort [none|low|medium|high|xhigh|max]" `isInfixOf`)

    describe "runtime skill commands" do
        let skills =
                [ SkillCommand
                    { skillCommandName = "deploy"
                    , skillCommandSummary = "Deploy the service"
                    , skillCommandArgumentHint = Just "<environment>"
                    , skillCommandSource = "repo · agents"
                    }
                ]

        it "parses a skill invocation and preserves arguments" do
            parseReplLineWithSkills skills "/deploy production now"
                `shouldBe` ReplInvokeSkill "deploy" "production now"

        it "parses the skills listing and reload commands" do
            parseReplLine "/skills" `shouldBe` ReplSkills False
            parseReplLine "/skills reload" `shouldBe` ReplSkills True
            parseReplLine "/skills nope"
                `shouldBe` ReplCommandError "usage: /skills [reload]"

        it "adds skills to completion, the live menu, and help" do
            slashCompletionCandidatesWithSkills skills "" "/de"
                `shouldBe` ["/deploy"]
            fmap (map (.slashSuggestionDisplay) . (.slashMenuSuggestions))
                (slashMenuForWithSkills skills "/dep" 4)
                `shouldBe` Just ["/deploy"]
            let help = formatSlashHelpWithSkills False skills (Just "deploy")
            Text.unpack help `shouldSatisfy` ("Deploy the service" `isInfixOf`)
            Text.unpack help `shouldSatisfy` ("skill · repo · agents" `isInfixOf`)

    describe "setReasoningEffort" do
        it "writes effort onto an empty reasoning config" do
            let updated = setReasoningEffort "high" defaultResponseCreateParams
            currentEffort updated `shouldBe` "high"
            fmap (.effort) updated.reasoning `shouldBe` Just (Just "high")

        it "preserves other reasoning fields" do
            let original = case defaultResponseCreateParams of
                    ResponseCreateParams{..} -> ResponseCreateParams
                        { reasoning = Just ReasoningConfig
                            { context = Just "256k"
                            , effort = Just "low"
                            , generateSummary = Just "auto"
                            , reasoningMode = Nothing
                            , summary = Just "concise"
                            , extraFields = KeyMap.empty
                            }
                        , ..
                        }
                updated = setReasoningEffort "xhigh" original
            currentEffort updated `shouldBe` "xhigh"
            case updated.reasoning of
                Just config -> do
                    config.context `shouldBe` Just "256k"
                    config.generateSummary `shouldBe` Just "auto"
                    config.summary `shouldBe` Just "concise"
                Nothing -> expectationFailure "expected reasoning config"

    describe "currentEffort" do
        it "defaults to low when reasoning is missing" do
            currentEffort defaultResponseCreateParams `shouldBe` "low"

    describe "setModel" do
        it "writes the model onto request params" do
            let updated = setModel "grok-4.6" defaultResponseCreateParams
            currentModel updated `shouldBe` "grok-4.6"
            updated.model `shouldBe` Just "grok-4.6"

        it "preserves other request fields" do
            let original = case defaultResponseCreateParams of
                    ResponseCreateParams{..} -> ResponseCreateParams
                        { model = Just "old-model"
                        , instructions = Just "keep me"
                        , store = Just True
                        , ..
                        }
                updated = setModel "new-model" original
            currentModel updated `shouldBe` "new-model"
            updated.instructions `shouldBe` Just "keep me"
            updated.store `shouldBe` Just True

    describe "currentModel" do
        it "defaults to (unset) when model is missing" do
            currentModel defaultResponseCreateParams `shouldBe` "(unset)"
