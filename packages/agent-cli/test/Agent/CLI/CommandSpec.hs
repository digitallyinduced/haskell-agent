module Agent.CLI.CommandSpec (spec) where

import Agent.CLI.Command
import Agent.CLI.Afk
import Agent.Dialect (DialectId(..))
import Agent.ReasoningEffort (ReasoningEffort(..))
import Agent.Responses.Types
import Data.List (isInfixOf)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
    describe "parseReplLine" do
        it "recognizes bare shell- and Vim-style exit aliases" do
            map parseReplLine
                [ "exit"
                , "quit"
                , "EXIT"
                , " Quit "
                , ":q"
                , ":Q!"
                , ":quit"
                , ":wq"
                , ":WQ!"
                ]
                `shouldBe` replicate 9 ReplQuit
            parseReplLine "/quit" `shouldBe` ReplQuit
            parseReplLine "/exit" `shouldBe` ReplQuit

        it "does not exit for near matches" do
            map parseReplLine ["exiting", "quite", "q", "wq", ":w", ":x"]
                `shouldBe`
                    map ReplPrompt ["exiting", "quite", "q", "wq", ":w", ":x"]

        it "treats :reload as a GHCi reload request" do
            parseReplLine ":reload" `shouldBe` ReplReload
            parseReplLine "  :reload  " `shouldBe` ReplReload

        it "parses update-and-restart without arguments" do
            parseReplLine "/update-and-restart"
                `shouldBe` ReplUpdateAndRestart
            parseReplLine "/update-and-restart now"
                `shouldBe`
                    ReplCommandError "usage: /update-and-restart"

        it "parses exact-turn retry" do
            parseReplLine "/retry" `shouldBe` ReplRetry
            parseReplLine "/retry now"
                `shouldBe` ReplCommandError "usage: /retry"

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

        it "treats absolute paths as prompt text rather than slash commands" do
            parseReplLine "/Users/marc/Downloads/template.png"
                `shouldBe` ReplPrompt "/Users/marc/Downloads/template.png"
            parseReplLine
                "  /Users/marc/Downloads/template.png use this template  "
                `shouldBe` ReplPrompt
                    "  /Users/marc/Downloads/template.png use this template  "

        it "shows the current effort with a bare /effort" do
            parseReplLine "/effort" `shouldBe` ReplShowEffort
            parseReplLine "  /Effort  " `shouldBe` ReplShowEffort

        it "toggles Fast mode with /fast" do
            let catalog = mkSlashCatalog True CodexDialect [] [] []
            parseReplLineWithCatalog catalog "/fast"
                `shouldBe` ReplToggleFast
            parseReplLineWithCatalog catalog "/FAST"
                `shouldBe` ReplToggleFast
            parseReplLineWithCatalog catalog "/fast on"
                `shouldBe` ReplCommandError "usage: /fast"

        it "sets a valid effort level" do
            parseReplLine "/effort none" `shouldBe` ReplSetEffort EffortNone
            parseReplLine "/effort high" `shouldBe` ReplSetEffort EffortHigh
            parseReplLine "/effort XHIGH" `shouldBe` ReplSetEffort EffortXHigh
            parseReplLine "/effort MAX" `shouldBe` ReplSetEffort EffortMax
            parseReplLine "/effort medium" `shouldBe` ReplSetEffort EffortMedium

        it "parses the Codex workflow commands" do
            parseReplLine "/init" `shouldBe` ReplInit
            parseReplLine "/init now"
                `shouldBe` ReplCommandError "usage: /init"
            parseReplLine "/review" `shouldBe` ReplReview Nothing
            parseReplLine "/review   inspect auth  "
                `shouldBe` ReplReview (Just "inspect auth")
            parseReplLine "/diff" `shouldBe` ReplDiff
            parseReplLine "/diff now"
                `shouldBe` ReplCommandError "usage: /diff"
            parseReplLine "/fork"
                `shouldBe` ReplFork (ForkRequest Nothing Nothing)
            parseReplLine "/fork   experiment branch  "
                `shouldBe`
                    ReplFork
                        (ForkRequest Nothing (Just "experiment branch"))
            parseReplLine "/fork --worktree fix the tests"
                `shouldBe`
                    ReplFork
                        (ForkRequest (Just True) (Just "fix the tests"))
            parseReplLine "/fork --no-worktree"
                `shouldBe` ReplFork (ForkRequest (Just False) Nothing)
            parseReplLine "/fork --unknown stays a directive"
                `shouldBe`
                    ReplFork
                        (ForkRequest
                            Nothing
                            (Just "--unknown stays a directive"))
            parseReplLine "/fork --worktree --no-worktree"
                `shouldBe`
                    ReplCommandError
                        "--worktree and --no-worktree are mutually exclusive"
            parseReplLine "/fork --worktree --worktree"
                `shouldBe`
                    ReplCommandError "--worktree specified twice"
            parseReplLine "/fork --at 3"
                `shouldBe`
                    ReplCommandError
                        "--at is not supported in this version"
            parseReplLine "/export" `shouldBe` ReplExport Nothing
            parseReplLine "/export  notes/session.md  "
                `shouldBe` ReplExport (Just "notes/session.md")
            parseReplLine "/history" `shouldBe` ReplHistory
            parseReplLine "/history now"
                `shouldBe` ReplCommandError "usage: /history"
            parseReplLine "/find" `shouldBe` ReplFind Nothing
            parseReplLine "/find   exact  phrase"
                `shouldBe` ReplFind (Just "exact  phrase")
            parseReplLine "/permissions" `shouldBe` ReplPermissions
            parseReplLine "/permissions now"
                `shouldBe` ReplCommandError "usage: /permissions"

        it "includes the no-overwrite AGENTS.md init prompt" do
            initInstruction `shouldSatisfy` Text.isInfixOf
                "Before writing, check whether AGENTS.md already exists"
            initInstruction `shouldSatisfy` Text.isInfixOf
                "Repository Guidelines"

        it "toggles always-approve from slash and colon aliases" do
            parseReplLine "/always-approve" `shouldBe` ReplToggleAlwaysApprove
            parseReplLine "/Always-Approve" `shouldBe` ReplToggleAlwaysApprove
            parseReplLine "/yolo" `shouldBe` ReplToggleAlwaysApprove
            parseReplLine ":yolo" `shouldBe` ReplToggleAlwaysApprove
            parseReplLine ":always-approve" `shouldBe` ReplToggleAlwaysApprove

        it "shows or selects the runtime shell tools" do
            parseReplLine "/shell" `shouldBe` ReplShowShell
            parseReplLine "/shell ghci" `shouldBe` ReplSetShell ShellGhci
            parseReplLine "/shell BASH" `shouldBe` ReplSetShell ShellBash
            parseReplLine "/shell both" `shouldBe` ReplSetShell ShellBoth
            parseReplLine "/shell none" `shouldBe` ReplSetShell ShellNone
            parseReplLine "/shell fish"
                `shouldBe` ReplCommandError
                    "usage: /shell [ghci|bash|both|none]"

        it "parses runtime code-mode enablement" do
            parseReplLine "/codemod" `shouldBe` ReplEnableCodeMode
            parseReplLine "/code-mode" `shouldBe` ReplEnableCodeMode
            parseReplLine "/codemod now"
                `shouldBe` ReplCommandError "usage: /codemod"

        it "rejects extra args on /always-approve" do
            parseReplLine "/always-approve now"
                `shouldBe` ReplCommandError "usage: /always-approve"
            parseReplLine "/yolo on"
                `shouldBe` ReplCommandError "usage: /always-approve"

        it "prints the current session id" do
            parseReplLine "/session" `shouldBe` ReplShowSession
            parseReplLine "/session now"
                `shouldBe` ReplCommandError "usage: /session"

        it "shows expanded session information through compatibility aliases" do
            parseReplLine "/session-info" `shouldBe` ReplShowSessionInfo
            parseReplLine "/status" `shouldBe` ReplShowSessionInfo
            parseReplLine "/info" `shouldBe` ReplShowSessionInfo
            parseReplLine "/status now"
                `shouldBe` ReplCommandError "usage: /session-info"

        it "opens the current conversation in the desktop app" do
            parseReplLine "/desktop" `shouldBe` ReplDesktop
            parseReplLine "  /Desktop  " `shouldBe` ReplDesktop
            parseReplLine "/desktop now"
                `shouldBe` ReplCommandError "usage: /desktop"

        it "hands the session to local or remote tmux" do
            parseReplLine "/afk" `shouldBe` ReplAfk Nothing
            parseReplLine "/afk office-builder:~/haskell-agent"
                `shouldBe` ReplAfk (Just "office-builder:~/haskell-agent")
            parseReplLine "/afk one two"
                `shouldBe` ReplCommandError "usage: /afk [HOST:PATH]"

        it "starts a fresh session in a new worktree" do
            parseReplLine "/worktree" `shouldBe` ReplWorktree
            parseReplLine "  /Worktree  " `shouldBe` ReplWorktree
            parseReplLine "/worktree now"
                `shouldBe` ReplCommandError "usage: /worktree"

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

        it "clears, starts, or deletes a session" do
            parseReplLine "/clear" `shouldBe` ReplClear
            parseReplLine "/new" `shouldBe` ReplNew
            parseReplLine "/delete" `shouldBe` ReplDelete
            parseReplLine "/clear now"
                `shouldBe` ReplCommandError "usage: /clear"
            parseReplLine "/new now"
                `shouldBe` ReplCommandError "usage: /new"
            parseReplLine "/delete now"
                `shouldBe` ReplCommandError "usage: /delete"

        it "returns home or rewinds through Grok-compatible aliases" do
            parseReplLine "/home" `shouldBe` ReplHome
            parseReplLine "/welcome" `shouldBe` ReplHome
            parseReplLine "/rewind" `shouldBe` ReplRewind
            parseReplLine "/undo" `shouldBe` ReplRewind
            parseReplLine "/home now"
                `shouldBe` ReplCommandError "usage: /home"
            parseReplLine "/undo now"
                `shouldBe` ReplCommandError "usage: /rewind"

        it "opens the changelog without arguments" do
            parseReplLine "/changelog" `shouldBe` ReplChangelog
            parseReplLine "/changelog now"
                `shouldBe` ReplCommandError "usage: /changelog"

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
                `shouldBe` ReplPaste False ""
            parseReplLine "  /Paste  "
                `shouldBe` ReplPaste False ""
            parseReplLine "/paste what is this?"
                `shouldBe` ReplPaste False "what is this?"
            parseReplLine "/paste   keep  spaces"
                `shouldBe` ReplPaste False "keep  spaces"
            parseReplLine "/paste --send"
                `shouldBe` ReplPaste True ""
            parseReplLine "/paste --send look"
                `shouldBe` ReplPaste True "look"
            parseReplLine "/attachments" `shouldBe` ReplShowAttachments
            parseReplLine "/clear-attachments" `shouldBe` ReplClearAttachments

        it "parses terminal clipboard commands" do
            parseReplLine "/copy"
                `shouldBe` ReplCopy (CopyRequest 1 Nothing)
            parseReplLine "/copy-last"
                `shouldBe` ReplCopy (CopyRequest 1 Nothing)
            parseReplLine "/copy 3"
                `shouldBe` ReplCopy (CopyRequest 3 Nothing)
            parseReplLine "/copy out.txt"
                `shouldBe` ReplCopy (CopyRequest 1 (Just "out.txt"))
            parseReplLine "/copy 2 ~/exports/my note.md"
                `shouldBe`
                    ReplCopy
                        (CopyRequest 2 (Just "~/exports/my note.md"))
            parseReplLine "/copy 0"
                `shouldBe`
                    ReplCommandError
                        "usage: /copy [N] [PATH] where N is 1 (latest), 2, 3, ..."
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

        it "opens and selects a theme" do
            parseReplLine "/theme" `shouldBe` ReplShowTheme
            parseReplLine "/t" `shouldBe` ReplShowTheme
            parseReplLine "/theme TokyoNight"
                `shouldBe` ReplSetTheme "TokyoNight"
            parseReplLine "/t daylight"
                `shouldBe` ReplSetTheme "daylight"

        it "rejects extra theme arguments" do
            parseReplLine "/theme tokyo night"
                `shouldBe` ReplCommandError "usage: /theme [NAME]"

        it "opens the resume picker" do
            parseReplLine "/resume" `shouldBe` ReplResume Nothing
            parseReplLine "/resume abc-123" `shouldBe` ReplResume (Just "abc-123")
            parseReplLine "/resume a b"
                `shouldBe` ReplCommandError "usage: /resume [ID]"

        it "searches past conversations with the full query suffix" do
            parseReplLine "/search postgres migration"
                `shouldBe` ReplSearch "postgres migration"
            parseReplLine "/SEARCH   keep  spaces"
                `shouldBe` ReplSearch "keep  spaces"
            parseReplLine "/search"
                `shouldBe` ReplCommandError "usage: /search <QUERY>"

        it "opens the agent hierarchy" do
            parseReplLine "/agents" `shouldBe` ReplAgents
            parseReplLine "/a" `shouldBe` ReplAgents
            parseReplLine "/agents now"
                `shouldBe` ReplCommandError "usage: /agents [limit [N]]"

        it "shows or sets the concurrent agent limit" do
            parseReplLine "/agents limit" `shouldBe` ReplShowAgentLimit
            parseReplLine "/agents limit 64" `shouldBe` ReplSetAgentLimit 64
            parseReplLine "/agents limit 0"
                `shouldBe` ReplCommandError "usage: /agents [limit [N]]"
            parseReplLine "/agents limit nope"
                `shouldBe` ReplCommandError "usage: /agents [limit [N]]"

        it "opens the MCP server manager" do
            parseReplLine "/mcp" `shouldBe` ReplMcp
            parseReplLine "/MCP" `shouldBe` ReplMcp
            parseReplLine "/mcps" `shouldBe` ReplMcp
            parseReplLine "/mcp now"
                `shouldBe` ReplCommandError "usage: /mcp [prompt <server> <prompt> [key=value ...]]"

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

        it "parses session inspection and prompt editing commands" do
            parseReplLine "/view-plan" `shouldBe` ReplViewPlan
            parseReplLine "/show-plan" `shouldBe` ReplViewPlan
            parseReplLine "/plan-view" `shouldBe` ReplViewPlan
            parseReplLine "/queue" `shouldBe` ReplQueue
            parseReplLine "/transcript" `shouldBe` ReplTranscript
            parseReplLine "/log" `shouldBe` ReplTranscript
            parseReplLine "/edit-prompt" `shouldBe` ReplEditPrompt
            parseReplLine "/context" `shouldBe` ReplContext
            parseReplLine "/view-plan now"
                `shouldBe` ReplCommandError "usage: /view-plan"
            parseReplLine "/queue now"
                `shouldBe` ReplCommandError "usage: /queue"
            parseReplLine "/transcript now"
                `shouldBe` ReplCommandError "usage: /transcript"
            parseReplLine "/edit-prompt now"
                `shouldBe` ReplCommandError "usage: /edit-prompt"
            parseReplLine "/context now"
                `shouldBe` ReplCommandError "usage: /context"

        it "asks a side question with the full suffix" do
            parseReplLine "/btw why this file?"
                `shouldBe` ReplBtw "why this file?"
            parseReplLine "/BTW   keep  spaces"
                `shouldBe` ReplBtw "keep  spaces"
            parseReplLine "/btw"
                `shouldBe` ReplCommandError "usage: /btw <QUESTION>"

        it "opens the portable Meta Console with the full request" do
            parseReplLine "/meta connect my Grok account"
                `shouldBe` ReplMetaConsole "connect my Grok account"
            parseReplLine "/CONFIGURE   add  this MCP"
                `shouldBe` ReplMetaConsole "add  this MCP"
            parseReplLine "/meta"
                `shouldBe` ReplCommandError "usage: /meta <REQUEST>"

        it "requests a session recap" do
            parseReplLine "/recap" `shouldBe` ReplRecap
            parseReplLine "/summarize" `shouldBe` ReplRecap
            parseReplLine "/recap now"
                `shouldBe` ReplCommandError "usage: /recap"

        it "rejects unknown levels, extra args, and unknown commands" do
            parseReplLine "/effort bogus"
                `shouldBe` ReplCommandError
                    "effort must be none, low, medium, high, xhigh, or max (got bogus)"
            parseReplLine "/effort high extra"
                `shouldBe` ReplCommandError
                    "usage: /effort [none|low|medium|high|xhigh|max]"
            parseReplLine "/bogus"
                `shouldBe` ReplCommandError "unknown command: /bogus (try /help)"

    describe "parseAfkTarget" do
        it "selects local tmux without an argument" do
            parseAfkTarget Nothing `shouldBe` Right AfkLocal

        it "parses an SSH host and remote folder" do
            parseAfkTarget (Just "office-builder:~/haskell-agent")
                `shouldBe` Right
                    (AfkRemote "office-builder" "~/haskell-agent")

        it "rejects incomplete remote targets" do
            parseAfkTarget (Just "office-builder")
                `shouldBe` Left "remote AFK target must be HOST:PATH"

    describe "slashCommands" do
        it "contains every static slash spec, including gated commands" do
            let names = map (.slashName) slashCommands
            names
                `shouldBe`
                    [ "help"
                    , "init"
                    , "review"
                    , "diff"
                    , "fork"
                    , "export"
                    , "history"
                    , "find"
                    , "permissions"
                    , "model"
                    , "theme"
                    , "effort"
                    , "fast"
                    , "plan"
                    , "view-plan"
                    , "queue"
                    , "transcript"
                    , "edit-prompt"
                    , "context"
                    , "btw"
                    , "meta"
                    , "recap"
                    , "retry"
                    , "session"
                    , "session-info"
                    , "desktop"
                    , "afk"
                    , "worktree"
                    , "rename"
                    , "login"
                    , "resume"
                    , "home"
                    , "search"
                    , "compact"
                    , "rewind"
                    , "clear"
                    , "new"
                    , "delete"
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
                    , "changelog"
                    , "agents"
                    , "mcp"
                    , "loop"
                    , "goal"
                    , "workflow"
                    , "deep-research"
                    , "skills"
                    , "shell"
                    , "codemod"
                    , "always-approve"
                    , "update-and-restart"
                    , "quit"
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
            fmap (.slashName) (lookupSlashCommand "/mcps")
                `shouldBe` Just "mcp"
            fmap (.slashName) (lookupSlashCommand "/status")
                `shouldBe` Just "session-info"
            fmap (.slashName) (lookupSlashCommand "/configure")
                `shouldBe` Just "meta"
            fmap (.slashName) (lookupSlashCommand "/exit")
                `shouldBe` Just "quit"
            fmap (.slashName) (lookupSlashCommand "/show-plan")
                `shouldBe` Just "view-plan"
            fmap (.slashName) (lookupSlashCommand "/plan-view")
                `shouldBe` Just "view-plan"
            fmap (.slashName) (lookupSlashCommand "/log")
                `shouldBe` Just "transcript"
            fmap (.slashName) (lookupSlashCommand "/welcome")
                `shouldBe` Just "home"
            fmap (.slashName) (lookupSlashCommand "/changelog")
                `shouldBe` Just "changelog"
            fmap (.slashName) (lookupSlashCommand "/undo")
                `shouldBe` Just "rewind"

        it "completes command names from a leading slash" do
            slashCompletionCandidates "" "/"
                `shouldSatisfy` (\xs ->
                    "/help" `elem` xs
                        && "/model" `elem` xs
                        && "/m" `elem` xs
                        && "/agents" `elem` xs
                        && "/mcp" `elem` xs
                        && "/btw" `elem` xs
                        && "/rewind" `elem` xs
                        && "/undo" `elem` xs)
            slashCompletionCandidates "" "/mo" `shouldBe` ["/model"]
            slashCompletionCandidates "ledom/" "high" `shouldBe` []

        it "completes effort and model arguments" do
            slashCompletionCandidates "troffe/" "h" `shouldBe` ["high"]
            slashCompletionCandidates "troffe/" "m" `shouldBe` ["medium", "max"]
            slashCompletionCandidates "troffe/" "n" `shouldBe` ["none"]
            let grokCatalog = mkSlashCatalog False GrokBuildDialect [] [] []
            slashCompletionCandidatesWithCatalog
                grokCatalog
                "troffe/"
                "m"
                `shouldBe` ["medium"]
            fmap (.slashUsage) (lookupSlashCommandIn grokCatalog "effort")
                `shouldBe`
                    Just "/effort [none|low|medium|high|xhigh]"
            slashCompletionCandidatesWithModels
                ["grok-4.6", "grok-4.5", "grok-4.5-mini", "qwen-local"]
                "m/"
                "grok-4"
                `shouldSatisfy` (\xs ->
                    "grok-4.6" `elem` xs
                        && "grok-4.5" `elem` xs
                        && "grok-4.5-mini" `elem` xs)
            slashCompletionCandidatesWithModels
                ["grok-4.6", "qwen-local"]
                "ledom/"
                "qw"
                `shouldBe` ["qwen-local"]
            slashCompletionCandidates "emaner/" "-"
                `shouldBe` ["--auto"]
            slashCompletionCandidates "krof/" "-"
                `shouldBe` ["--worktree", "--no-worktree"]
            slashCompletionCandidates "llehs/" "b"
                `shouldBe` ["bash", "both"]

        it "does not complete ordinary prompts" do
            slashCompletionCandidates "" "help" `shouldBe` []
            slashCompletionCandidates (reverse "list the ") "files" `shouldBe` []

        it "opens a live menu on slash and fuzzy-filters command names" do
            let displays text cursor =
                    maybe [] (map (.slashSuggestionDisplay) . (.slashMenuSuggestions))
                        (slashMenuFor text cursor)
            displays "/" 1
                `shouldBe`
                    map
                        (("/" <>) . (.slashName))
                        defaultSlashCatalog.slashCatalogCommands
            displays "/mo" 3
                `shouldBe` ["/model", "/codemod", "/permissions"]
            displays "/ra" 3 `shouldSatisfy` ("/reload-auth" `elem`)
            displays "look at /mo" 11 `shouldBe` []

        it "offers argument rows" do
            let menu = slashMenuFor "/effort h" 9
            fmap (.slashMenuReplaceStart) menu `shouldBe` Just 8
            fmap (map (.slashSuggestionDisplay) . (.slashMenuSuggestions)) menu
                `shouldBe` Just ["high", "xhigh"]
            fmap (map (.slashSuggestionDisplay) . (.slashMenuSuggestions))
                (slashMenuForWithModels
                    ["grok-4.6", "qwen-local"]
                    "/model qw"
                    9)
                `shouldBe` Just ["qwen-local"]

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
            listing `shouldSatisfy` ("/mcp" `isInfixOf`)
            listing `shouldSatisfy` ("/usage" `isInfixOf`)
            listing `shouldSatisfy` ("/worktree" `isInfixOf`)
            Text.unpack (formatSlashHelp False (Just "effort"))
                `shouldSatisfy`
                    ("/effort [none|low|medium|high|xhigh|max]" `isInfixOf`)

    describe "capability-gated slash catalog" do
        it "only exposes Fast mode for models advertising priority" do
            let unavailable = mkSlashCatalog False CodexDialect [] [] []
                available = mkSlashCatalog True CodexDialect [] [] []
            lookupSlashCommandIn unavailable "/fast" `shouldBe` Nothing
            lookupSlashCommandIn available "/fast"
                `shouldSatisfy` maybe False (const True)
            parseReplLineWithCatalog unavailable "/fast"
                `shouldBe` ReplCommandError
                    "unknown command: /fast (try /help)"

        let grokCatalog tools =
                mkSlashCatalog False GrokBuildDialect tools [] ["grok-4.6"]
            allCoreTools =
                [ "scheduler_create"
                , "update_goal"
                , "workflow"
                ]
            enabled = grokCatalog allCoreTools

        it "fails closed when the dialect or backing tool is unavailable" do
            parseReplLine "/loop 5m check ci"
                `shouldBe`
                    ReplCommandError "unknown command: /loop (try /help)"
            parseReplLineWithCatalog
                (mkSlashCatalog
                    False CodexDialect allCoreTools [] [])
                "/loop 5m check ci"
                `shouldBe`
                    ReplCommandError "unknown command: /loop (try /help)"
            parseReplLineWithCatalog
                (grokCatalog [])
                "/goal ship it"
                `shouldBe`
                    ReplCommandError "unknown command: /goal (try /help)"
            parseReplLineWithCatalog
                (grokCatalog ["scheduler_create"])
                "/help goal"
                `shouldBe`
                    ReplCommandError "unknown command: goal (try /help)"

        it "uses the same filtered commands for help and both completion paths" do
            let disabled = grokCatalog []
                enabledNames =
                    map (.slashName) enabled.slashCatalogCommands
                disabledHelp =
                    Text.unpack
                        (formatSlashHelpWithCatalog False disabled Nothing)
                enabledHelp =
                    Text.unpack
                        (formatSlashHelpWithCatalog False enabled Nothing)
            disabledHelp `shouldNotSatisfy` ("/loop" `isInfixOf`)
            enabledHelp `shouldSatisfy` ("/loop" `isInfixOf`)
            enabledNames `shouldSatisfy` ("goal" `elem`)
            slashCompletionCandidatesWithCatalog disabled "" "/lo"
                `shouldNotSatisfy` ("/loop" `elem`)
            slashCompletionCandidatesWithCatalog enabled "" "/lo"
                `shouldSatisfy` ("/loop" `elem`)
            fmap
                (map (.slashSuggestionDisplay) . (.slashMenuSuggestions))
                (slashMenuForCatalog disabled "/loo" 4)
                `shouldBe` Nothing
            fmap
                (map (.slashSuggestionDisplay) . (.slashMenuSuggestions))
                (slashMenuForCatalog enabled "/loo" 4)
                `shouldBe` Just ["/loop"]

        it "expands /loop while preserving the submitted slash text" do
            case parseReplLineWithCatalog enabled
                    "  /loop every 5 minutes check ci  " of
                ReplExpandedPrompt original expanded -> do
                    original `shouldBe` "  /loop every 5 minutes check ci  "
                    Text.unpack expanded
                        `shouldSatisfy` ("scheduler_create" `isInfixOf`)
                    Text.unpack expanded
                        `shouldSatisfy` ("fire_immediately" `isInfixOf`)
                    Text.unpack expanded
                        `shouldNotSatisfy` ("scheduler_delete" `isInfixOf`)
                    Text.unpack expanded
                        `shouldSatisfy`
                            ("parent session or user owns cancellation"
                                `isInfixOf`)
                    Text.unpack expanded
                        `shouldSatisfy` ("every 5 minutes check ci" `isInfixOf`)
                other ->
                    expectationFailure
                        ("expected expanded prompt, got " <> show other)
            parseReplLineWithCatalog enabled "/loop"
                `shouldSatisfy` \case
                    ReplCommandError message ->
                        "/loop [interval] <prompt>"
                            `Text.isInfixOf` message
                    _ -> False

        it "parses goal lifecycle and a strict trailing budget" do
            parseReplLineWithCatalog enabled "/goal"
                `shouldBe` ReplGoalStatus
            parseReplLineWithCatalog enabled "/goal status"
                `shouldBe` ReplGoalStatus
            parseReplLineWithCatalog enabled "/goal pause"
                `shouldBe` ReplGoalPause
            parseReplLineWithCatalog enabled "/goal resume"
                `shouldBe` ReplGoalResume
            parseReplLineWithCatalog enabled "/goal clear"
                `shouldBe` ReplGoalClear
            case parseReplLineWithCatalog enabled
                    "/goal ship the widget --budget 1200" of
                ReplGoalSet original objective budget expanded -> do
                    original
                        `shouldBe`
                            "/goal ship the widget --budget 1200"
                    objective `shouldBe` "ship the widget"
                    budget `shouldBe` Just 1200
                    expanded `shouldSatisfy`
                        Text.isInfixOf "advisory scope budget of 1200"
                other ->
                    expectationFailure
                        ("expected budgeted goal, got " <> show other)
            parseReplLineWithCatalog enabled
                "/goal explain --budget nope"
                `shouldBe`
                    ReplCommandError
                        "usage: /goal <objective> [--budget POSITIVE_INTEGER]"

        it "parses workflow launch, management, and deep research" do
            fmap (.slashUsage) (lookupSlashCommandIn enabled "/workflow")
                `shouldBe` Just "/workflow runs | <name> [input]"
            fmap (.slashSummary) (lookupSlashCommandIn enabled "/workflow")
                `shouldBe`
                    Just "Launch a named workflow or list workflow runs"
            slashCompletionCandidatesWithCatalog
                enabled
                "wolfkrow/"
                ""
                `shouldBe` ["runs"]
            parseReplLineWithCatalog enabled "/workflow"
                `shouldBe` ReplWorkflowRuns
            parseReplLineWithCatalog enabled "/workflow runs"
                `shouldBe` ReplWorkflowRuns
            parseReplLineWithCatalog enabled "/workflow pause wf_12"
                `shouldBe` ReplWorkflowManage "pause" (Just "wf_12")
            parseReplLineWithCatalog enabled "/workflow wf_12 stop"
                `shouldBe` ReplWorkflowManage "stop" (Just "wf_12")
            case parseReplLineWithCatalog enabled
                    "/workflow deep-research rust pitfalls" of
                ReplExpandedPrompt original expanded -> do
                    original
                        `shouldBe`
                            "/workflow deep-research rust pitfalls"
                    Text.unpack expanded
                        `shouldSatisfy`
                            ("name: deep-research" `isInfixOf`)
                    Text.unpack expanded
                        `shouldSatisfy`
                            ("args: {\"query\":\"rust pitfalls\"}"
                                `isInfixOf`)
                other ->
                    expectationFailure
                        ("expected workflow expansion, got " <> show other)
            case parseReplLineWithCatalog enabled
                    "/deep-research compare schedulers" of
                ReplExpandedPrompt _ expanded ->
                    expanded
                        `shouldBe`
                            deepResearchInstruction "compare schedulers"
                other ->
                    expectationFailure
                        ("expected deep-research expansion, got " <> show other)
            parseReplLineWithCatalog enabled "/deep-research"
                `shouldBe`
                    ReplCommandError "usage: /deep-research <query>"

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
            slashCompletionCandidatesWithSkills skills "" "/depl"
                `shouldBe` ["/deploy"]
            fmap (map (.slashSuggestionDisplay) . (.slashMenuSuggestions))
                (slashMenuForWithSkills skills "/depl" 5)
                `shouldBe` Just ["/deploy"]
            let help = formatSlashHelpWithSkills False skills (Just "deploy")
            Text.unpack help `shouldSatisfy` ("Deploy the service" `isInfixOf`)
            Text.unpack help `shouldSatisfy` ("skill · repo · agents" `isInfixOf`)

        it "offers Codex-style dollar skill mentions inside prompts" do
            fmap (map (.slashSuggestionDisplay) . (.slashMenuSuggestions))
                (slashMenuForWithSkills skills "please $dep" 11)
                `shouldBe` Just ["$deploy"]
            fmap
                (\menu ->
                    ( menu.slashMenuReplaceStart
                    , menu.slashMenuReplaceEnd
                    , map (.slashSuggestionReplacement)
                        menu.slashMenuSuggestions
                    ))
                (slashMenuForWithSkills skills "please $dep later" 11)
                `shouldBe` Just (7, 11, ["$deploy "])

        it "combines runtime skills and model ids" do
            slashCompletionCandidatesWithSkillsAndModels
                skills
                ["qwen-local"]
                "ledom/"
                "qw"
                `shouldBe` ["qwen-local"]
            fmap (map (.slashSuggestionDisplay) . (.slashMenuSuggestions))
                (slashMenuForWithSkillsAndModels
                    skills
                    ["qwen-local"]
                    "/model qw"
                    9)
                `shouldBe` Just ["qwen-local"]

    describe "setReasoningEffort" do
        it "writes effort onto an empty reasoning config" do
            let updated =
                    setReasoningEffort EffortHigh defaultResponseCreateParams
            currentEffort updated `shouldBe` EffortHigh
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
                            }
                        , ..
                        }
                updated = setReasoningEffort EffortXHigh original
            currentEffort updated `shouldBe` EffortXHigh
            case updated.reasoning of
                Just config -> do
                    config.context `shouldBe` Just "256k"
                    config.generateSummary `shouldBe` Just "auto"
                    config.summary `shouldBe` Just "concise"
                Nothing -> expectationFailure "expected reasoning config"

    describe "currentEffort" do
        it "defaults to low when reasoning is missing" do
            currentEffort defaultResponseCreateParams `shouldBe` EffortLow

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
