module Documentation.Content (pages) where

import Documentation.Types (Page)
import qualified Documentation.Pages.Introduction as Introduction
import qualified Documentation.Pages.Installation as Installation
import qualified Documentation.Pages.Authentication as Authentication
import qualified Documentation.Pages.Terminal as Terminal
import qualified Documentation.Pages.Sessions as Sessions
import qualified Documentation.Pages.Projects as Projects
import qualified Documentation.Pages.ParallelAgents as ParallelAgents
import qualified Documentation.Pages.AgentLifecycle as AgentLifecycle
import qualified Documentation.Pages.Models as Models
import qualified Documentation.Pages.Skills as Skills
import qualified Documentation.Pages.Mcp as Mcp
import qualified Documentation.Pages.Approvals as Approvals
import qualified Documentation.Pages.Commands as Commands
import qualified Documentation.Pages.Configuration as Configuration
import qualified Documentation.Pages.Providers as Providers
import qualified Documentation.Pages.Tools as Tools
import qualified Documentation.Pages.Keybindings as Keybindings
import qualified Documentation.Pages.Troubleshooting as Troubleshooting
import qualified Documentation.Pages.LanguageServers as LanguageServers
import qualified Documentation.Pages.WebAccess as WebAccess
import qualified Documentation.Pages.LearnedSkills as LearnedSkills
import qualified Documentation.Pages.Telegram as Telegram
import qualified Documentation.Pages.Voice as Voice
import qualified Documentation.Pages.Tutorials as Tutorials
import qualified Documentation.Pages.LocalModelTutorial as LocalModelTutorial
import qualified Documentation.Pages.McpTutorial as McpTutorial
import qualified Documentation.Pages.DocumentationSite as DocumentationSite
import qualified Documentation.Pages.Environment as Environment
import qualified Documentation.Pages.PersistedSettings as PersistedSettings
import qualified Documentation.Pages.BrowserControl as BrowserControl
import qualified Documentation.Pages.Deployment as Deployment
import qualified Documentation.Pages.NativeIntegration as NativeIntegration
import qualified Documentation.Pages.RuntimeDaemon as RuntimeDaemon
import qualified Documentation.Pages.ScheduledWork as ScheduledWork
import qualified Documentation.Pages.Server as Server
import qualified Documentation.Pages.StructuredMemory as StructuredMemory
import qualified Documentation.Pages.ToolExecution as ToolExecution

-- This order defines the documentation navigation and previous/next links.
pages :: [Page]
pages =
    [ Introduction.page
    , Installation.page
    , Authentication.page
    , Terminal.page
    , Sessions.page
    , Projects.page
    , ParallelAgents.page
    , AgentLifecycle.page
    , StructuredMemory.page
    , ScheduledWork.page
    , BrowserControl.page
    , Voice.page
    , Telegram.page
    ] <> Tutorials.pages <>
    [ LocalModelTutorial.page
    , McpTutorial.page
    , Models.page
    , Skills.page
    , LearnedSkills.page
    , Mcp.page
    , LanguageServers.page
    , WebAccess.page
    , Approvals.page
    , Commands.page
    , Configuration.page
    , PersistedSettings.page
    , Environment.page
    , Providers.page
    , Tools.page
    , ToolExecution.page
    , Keybindings.page
    , Troubleshooting.page
    , DocumentationSite.page
    , Deployment.page
    , Server.page
    , RuntimeDaemon.page
    , NativeIntegration.page
    ]
