module Agent.ClaudeCode.Internal.Environment
    ( anthropicEnvironmentVariables
    , getSanitizedClaudeEnvironment
    , isClaudeCredentialEnvironmentVariable
    , sanitizedClaudeEnvironment
    , sanitizeClaudeEnvironment
    ) where

import Data.List (isPrefixOf)
import System.Environment (getEnvironment)

-- | Remove variables that can make Claude Code use an API key or a
-- third-party cloud provider instead of the signed-in Claude subscription.
--
-- The allow-list check performed by 'Agent.ClaudeCode.Auth' remains the
-- authoritative guard. Keeping the filtering here shared lets interactive
-- sessions use the same clean environment as the authentication probe.
sanitizeClaudeEnvironment :: [(String, String)] -> [(String, String)]
sanitizeClaudeEnvironment =
    filter (not . isClaudeCredentialEnvironmentVariable . fst)

getSanitizedClaudeEnvironment :: IO [(String, String)]
getSanitizedClaudeEnvironment = sanitizedClaudeEnvironment

-- | The sanitized process environment used by both the auth probe and
-- interactive Claude Code sessions.
sanitizedClaudeEnvironment :: IO [(String, String)]
sanitizedClaudeEnvironment = sanitizeClaudeEnvironment <$> getEnvironment

isClaudeCredentialEnvironmentVariable :: String -> Bool
isClaudeCredentialEnvironmentVariable name =
    "ANTHROPIC_" `isPrefixOf` name
        || name `elem` anthropicEnvironmentVariables

-- | Environment variables that can select or authenticate a billing path
-- other than the signed-in first-party Claude subscription. Ordinary process
-- variables, including cloud credentials that Claude's shell tools may need,
-- are preserved. Removing the Claude provider selectors prevents those
-- credentials from changing the model transport.
--
-- All @ANTHROPIC_*@ variables are filtered by the prefix check above as a
-- forward-compatible precaution; the explicit entries here document the
-- currently relevant provider override variables.
anthropicEnvironmentVariables :: [String]
anthropicEnvironmentVariables =
    [ "ANTHROPIC_API_KEY"
    , "ANTHROPIC_AUTH_TOKEN"
    , "ANTHROPIC_BASE_URL"
    , "ANTHROPIC_BEDROCK_BASE_URL"
    , "ANTHROPIC_VERTEX_BASE_URL"
    , "ANTHROPIC_VERTEX_PROJECT_ID"
    , "ANTHROPIC_FOUNDRY_RESOURCE"
    , "ANTHROPIC_FOUNDRY_API_KEY"
    , "CLAUDE_CODE_USE_BEDROCK"
    , "CLAUDE_CODE_USE_VERTEX"
    , "CLAUDE_CODE_USE_FOUNDRY"
    , "CLAUDE_CODE_SKIP_BEDROCK_AUTH"
    , "CLAUDE_CODE_SKIP_VERTEX_AUTH"
    , "CLAUDE_CODE_API_BASE_URL"
    , "AWS_BEARER_TOKEN_BEDROCK"
    ]
