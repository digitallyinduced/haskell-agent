-- | Skill content discovery and integrity checks shared by session hosts.
module Agent.Runtime.Skills.Content
    ( loadMcpSkillsCatalog, resolveSkillContent, verifyMcpSkillContent ) where

import Agent.Json (rawJsonBytes)
import Agent.MCP.Fleet qualified as MCP
import Agent.MCP.Types
    ( McpFleet, McpResourceContent(..), McpSkillEntry(..)
    , McpSkillRegistration(..), McpSkillResource(..), McpSkillResources(..) )
import Agent.Skills
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import System.OsPath (unsafeEncodeUtf)

loadMcpSkillsCatalog :: McpFleet -> IO SkillCatalog
loadMcpSkillsCatalog fleet = do
    registrations <- MCP.mcpFleetSkillRegistrations fleet
    pure $
        SkillCatalog
            (mapMaybe (either (const Nothing) Just . loadRegistration) registrations)
            [ SkillWarning
                (unsafeEncodeUtf
                    (Text.unpack registration.mcpSkillEntry.mcpSkillUri))
                ("MCP skill ignored: " <> err)
            | registration <- registrations
            , Left err <- [loadRegistration registration]
            ]
  where
    loadRegistration registration = do
        value <-
            either
                (Left . Text.pack)
                Right
                (Aeson.eitherDecodeStrict'
                    (rawJsonBytes registration.mcpSkillEntry.mcpSkillFrontmatter))
        loadMcpSkillMetadata
            registration.mcpSkillServer
            registration.mcpSkillEntry.mcpSkillUri
            (entryResourceUris registration.mcpSkillEntry)
            value

-- | Resolve local content immediately or fetch and verify an MCP skill on
-- demand. Remote instructions are never trusted merely because they appeared
-- in @skills/list@.
resolveSkillContent
    :: Maybe McpFleet
    -> Skill
    -> IO (Either Text SkillContent)
resolveSkillContent fleet skill =
    case filesystemSkillContent skill of
        Just content -> pure (Right content)
        Nothing ->
            case (fleet, skill.skillSource) of
                (Nothing, McpSkillSource{}) ->
                    pure (Left "MCP skill server is unavailable")
                (Just activeFleet, McpSkillSource server uri _) ->
                    resolveMcpSkillContent activeFleet server uri skill
                _ -> pure (Left "unsupported skill source")

resolveMcpSkillContent
    :: McpFleet
    -> Text
    -> Text
    -> Skill
    -> IO (Either Text SkillContent)
resolveMcpSkillContent fleet server uri advertised =
    MCP.mcpFleetGetSkill fleet server uri >>= \case
        Left err -> pure (Left err)
        Right entry ->
            MCP.mcpFleetReadResource fleet server uri >>= \case
                Left err -> pure (Left err)
                Right contents ->
                    pure (verifyMcpSkillContent advertised server entry contents)

-- | Validate the complete response chain used to activate an MCP skill.
-- Kept pure so integrity and identity failures can be tested without a live
-- transport.
verifyMcpSkillContent
    :: Skill
    -> Text
    -> McpSkillEntry
    -> [McpResourceContent]
    -> Either Text SkillContent
verifyMcpSkillContent advertised server entry contents = do
    case advertised.skillSource of
        McpSkillSource advertisedServer advertisedUri _
            | advertisedServer /= server ->
                Left "MCP skill server does not match its catalog entry"
            | entry.mcpSkillUri /= advertisedUri ->
                Left "MCP skills/get returned a different skill URI"
            | otherwise -> pure ()
        FilesystemSkillSource{} ->
            Left "expected an MCP skill catalog entry"
    (checkedSkill, manifest) <- validateFetchedEntry advertised server entry
    text <- selectTextResource entry.mcpSkillUri contents
    verifyManifest manifest text
    loadMcpSkillDocument checkedSkill text

validateFetchedEntry
    :: Skill
    -> Text
    -> McpSkillEntry
    -> Either Text (Skill, McpSkillResource)
validateFetchedEntry advertised server entry = do
    resources <- case entry.mcpSkillResources of
        McpSkillResourcesDynamic ->
            Left "MCP skills/get did not provide a resource manifest"
        McpSkillResourcesListed listed -> Right listed
    manifest <-
        case filter
            ((== entry.mcpSkillUri) . (.mcpSkillResourceUri))
            resources of
            [resource] -> Right resource
            [] -> Left "MCP skill manifest does not contain its SKILL.md URI"
            _ -> Left "MCP skill manifest contains duplicate SKILL.md entries"
    value <-
        either
            (Left . Text.pack)
            Right
            (Aeson.eitherDecodeStrict' (rawJsonBytes entry.mcpSkillFrontmatter))
    fetched <-
        loadMcpSkillMetadata
            server
            entry.mcpSkillUri
            (map (.mcpSkillResourceUri) resources)
            value
    let checked =
            advertised
                { skillSource =
                    McpSkillSource
                        server
                        entry.mcpSkillUri
                        (map (.mcpSkillResourceUri) resources)
                }
        sameMetadata =
            fetched { skillSource = advertised.skillSource }
                == advertised
    if sameMetadata
        then Right (checked, manifest)
        else Left "MCP skills/get metadata does not match its catalog entry"

selectTextResource
    :: Text
    -> [McpResourceContent]
    -> Either Text Text
selectTextResource uri contents =
    case [text | content <- contents, content.mcpResourceUri == uri
               , Just text <- [content.mcpResourceText]] of
        [text] -> Right text
        [] -> Left "MCP resources/read returned no textual SKILL.md"
        _ -> Left "MCP resources/read returned duplicate SKILL.md content"

verifyManifest :: McpSkillResource -> Text -> Either Text ()
verifyManifest resource text
    | resource.mcpSkillResourceSize /= BS.length bytes =
        Left "MCP SKILL.md size does not match its manifest"
    | not ("sha256:" `Text.isPrefixOf` digest)
        || Text.length expected /= 64
        || Text.any (not . isHexDigit) expected =
            Left "MCP SKILL.md manifest has an invalid SHA-256 digest"
    | Text.toLower expected /= actual =
        Left "MCP SKILL.md SHA-256 digest does not match its manifest"
    | otherwise = Right ()
  where
    bytes = TextEncoding.encodeUtf8 text
    digest = Text.toLower resource.mcpSkillResourceDigest
    expected = Text.drop 7 digest
    actual = Text.pack (show (hash bytes :: Digest SHA256))
    isHexDigit c =
        ('0' <= c && c <= '9')
            || ('a' <= c && c <= 'f')

entryResourceUris :: McpSkillEntry -> [Text]
entryResourceUris entry =
    case entry.mcpSkillResources of
        McpSkillResourcesDynamic -> []
        McpSkillResourcesListed resources ->
            map (.mcpSkillResourceUri) resources
