# MCP OAuth authorization

Remote Streamable HTTP MCP servers can require OAuth 2.1 authorization. The
harness implements the client side of the MCP authorization specification
(revision 2026-07-28): protected resource metadata discovery, authorization
server metadata discovery with issuer validation, PKCE, client registration,
scope selection, RFC 8707 resource indicators, and RFC 9207 issuer validation
of the authorization response.

## Logging in

```
agent-cli mcp login https://example.com/mcp
agent-cli mcp login https://example.com/mcp --scope files:write
agent-cli mcp logout https://example.com/mcp
```

In an interactive session, `/mcp` adds an HTTP server and starts the browser
OAuth flow immediately. The local callback server is listening before the
browser opens, so the provider redirect can complete without returning to the
terminal first. `i` still re-authorizes a selected HTTP server.

`--scope` (repeatable) requests additional scopes for step-up authorization;
they are unioned with the scopes already granted for the same issuer. A `403`
with `insufficient_scope` during a session names the missing scope in its
error message.

`mcp login` runs the interactive browser flow:

1. An unauthenticated JSON-RPC request is sent to the MCP endpoint. The
   `WWW-Authenticate: Bearer ...` challenge of the `401` response supplies the
   `resource_metadata` URL and the `scope` the server requires.
2. Protected resource metadata (RFC 9728) is fetched from the challenge URL.
   Without a challenge the well-known URIs are tried in order: the path-aware
   `https://host/.well-known/oauth-protected-resource/<path>` and then the root
   `https://host/.well-known/oauth-protected-resource`. A document whose
   `resource` names a different server is rejected.
3. Authorization server metadata is discovered for the first advertised
   authorization server (or the one used by a previous login). For issuers
   with a path component the candidates are
   `/.well-known/oauth-authorization-server/<path>`,
   `/.well-known/openid-configuration/<path>`, and
   `/<path>/.well-known/openid-configuration`; without a path they are
   `/.well-known/oauth-authorization-server` and
   `/.well-known/openid-configuration`. The document's `issuer` must be
   byte-identical to the issuer used to build the URL.
4. The login refuses to proceed unless `code_challenge_methods_supported`
   contains `S256`.
5. A `client_id` is obtained using the first available mechanism:
   pre-registered credentials from the config, a Client ID Metadata Document
   URL when the authorization server advertises
   `client_id_metadata_document_supported`, a dynamic registration reused
   from the previous login for the same issuer and redirect URI, or a new
   Dynamic Client Registration (RFC 7591) as a native public client. If none
   is available the login fails with instructions to configure
   `oauth.clientId` or `oauth.clientIdMetadataUrl`.
6. Scopes are selected from the challenge `scope`, else the protected
   resource metadata `scopes_supported`, else the configured `oauth.scopes`.
   Previously granted scopes for the same issuer and any additional scopes
   requested for step-up authorization are unioned in. `offline_access` is
   added only when the authorization server advertises it.
7. A loopback callback server is started first, then the browser is opened
   with `code_challenge`, `state`, `scope`, and the canonical server URI as
   `resource`. On callback the `iss` parameter is validated against the
   recorded issuer before anything else is inspected, then `state` is verified
   and the code is exchanged (again with `resource`, and `client_secret` for
   confidential pre-registered clients).

The token record is written atomically with mode `0600` to
`~/.haskell-agent/credentials/mcp/<hex(url)>.json` and is picked up by
configured remote servers automatically. The wire client refreshes expired
tokens with the stored `resource` and `client_secret`.

After authorization is saved, login also registers the endpoint in
`~/.haskell-agent/config.json` if it is missing, enabled by default. The
generated server name is derived from the URL host, with a numeric suffix
when necessary to avoid overwriting another server. Existing server names,
settings, and explicit disabled states are preserved. If the server is
disabled, login prints the command needed to enable it.

Equivalent URL spellings (such as a trailing slash or host capitalization)
reuse the configured server. Its URL is updated to the spelling used for login
so it uses the saved credential. Query parameters remain significant.

Start a new session to connect the
registered server. A failed authorization does not register a server.

## Configuration

Remote servers accept an optional `oauth` object. Every key is optional:

```json
{
  "mcpServers": {
    "remote": {
      "url": "https://example.com/mcp",
      "oauth": {
        "clientId": "pre-registered-client-id",
        "clientSecret": "only-for-confidential-clients",
        "clientIdMetadataUrl": "https://app.example.com/oauth/client-metadata.json",
        "scopes": ["files:read"]
      }
    }
  }
}
```

- `clientId` / `clientSecret`: credentials from a pre-registration with the
  authorization server. They take precedence over every other mechanism. The
  secret is never printed; it is stored in the private token record so token
  refreshes can present it.
- `clientIdMetadataUrl`: an `https` URL with a path that serves a Client ID
  Metadata Document. It is used verbatim as the `client_id` when the
  authorization server supports the mechanism.
- `scopes`: fallback scopes when neither the challenge nor the protected
  resource metadata names any.

## Token record

```json
{
  "client_id": "...",
  "token_endpoint": "https://auth.example.com/token",
  "access_token": "...",
  "refresh_token": "...",
  "expires_at": 1790000000,
  "issuer": "https://auth.example.com",
  "scope": "files:read offline_access",
  "resource": "https://example.com/mcp",
  "client_id_source": "dynamic_registration",
  "client_id_metadata_url": null,
  "client_secret": null,
  "redirect_uri": "http://127.0.0.1:43127/callback"
}
```

The first five keys are the compatibility record used by the wire client;
records written before the remaining keys existed still load. Credentials are
bound to `issuer`: when a later login discovers a different authorization
server the stored client registration and scopes are not reused.

## Library API

`Agent.MCP.OAuth` exposes the pure building blocks so the wire client and
tests share one implementation: `parseWwwAuthenticate` / `challengeScopes`,
`canonicalResourceUri`, `protectedResourceMetadataUrls`,
`authorizationServerMetadataUrls`, `validateIssuer`, `checkPkceSupport`,
`selectClientRegistration`, `selectScopes` / `planScopes`, and
`validateAuthorizationResponseIssuer`, plus the IO discovery and token
endpoint helpers built on them.
