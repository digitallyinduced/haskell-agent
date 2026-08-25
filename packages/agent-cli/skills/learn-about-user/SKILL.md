---
name: learn-about-user
description: Build or refresh a consent-reviewed technical user profile from a confirmed public GitHub account and save it as durable user-scoped guidance.
when-to-use: Use when the user asks the agent to learn about them, inspect their GitHub work, understand their technical preferences, or refresh their saved user profile.
argument-hint: "[GitHub handle]"
user-invocable: true
---

# Learn about the user

Create or refresh a compact technical profile that helps future agents choose
better defaults. Public GitHub activity is evidence, not permission to make
unrelated personal inferences.

## Safety and consent

- Treat a supplied GitHub handle as a candidate until the user confirms it is
  theirs. If no handle was supplied, `gh api user` may be used to discover the
  currently authenticated account. If that fails, ask for the handle.
- Show the candidate account's public login, display name, bio, and profile URL
  and ask for confirmation before inspecting its repositories.
- Inspect only public GitHub data unless the user separately and explicitly
  asks to include private repositories. Authentication must never be treated as
  permission to inspect private data.
- Never print, request, persist, or infer from access tokens, Git credentials,
  private email addresses, private repository names, or secrets found in files.
  Do not inspect Git remote URLs because they can contain credentials.
- Do not infer sensitive traits, personality, health, politics, religion,
  ethnicity, sexuality, finances, or other non-technical personal attributes.
- Before saving anything, present the proposed profile and let the user edit,
  exclude, or reject it. Persistence requires the normal approval for the
  learned-skill mutation as well.

## Discover and research

Use current-information tools or the GitHub CLI. Do not guess when public data
can be checked.

1. Resolve and confirm exactly one GitHub account.
2. Inspect its public profile, pinned repositories when available, and public
   repositories. Prefer recently active, maintained, original repositories over
   forks and generated mirrors.
3. Select a representative sample rather than exhaustively reading everything.
   Usually five to ten repositories spanning the user's active ecosystems is
   enough. Explain the sample if it could bias the result.
4. Read high-signal public files such as README, language/package manifests,
   lockfiles, formatter and linter configuration, CI workflows, Nix files,
   tests, and contribution documentation. Use a private session temporary
   directory for any shallow clones and remove them when finished.
5. Distinguish:
   - **Verified facts:** directly visible in public sources.
   - **Strong preferences:** repeated choices across several relevant projects.
   - **Tentative signals:** limited or conflicting evidence.

Repository language statistics alone do not prove preference or expertise.
Generated, vendored, tutorial, archived, old, and employer-owned code should be
weighted cautiously. Absence of a tool is not evidence that the user dislikes
it.

Useful GitHub CLI queries include:

```sh
gh api user --jq '{login, name, bio, html_url}'
gh api "users/HANDLE"
gh api --paginate "users/HANDLE/repos?type=owner&sort=updated&per_page=100"
```

Use the public `users/HANDLE/repos` endpoint, not the authenticated
`user/repos` endpoint. A GraphQL query may be used for public pinned
repositories. Ensure its output selects only public repository fields.

## Draft the durable profile

Keep the always-loaded profile concise, normally no more than about 1,500
characters. Include only information that can improve future technical work:

```text
Public technical context:
- Confirmed GitHub account and broad, relevant areas of work.

Strong defaults:
- Repeatedly evidenced language, tooling, architecture, testing, packaging, or
  workflow preferences.

Contextual or tentative signals:
- Useful hypotheses labeled with confidence and the conditions under which they
  apply.

How to apply:
- Use these as defaults only when the request and repository are ambiguous.
- Explicit user instructions and repository-local conventions always win.
- Do not mention or expose the profile unless it is relevant.

Freshness:
- Public sources observed on YYYY-MM-DD; re-check changing facts when needed.
```

Do not turn repository names, contribution counts, employer affiliations, or a
long technology inventory into always-loaded instructions unless they directly
change agent behavior. Prefer statements such as “when starting a Haskell
project, consider Nix flakes because the user repeatedly uses them” over
biographical cataloguing.

Present the draft with a short evidence summary and explicitly ask whether to
save it. Apply requested corrections before persistence.

## Persist or refresh

1. Search learned skills for an existing user technical profile before
   creating anything.
2. Use the stable user-scoped slug `user-technical-profile`.
3. If it exists, read its current revision, compare it with the new evidence,
   show the meaningful changes, and use `skill_update` with that revision.
   Preserve user corrections unless the user explicitly changes them.
4. Otherwise use `skill_create` with:
   - scope: `user`
   - activation: `always`
   - title: `User technical profile`
   - a description and `applies_when` explaining that it supplies technical
     defaults across projects
   - the approved profile as the instructions
5. Evidence should record the confirmation date, confirmed public handle,
   representative public repositories or files sampled, and user corrections.
   Keep evidence factual and omit sensitive or irrelevant data.

Create additional topic-specific `relevant` skills only when the user asks for
that extra detail. The default result is one short always-loaded profile.

Finish by stating what was saved or updated, its scope and activation, and how
the user can refresh or remove it later.
