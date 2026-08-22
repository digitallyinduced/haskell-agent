# Syntax grammar manifest

All XML files in this directory were copied without modification from:

- Repository: `https://github.com/jgm/skylighting`
- Commit: `e432d65743ecef9475816b2cc074d34833837ced`
- Commit date: 2026-08-04
- Package: `skylighting-core` 0.14.7
- Source directory: `skylighting-core/xml/`

“Direct” means the grammar is part of the initial supported-language set.
Other files are present because a direct or transitive grammar references them
with KDE `IncludeRules`.

The license column reproduces the SPDX identifier or `license` attribute
declared by the upstream XML. “Not declared” means that the upstream file has
no non-empty license declaration; no license conclusion is inferred here.

| File | License declared upstream | Inclusion reason |
| --- | --- | --- |
| `alert.xml` | MIT | Included by `comments.xml`, `doxygenlua.xml`, and `javadoc.xml` |
| `bash.xml` | LGPL | Direct |
| `c.xml` | Not declared | Direct |
| `cmake.xml` | LGPL-2.0-or-later | Included by `markdown.xml` |
| `comments.xml` | MIT | Shared dependency of language grammars |
| `cpp.xml` | LGPL | Direct |
| `cs.xml` | Not declared | Direct |
| `css.xml` | LGPL | Direct |
| `diff.xml` | Not declared | Direct |
| `dockerfile.xml` | MIT | Direct |
| `doxygen.xml` | MIT | Included by C, C#, ISO C++, JavaScript, Markdown, and PHP |
| `doxygenlua.xml` | MIT | Included by `lua.xml` |
| `email.xml` | MIT | Included by `markdown.xml` |
| `gcc.xml` | LGPL | Included by C and ISO C++ |
| `go.xml` | GPLv2+ | Direct |
| `hamlet.xml` | LGPL | Included by Haskell and Markdown |
| `haskell.xml` | LGPL | Direct |
| `html.xml` | LGPL | Direct |
| `isocpp.xml` | LGPL | Included by `cpp.xml` |
| `java.xml` | LGPL | Direct |
| `javadoc.xml` | LGPL | Included by `java.xml` |
| `javascript-react.xml` | MIT | Direct |
| `javascript.xml` | Not declared | Direct |
| `json.xml` | GPL | Direct |
| `kotlin.xml` | LGPLv2+ | Direct |
| `lua.xml` | Not declared | Direct |
| `markdown.xml` | GPL,BSD | Direct |
| `matlab.xml` | Not declared | Included by `markdown.xml` |
| `modelines.xml` | MIT | Included by `comments.xml` |
| `mustache.xml` | MIT | Included by HTML and Markdown |
| `nim.xml` | WTFPL | Included by `markdown.xml` |
| `nix.xml` | MIT | Direct |
| `perl.xml` | LGPLv2 | Included by Markdown and Raku |
| `php.xml` | Not declared | Included by `markdown.xml` |
| `python.xml` | Not declared | Direct |
| `qml.xml` | MIT | Included by `markdown.xml` |
| `r.xml` | GPLv2 | Included by `markdown.xml` |
| `raku.xml` | MIT | Included by `markdown.xml` |
| `rest.xml` | Not declared | Included by Bash, CMake, and Markdown |
| `rhtml.xml` | LGPLv2+ | Included by `markdown.xml` |
| `ruby.xml` | LGPLv2+ | Included by `markdown.xml` |
| `rust.xml` | MIT | Direct |
| `spdx-comments.xml` | MIT | Included by `comments.xml` |
| `sql-mysql.xml` | Not declared | Included by Markdown and PHP |
| `sql.xml` | LGPL | Direct |
| `swift.xml` | MIT | Direct |
| `toml.xml` | LGPLv2+ | Direct |
| `typescript.xml` | MIT | Direct |
| `xml.xml` | LGPL | Direct |
| `yaml.xml` | LGPL | Direct |
| `zig.xml` | MIT | Direct |
| `zsh.xml` | MIT | Included by `markdown.xml` |
