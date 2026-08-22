# Third-party notices

## Skylighting core

`agent-tui` uses `skylighting-core` 0.14.7 from:

- Repository: `https://github.com/jgm/skylighting`
- Commit: `e432d65743ecef9475816b2cc074d34833837ced`
- Commit date: 2026-08-04
- Source subdirectory: `skylighting-core`

The Haskell implementation of `skylighting-core` is BSD-3-Clause licensed.
Its upstream license file remains part of the pinned source used by the Nix
build.

## KDE syntax definitions

The files installed under `data/syntax/` are unmodified KDE
KSyntaxHighlighting XML definitions copied from the same pinned Skylighting
checkout's `skylighting-core/xml/` directory.

These data files are not covered by agent-tui's BSD-3-Clause license. They
retain their original, mixed licenses, including MIT, LGPL, GPL, BSD, and
WTFPL declarations. Some older definitions do not declare a license in the
XML file; the manifest records those cases explicitly rather than assigning a
license that upstream did not state.

See `data/syntax/MANIFEST.md` for per-file provenance, inclusion reason, and
the license declaration present in each upstream file. Copyright and license
headers in every XML file have been preserved verbatim.
