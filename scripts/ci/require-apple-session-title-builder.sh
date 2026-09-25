#!/bin/bash
# The Darwin release builds apple-session-title with host Xcode. That
# derivation and the Xcode wrapper set __noChroot. A strict Nix sandbox
# rejects them before the compiler runs. Relaxed mode builds only those
# derivations on the host.
set -euo pipefail

nix_bin="${1:?nix binary required}"
token="${2:?probe token required}"

if [[ ! "$token" =~ ^[A-Za-z0-9._-]+$ ]]; then
    printf '::error title=Invalid sandbox probe::Probe token contains unsupported characters\n' >&2
    exit 1
fi

xcodebuild="/Applications/Xcode.app/Contents/Developer/usr/bin/xcodebuild"
if [[ ! -x "$xcodebuild" ]]; then
    printf '::error title=Missing Xcode::apple-session-title requires Xcode at /Applications/Xcode.app\n' >&2
    exit 1
fi

case "$(uname -s)-$(uname -m)" in
    Darwin-arm64)
        system="aarch64-darwin"
        ;;
    Darwin-x86_64)
        system="x86_64-darwin"
        ;;
    *)
        printf '::error title=Unsupported builder::apple-session-title is built on Darwin\n' >&2
        exit 1
        ;;
esac

log="$(mktemp)"
trap 'rm -f "$log"' EXIT

# A fresh token keeps a previous realisation from hiding a strict daemon.
expr="derivation {
  name = \"sandbox-relaxed-probe\";
  system = \"${system}\";
  builder = \"/bin/sh\";
  args = [ \"-c\" \"echo ${token} > \$out\" ];
  __noChroot = true;
}"

if ! "$nix_bin" build \
    --option sandbox relaxed \
    --no-link \
    --print-out-paths \
    --expr "$expr" \
    >"$log" 2>&1
then
    cat "$log" >&2
    if grep -q "__noChroot" "$log"; then
        printf '::error title=Nix sandbox is strict::The daemon kept sandbox=true and refused the Xcode wrapper. Set sandbox = relaxed in /etc/nix/nix.conf and restart the Nix daemon. A client can pass --option sandbox relaxed only when it is a trusted user.\n' >&2
    else
        printf '::error title=Relaxed sandbox probe failed::Could not build a __noChroot derivation with sandbox=relaxed.\n' >&2
    fi
    exit 1
fi
