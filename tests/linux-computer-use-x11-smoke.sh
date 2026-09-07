#!/usr/bin/env bash
set -euo pipefail

for program in Xvfb xrandr maim xdotool; do
    if ! command -v "$program" >/dev/null 2>&1; then
        echo "skip: $program is unavailable" >&2
        exit 0
    fi
done

: "${TMPDIR:?TMPDIR must name a writable temporary directory}"
display="${AGENT_XVFB_DISPLAY:-:97}"
scratch="$(mktemp -d "$TMPDIR/agent-computer-use-x11.XXXXXX")"
xvfb_pid=

cleanup() {
    if [[ -n "$xvfb_pid" ]]; then
        kill "$xvfb_pid" 2>/dev/null || true
        wait "$xvfb_pid" 2>/dev/null || true
    fi
    rm -r "$scratch"
}
trap cleanup EXIT

Xvfb "$display" -screen 0 800x600x24 -nolisten tcp >"$scratch/xvfb.log" 2>&1 &
xvfb_pid=$!

ready=false
for _ in {1..50}; do
    if DISPLAY="$display" xrandr --listactivemonitors >"$scratch/xrandr.txt" 2>&1
    then
        ready=true
        break
    fi
    sleep 0.1
done

if [[ "$ready" != true ]]; then
    cat "$scratch/xvfb.log" >&2
    exit 1
fi

grep -E '800/[0-9]+x600/[0-9]+\+0\+0' "$scratch/xrandr.txt"
DISPLAY="$display" maim --geometry 800x600+0+0 "$scratch/capture.png"
test -s "$scratch/capture.png"

DISPLAY="$display" xdotool mousemove --sync 123 234
DISPLAY="$display" xdotool getmouselocation --shell >"$scratch/pointer.txt"
grep -Fx "X=123" "$scratch/pointer.txt"
grep -Fx "Y=234" "$scratch/pointer.txt"

echo "Linux X11 computer-use smoke passed"
