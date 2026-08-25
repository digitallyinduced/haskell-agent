#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 /path/to/agent-cli" >&2
  exit 2
fi

agent_cli=$1
credential_home=${AGENT_FUNCTIONAL_TEST_CREDENTIAL_HOME:?must point at the CI runner home containing provider credentials}
workspace=$TMPDIR/workspace
test_home=$TMPDIR/home
prompt_file=$TMPDIR/prompt.txt
status_file=$TMPDIR/agent.status
transcript=$TMPDIR/tmux.log
tmux_server="agent-functional-$$"
tmux_session=hello-world

mkdir -p "$workspace" "$test_home"
chmod 700 "$test_home"

copy_file_if_present() {
  local source=$1
  local destination=$2

  if [[ -f "$source" ]]; then
    mkdir -p "$(dirname "$destination")"
    cp "$source" "$destination"
    chmod 600 "$destination"
    return 0
  fi
  return 1
}

provider=${AGENT_FUNCTIONAL_TEST_PROVIDER:-}
model=${AGENT_FUNCTIONAL_TEST_MODEL:-}

case "$provider" in
  "")
    if copy_file_if_present \
      "$credential_home/.codex/auth.json" \
      "$test_home/.codex/auth.json"; then
      provider=openai
      model=${model:-gpt-5.6-luna}
    elif copy_file_if_present \
      "$credential_home/.grok/auth.json" \
      "$test_home/.grok/auth.json"; then
      provider=xai
      model=${model:-grok-4.6}
    elif [[ -d "$credential_home/.haskell-agent/credentials" ]]; then
      mkdir -p "$test_home/.haskell-agent"
      cp -R \
        "$credential_home/.haskell-agent/credentials" \
        "$test_home/.haskell-agent/credentials"
      chmod -R go-rwx "$test_home/.haskell-agent/credentials"
    else
      echo "no supported agent credentials found under $credential_home" >&2
      exit 1
    fi
    ;;
  openai)
    copy_file_if_present \
      "$credential_home/.codex/auth.json" \
      "$test_home/.codex/auth.json" || {
        echo "missing $credential_home/.codex/auth.json" >&2
        exit 1
      }
    model=${model:-gpt-5.6-luna}
    ;;
  xai)
    copy_file_if_present \
      "$credential_home/.grok/auth.json" \
      "$test_home/.grok/auth.json" || {
        echo "missing $credential_home/.grok/auth.json" >&2
        exit 1
      }
    model=${model:-grok-4.6}
    ;;
  *)
    echo "unsupported AGENT_FUNCTIONAL_TEST_PROVIDER: $provider" >&2
    exit 1
    ;;
esac

cat >"$prompt_file" <<'EOF'
Create a file named Main.hs in the current workspace containing a standalone
Haskell program that prints exactly:

Hello, world!

Run the program with runghc and fix any problem you find. Do not only explain
the solution: create and verify the file.
EOF

runner=$TMPDIR/run-agent.sh
cat >"$runner" <<EOF
#!/usr/bin/env bash
set +e
args=(
  --cwd $(printf '%q' "$workspace")
  --prompt-file $(printf '%q' "$prompt_file")
  --yolo
  --minimal
  --motion off
  --no-agents-md
  --no-skills
  --max-turns 12
)
if [[ -n $(printf '%q' "$provider") ]]; then
  args+=(--provider $(printf '%q' "$provider"))
fi
if [[ -n $(printf '%q' "$model") ]]; then
  args+=(--model $(printf '%q' "$model"))
fi
HOME=$(printf '%q' "$test_home") \\
  $(printf '%q' "$agent_cli") "\${args[@]}"
status=\$?
printf '%s\n' "\$status" >$(printf '%q' "$status_file")
tmux -L $(printf '%q' "$tmux_server") wait-for -S agent-finished
sleep 1
exit "\$status"
EOF
chmod +x "$runner"

cleanup() {
  tmux -L "$tmux_server" kill-server 2>/dev/null || true
}
trap cleanup EXIT

tmux -L "$tmux_server" new-session -d -s "$tmux_session" "$runner"
tmux -L "$tmux_server" set-option -t "$tmux_session" remain-on-exit on

if ! timeout 10m tmux -L "$tmux_server" wait-for agent-finished; then
  tmux -L "$tmux_server" capture-pane -p -S - -t "$tmux_session" \
    >"$transcript" 2>/dev/null || true
  cat "$transcript" >&2
  echo "agent-cli functional test timed out" >&2
  exit 1
fi

tmux -L "$tmux_server" capture-pane -p -S - -t "$tmux_session" \
  >"$transcript" 2>/dev/null || true

if [[ ! -s "$status_file" ]]; then
  cat "$transcript" >&2
  echo "agent-cli exited without recording its status" >&2
  exit 1
fi

agent_status=$(<"$status_file")
if [[ "$agent_status" -ne 0 ]]; then
  cat "$transcript" >&2
  echo "agent-cli exited with status $agent_status" >&2
  exit 1
fi

if [[ ! -f "$workspace/Main.hs" ]]; then
  cat "$transcript" >&2
  echo "agent-cli did not create Main.hs" >&2
  exit 1
fi

program_output=$(runghc "$workspace/Main.hs")
if [[ "$program_output" != "Hello, world!" ]]; then
  cat "$workspace/Main.hs" >&2
  printf 'unexpected program output: %q\n' "$program_output" >&2
  exit 1
fi

echo "agent-cli generated and ran a valid Haskell hello-world program"
