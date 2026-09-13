{ pkgs, bridge }:

pkgs.runCommand "agent-native-cli-entrypoint-check"
  { nativeBuildInputs = [ pkgs.stdenv.cc pkgs.python3 ]; }
  ''
    $CC -Wall -Wextra -Werror \
      -I${bridge}/include \
      ${../../packages/agent-native-bridge/test/cbits/HaskellAgentBridgeCliSmoke.c} \
      -L${bridge}/lib -lhaskell-agent-bridge \
      -Wl,-rpath,${bridge}/lib \
      -o agent-cli
    export HOME="$TMPDIR/home"
    mkdir -p "$HOME"
    python3 - <<'PY'
    import json
    import os
    import pathlib
    import signal
    import subprocess
    import threading

    executable = str(pathlib.Path("agent-cli").resolve())
    environment = {
        "HOME": os.environ["HOME"],
        "TMPDIR": os.environ["TMPDIR"],
        "PATH": os.environ["PATH"],
        "LANG": "en_US.UTF-8",
        "LC_ALL": "en_US.UTF-8",
    }

    def invoke(arguments, status=0, additional_environment=None, input_text=None):
        result = subprocess.run(
            [executable, *arguments],
            env=environment | (additional_environment or {}),
            capture_output=True, text=True, timeout=30, input=input_text,
        )
        assert result.returncode == status, (
            arguments, result.returncode, result.stdout, result.stderr
        )
        return result

    invoke(["--check-entrypoint-arguments"])
    invoke(["--check-runtime-conflict"])
    assert "agent-cli " in invoke(["--version"]).stdout
    assert "Usage:" in invoke(["--help"]).stdout
    assert "Invalid option" in invoke(["--invalid-entrypoint-option"], 1).stderr
    assert "Usage:" in invoke(["+RTS", "-N2", "-RTS", "--help"]).stdout
    assert "Usage:" in invoke(["--help"], additional_environment={"GHCRTS": "-N1"}).stdout
    assert "using -N4" in invoke(["--help", "+RTS", "-s"]).stderr
    assert "using -N2" in invoke(["--help", "+RTS", "-N2", "-s"]).stderr
    assert "not initialized" in invoke(["storage", "status"]).stdout
    assert json.loads(invoke(["mcp", "list", "--json"]).stdout) == []
    assert "invalid transferred session" in invoke(
        ["sessions", "import"], 1, input_text="not valid JSON"
    ).stderr

    # Also exercise interruption while the CLI reads an ordinary stdin pipe.
    child = subprocess.Popen(
        [executable, "sessions", "import"], env=environment,
        stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        text=True,
    )
    try:
        try:
            child.wait(timeout=2)
        except subprocess.TimeoutExpired:
            pass
        else:
            raise AssertionError("Session import did not wait for stdin")
        child.send_signal(signal.SIGINT)
        # Keep stdin open until SIGINT is handled; communicate() would close
        # it immediately and race interruption against the invalid-JSON path.
        child.wait(timeout=10)
        stdout, stderr = child.communicate(timeout=10)
        assert child.returncode == -signal.SIGINT, (
            child.returncode, stdout, stderr
        )
    finally:
        if child.poll() is None:
            child.kill()
        child.communicate()

    # Block a real CLI operation while reading its local configuration. The
    # FIFO writer handshake proves Haskell main has started before SIGINT;
    # merely signalling immediately after Popen could pass without an RTS.
    configuration = pathlib.Path(environment["HOME"]) / ".haskell-agent/config.json"
    configuration.parent.mkdir(exist_ok=True)
    os.mkfifo(configuration)
    reader_ready = threading.Event()
    writer_release = threading.Event()

    def hold_configuration_open():
        descriptor = os.open(configuration, os.O_WRONLY)
        try:
            reader_ready.set()
            writer_release.wait(30)
        finally:
            os.close(descriptor)

    writer = threading.Thread(target=hold_configuration_open, daemon=True)
    writer.start()
    child = subprocess.Popen(
        [executable, "mcp", "list", "--json"], env=environment,
        stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True,
    )
    try:
        assert reader_ready.wait(15), "CLI did not open its configuration FIFO"
        assert child.poll() is None, "CLI did not block on configuration input"
        child.send_signal(signal.SIGINT)
        stdout, stderr = child.communicate(timeout=15)
        assert child.returncode == -signal.SIGINT, (
            child.returncode, stdout, stderr
        )
    finally:
        writer_release.set()
        if child.poll() is None:
            child.kill()
            child.wait()
        writer.join(timeout=1)
        configuration.unlink()
    PY
    touch "$out"
  ''
