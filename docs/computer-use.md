# Local computer use

Interactive terminal sessions expose local desktop capture and
keyboard/pointer control by default for the OpenAI computer-tool integration.
One-shot and non-interactive runs require an explicit `--computer-use`;
`--no-computer-use` disables the capability. Every call still goes through the
computer-use approval policy.

Linux and macOS are supported. This guide describes the Linux backends.

## Security and lifecycle

- Run the CLI as the logged-in desktop user. Do not run it as root or forward a
  desktop session into an unrelated account.
- The complete action batch is validated before any input is injected.
- A successful screenshot establishes the exact display lease for later
  actions. Input is rejected before the first observation, and a changed
  display requires another successful screenshot.
- The selected display and the active, unlocked logind session are checked
  before every action. A changed monitor, resolution, scale, portal stream, or
  session state aborts the batch without retrying it.
- Calls are serialized. A drag or modified input operation releases held
  buttons and modifiers when it fails.
- Portal and D-Bus connections belong to the `agent-cli` process and are closed
  when that process exits.

The model receives logical display coordinates with origin `(0, 0)`. Linux
multi-monitor sessions control one selected monitor; they are not combined
into a virtual desktop.

## X11

An X11 session is selected when `XDG_SESSION_TYPE=x11`, or when `DISPLAY` is
present and no Wayland session is detected. If multiple monitors are active,
the `xrandr` primary monitor is used. A layout with multiple monitors and no
single primary monitor fails closed.

Required programs for a non-Nix installation:

- `xrandr`
- `maim`
- `xdotool`
- a reachable system D-Bus and `systemd-logind`

Set a primary monitor, if needed:

```console
xrandr --output HDMI-1 --primary
```

## Wayland

Wayland is preferred when `XDG_SESSION_TYPE=wayland` or `WAYLAND_DISPLAY` is
set, even if the desktop also exposes an XWayland `DISPLAY`. There is no
automatic fallback from a failed Wayland portal request to X11.

The implementation uses only the standard
`org.freedesktop.portal.ScreenCast` and
`org.freedesktop.portal.RemoteDesktop` interfaces:

1. The first approved computer call opens the desktop's portal chooser.
2. Select exactly one monitor and grant pointer and keyboard access.
3. The selection is reused for the lifetime of that `agent-cli` process.
4. The grant is not persisted as a restore token. A new process prompts again.

The portal stream includes the cursor. A session-owned GStreamer process reads
screenshots from PipeWire and is suspended between explicit capture requests,
so an idle computer-use session does not continuously convert and PNG-compress
frames. Captures are normalized to the portal's logical dimensions before being
returned.

The portal connection is bound conservatively to the process's verified
`systemd-logind` session. The session-bus address is derived from logind's
`User.RuntimePath`; an ambient `DBUS_SESSION_BUS_ADDRESS` is ignored. The
portal's unique bus owner and Unix user are pinned for calls and signals.
Because the user session bus and portal are shared between graphical logins,
computer use fails closed if logind cannot prove that the current session is
the user's primary Wayland display or if the same user has another non-TTY
session.

Required components for a non-Nix installation:

- `xdg-desktop-portal`
- the portal backend for the current desktop, such as
  `xdg-desktop-portal-gnome` or `xdg-desktop-portal-kde`
- PipeWire
- GStreamer 1.x with the base, good, and bad plugin sets
- a reachable system D-Bus and `systemd-logind`

`gst-launch-1.0` and the `pipewiresrc`, `videoconvert`, and `pngenc` elements
must be discoverable. The packaged Nix executable sets the plugin search path
and includes these dependencies only on Linux.

### Wayland capture performance benchmark

The retained benchmark compares the former continuously running capture
pipeline with the request-gated pipeline. Both variants use a 60-fps synthetic
source, the production four-fps `videorate` cap, PNG encoding, and the same
bounded PNG parser as the portal backend. It reports median wall time, parent
and child CPU time, allocation, frame count, and encoded bytes from alternating
paired samples.

Run the optimized benchmark on Linux at representative resolutions:

```console
nix develop -c cabal build --offline agent-cli:bench:portal-capture-bench
bin=$(nix develop -c cabal list-bin agent-cli:bench:portal-capture-bench)
nix develop -c "$bin" 1920 1080 3000 8 5 +RTS -T
nix develop -c "$bin" 3840 2160 3000 8 5 +RTS -T
```

The positional arguments are width, height, idle milliseconds, active capture
count, and sample count. The idle workload demonstrates background processing;
the active workload checks the cost of producing the same requested frames.

## Troubleshooting

- **Session cannot be verified:** ensure the process runs inside the graphical
  login session and can reach `org.freedesktop.login1` on the system bus. The
  process ID is resolved through logind, so launch the CLI from the intended
  graphical session rather than trying to set a session environment variable.
- **Session inactive or locked:** unlock and activate the same graphical
  session. The check intentionally fails closed when logind is unavailable.
- **Portal cannot be associated with the session:** end other graphical or
  unclassified logind sessions for the same user, then launch the CLI again
  from the intended Wayland login. TTY sessions may remain open.
- **Portal request denied or cancelled:** approve the chooser and select one
  monitor. Selecting no monitor or more than one is rejected.
- **Portal capability error:** update the desktop portal/backend. It must
  advertise monitor capture plus keyboard and pointer device support.
- **PipeWire/GStreamer error:** check `gst-inspect-1.0 pipewiresrc` and
  `gst-inspect-1.0 pngenc`.
- **X11 monitor ambiguity:** choose one `xrandr` primary monitor.

## Manual desktop matrix

Use a disposable desktop session and a harmless text editor for input checks.
For each environment:

- [ ] Start `agent-cli --computer-use` as the logged-in desktop user.
- [ ] Approve a screenshot-only call and verify the returned image dimensions.
- [ ] Verify move, click, scroll, Unicode typing, a modified shortcut, and drag.
- [ ] Lock the session and confirm that the next call is rejected.
- [ ] Change resolution or display scale during a batch and confirm it aborts.
- [ ] End the CLI and confirm any portal sharing indicator/grant closes.

### GNOME Wayland

- [ ] The GNOME chooser offers monitors, not windows.
- [ ] Exactly one monitor can be selected.
- [ ] Pointer and keyboard injection work through RemoteDesktop.
- [ ] A second CLI process prompts again.

### KDE Plasma Wayland

- [ ] The KDE chooser offers monitors, not windows.
- [ ] Exactly one monitor can be selected.
- [ ] Pointer and keyboard injection work through RemoteDesktop.
- [ ] A second CLI process prompts again.

### X11 / optional Xvfb smoke

- [ ] A single-monitor Xvfb session is parsed as the selected display.
- [ ] `maim` captures only that monitor.
- [ ] `xdotool` input lands at monitor-local coordinates, including a monitor
  whose root-window origin is negative.

When `Xvfb` and the X11 tools are installed, the repository-level dependency
smoke can be run with:

```console
TMPDIR="$PWD" bash tests/linux-computer-use-x11-smoke.sh
```
