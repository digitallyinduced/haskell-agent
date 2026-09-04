# Local computer use

`agent-cli --computer-use` opts the current process into local desktop capture
and keyboard/pointer control. It is disabled by default, is exposed only by the
OpenAI computer-tool integration, and every call still goes through the normal
approval policy.

Linux and macOS are supported. This guide describes the Linux backends.

## Security and lifecycle

- Run the CLI as the logged-in desktop user. Do not run it as root or forward a
  desktop session into an unrelated account.
- The complete action batch is validated before any input is injected.
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

The portal stream includes the cursor. Screenshots are read from PipeWire and
normalized to the portal's logical dimensions before being returned.

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

## Troubleshooting

- **Session cannot be verified:** ensure the process runs inside the graphical
  login session and can reach `org.freedesktop.login1` on the system bus.
  `XDG_SESSION_ID` may identify the session; otherwise the process ID is
  resolved through logind.
- **Session inactive or locked:** unlock and activate the same graphical
  session. The check intentionally fails closed when logind is unavailable.
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
