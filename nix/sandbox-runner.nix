{ pkgs, vm }:

let
  virtioSerialDevice =
    if pkgs.stdenv.hostPlatform.isAarch64 then "virtio-serial-device" else "virtio-serial-pci";
in
pkgs.writeShellApplication {
  name = "agent-sandbox-runner";
  runtimeInputs = with pkgs; [
    coreutils
    gawk
    gnugrep
    iproute2
    socat
    util-linux
  ];
  text = ''
    set -eu
    set -o pipefail
    umask 077

    fail() {
      printf '%s\n' "agent-sandbox-runner: $*" >&2
      exit 2
    }

    [ "$#" -ge 1 ] || fail "expected the serve command"
    [ "$1" = "serve" ] || fail "unsupported command"
    shift

    protocol_version=
    tenant_id=
    workspace_root=
    workspace_device=
    workspace_inode=
    state_root=

    while [ "$#" -gt 0 ]; do
      [ "$#" -ge 2 ] || fail "missing option value"
      case "$1" in
        --protocol-version) protocol_version=$2 ;;
        --tenant-id) tenant_id=$2 ;;
        --workspace-root) workspace_root=$2 ;;
        --workspace-device) workspace_device=$2 ;;
        --workspace-inode) workspace_inode=$2 ;;
        --state-root) state_root=$2 ;;
        *) fail "unsupported option: $1" ;;
      esac
      shift 2
    done

    [ "$protocol_version" = "1" ] \
      || fail "unsupported protocol version"
    printf '%s\n' "$tenant_id" \
      | grep -Eq '^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$' \
      || fail "tenant id must be a canonical UUID"
    printf '%s\n' "$workspace_device" | grep -Eq '^[0-9]+$' \
      || fail "workspace device identity is invalid"
    printf '%s\n' "$workspace_inode" | grep -Eq '^[0-9]+$' \
      || fail "workspace inode identity is invalid"
    [ -d "$workspace_root" ] || fail "workspace root is unavailable"
    [ -d "$state_root" ] || fail "state root is unavailable"

    # Open the configured workspace first, then attest the descriptor against
    # the identity captured while loading the tenant registry. A rename or
    # symlink substitution can therefore select only the original directory.
    exec {workspace_fd}<"$workspace_root"
    [ -d "/proc/self/fd/$workspace_fd" ] \
      || fail "workspace root is not a directory"
    [ "$(stat -Lc '%d' "/proc/self/fd/$workspace_fd")" = "$workspace_device" ] \
      || fail "workspace root device changed after registry validation"
    [ "$(stat -Lc '%i' "/proc/self/fd/$workspace_fd")" = "$workspace_inode" ] \
      || fail "workspace root inode changed after registry validation"
    workspace_root="$(realpath -e -- "/proc/self/fd/$workspace_fd")"
    state_root="$(realpath -e -- "$state_root")"
    case "$workspace_root$state_root" in
      *','*|*$'\n'*) fail "mount paths may not contain commas or newlines" ;;
    esac
    case "$state_root/" in
      "$workspace_root/"*) fail "state root may not be inside the workspace" ;;
    esac
    case "$workspace_root/" in
      "$state_root/"*) fail "workspace may not be inside the state root" ;;
    esac

    guest_state="$state_root/guest"
    vm_state="$state_root/vm"
    mkdir -p -- "$guest_state"
    chmod 0700 -- "$guest_state"
    mkdir -p -- "$vm_state"
    chmod 0700 -- "$vm_state"

    # Keep both exported roots pinned by open directory descriptors through
    # QEMU startup.
    exec {guest_state_fd}<"$guest_state"
    [ "$(realpath -e -- "/proc/self/fd/$guest_state_fd")" = "$guest_state" ] \
      || fail "guest state root changed while opening it"

    exec 9>"$vm_state/lock"
    flock -n 9 || fail "a sandbox VM is already active for this tenant"

    snapshot_host_addresses() {
      destination=$1
      ipv4_snapshot="$destination.ipv4"
      ipv6_snapshot="$destination.ipv6"

      if ! ip -o -4 address show \
        | awk '{ sub(/\/.*/, "", $4); print "4 " $4 }' \
        >"$ipv4_snapshot"
      then
        rm -f -- "$ipv4_snapshot" "$ipv6_snapshot"
        return 1
      fi
      if ! ip -o -6 address show \
        | awk '{ sub(/\/.*/, "", $4); print "6 " $4 }' \
        >"$ipv6_snapshot"
      then
        rm -f -- "$ipv4_snapshot" "$ipv6_snapshot"
        return 1
      fi
      if ! sort -u -- "$ipv4_snapshot" "$ipv6_snapshot" >"$destination"; then
        rm -f -- "$ipv4_snapshot" "$ipv6_snapshot"
        return 1
      fi
      rm -f -- "$ipv4_snapshot" "$ipv6_snapshot"
    }

    address_from_event() {
      printf '%s\n' "$1" | awk '
        {
          for (field = 1; field < NF; field++) {
            if ($field == "inet" || $field == "inet6") {
              address = $(field + 1)
              sub(/\/.*/, "", address)
              if (address == "") exit 1
              print ($field == "inet" ? "4 " : "6 ") address
              matches++
            }
          }
        }
        END { if (matches != 1) exit 1 }
      '
    }

    address_monitor_subscribed() {
      monitor_pid=$1
      [ -d "/proc/$monitor_pid/fd" ] || return 1
      for descriptor in "/proc/$monitor_pid/fd/"*; do
        socket_target="$(readlink -- "$descriptor" 2>/dev/null)" || continue
        case "$socket_target" in
          'socket:['*']')
            socket_inode="''${socket_target#socket:[}"
            socket_inode="''${socket_inode%]}"
            ;;
          *) continue ;;
        esac
        socket_groups="$(
          awk -v inode="$socket_inode" -v pid="$monitor_pid" \
            'NR > 1 && $2 == 0 && $3 == pid && $10 == inode { print $4 }' \
            /proc/net/netlink
        )" || return 1
        for group_mask in $socket_groups; do
          case "$group_mask" in
            ""|*[!0-9A-Fa-f]*) continue ;;
          esac
          if (( (0x$group_mask & 0x110) == 0x110 )); then
            return 0
          fi
        done
      done
      return 1
    }

    runtime_dir="$(mktemp -d "$vm_state/runtime.XXXXXXXX")"
    broker_socket="$runtime_dir/broker.sock"
    vm_pid=
    address_monitor_pid=
    address_watch_pid=

    # shellcheck disable=SC2329
    cleanup() {
      status=$?
      trap - EXIT INT TERM
      for watcher_pid in "$address_monitor_pid" "$address_watch_pid"; do
        if [ -n "$watcher_pid" ] && kill -0 "$watcher_pid" 2>/dev/null; then
          kill -TERM "$watcher_pid" 2>/dev/null || true
          wait "$watcher_pid" 2>/dev/null || true
        fi
      done
      if [ -n "$vm_pid" ] && kill -0 "$vm_pid" 2>/dev/null; then
        kill -TERM "$vm_pid" 2>/dev/null || true
        wait "$vm_pid" 2>/dev/null || true
      fi
      find "$runtime_dir" -depth -delete 2>/dev/null || true
      exit "$status"
    }
    trap cleanup EXIT INT TERM

    # Subscribe before taking the firewall snapshot so an address change in
    # the snapshot-to-QEMU window is queued for the watcher below. Pre-open the
    # FIFO only for the writer/reader handoff; neither long-lived process may
    # retain both ends, because monitor exit must appear as EOF to the reader.
    address_events="$runtime_dir/address-events"
    address_watch_ready="$runtime_dir/address-watch-ready"
    address_snapshot="$runtime_dir/host-addresses"
    address_snapshot_ready="$runtime_dir/host-addresses-ready"
    address_current="$runtime_dir/host-addresses-current"
    vm_pid_file="$runtime_dir/vm.pid"
    mkfifo -- "$address_events"
    exec 8<>"$address_events"
    ip -o monitor address >&8 8>&- 2>/dev/null &
    address_monitor_pid=$!
    (
      exec 8>&-
      terminate_monitored_vm() {
        while [ ! -s "$vm_pid_file" ]; do
          sleep 0.01
        done
        read -r monitored_vm_pid <"$vm_pid_file" || exit 0
        kill -TERM "$monitored_vm_pid" 2>/dev/null || true
      }
      : >"$address_watch_ready"
      while IFS= read -r address_event; do
        while [ ! -e "$address_snapshot_ready" ]; do
          sleep 0.01
        done
        # A same-set event is harmless only when it concerns an address
        # already covered by the immutable firewall snapshot. This also
        # catches a fast add/remove pair even if the live set has reverted
        # before the watcher runs.
        case "$address_event" in
          Deleted*)
            terminate_monitored_vm
            exit 0
            ;;
        esac
        if ! event_address=$(address_from_event "$address_event"); then
          terminate_monitored_vm
          exit 0
        fi
        if ! grep -Fxq -- "$event_address" "$address_snapshot"; then
          terminate_monitored_vm
          exit 0
        fi
        if ! snapshot_host_addresses "$address_current"; then
          terminate_monitored_vm
          exit 0
        fi
        if ! cmp -s -- "$address_snapshot" "$address_current"; then
          terminate_monitored_vm
          exit 0
        fi
      done
      # Losing the monitor is also fail-closed: without it the immutable
      # firewall snapshot can no longer be trusted.
      terminate_monitored_vm
    ) <"$address_events" &
    address_watch_pid=$!

    attempts=0
    while [ ! -e "$address_watch_ready" ] \
      || ! address_monitor_subscribed "$address_monitor_pid"
    do
      kill -0 "$address_monitor_pid" 2>/dev/null \
        || fail "host address monitor failed during startup"
      kill -0 "$address_watch_pid" 2>/dev/null \
        || fail "host address watcher failed during startup"
      attempts=$((attempts + 1))
      [ "$attempts" -lt 100 ] \
        || fail "host address monitor did not become ready"
      sleep 0.01
    done
    exec 8>&-

    snapshot_host_addresses "$address_snapshot" \
      || fail "could not snapshot host addresses"
    : >"$address_snapshot_ready"

    kernel_params=("systemd.set_credential=agent.tenant_id:$tenant_id")
    while read -r family address; do
      [ -n "$address" ] || continue
      case "$family" in
        4) kernel_params+=("agent.block_ipv4=$address") ;;
        6) kernel_params+=("agent.block_ipv6=$address") ;;
        *) fail "host address snapshot is invalid" ;;
      esac
    done <"$address_snapshot"

    QEMU_KERNEL_PARAMS="''${kernel_params[*]}" \
    TMPDIR="$runtime_dir" \
    USE_TMPDIR=1 \
      ${vm}/bin/run-agent-tenant-sandbox-vm \
        -display none \
        -serial none \
        -monitor none \
        -no-reboot \
        -sandbox on,obsolete=deny,elevateprivileges=deny,spawn=deny,resourcecontrol=deny \
        -chardev "socket,id=broker,path=$broker_socket,server=on,wait=off" \
        -device ${virtioSerialDevice} \
        -device "virtserialport,chardev=broker,name=agent-broker" \
        -virtfs "local,path=/proc/self/fd/$workspace_fd,security_model=mapped-xattr,mount_tag=workspace,multidevs=remap" \
        -virtfs "local,path=/proc/self/fd/$guest_state_fd,security_model=mapped-xattr,mount_tag=state,multidevs=remap" \
        </dev/null >&2 &
    vm_pid=$!
    printf '%s\n' "$vm_pid" >"$vm_pid_file"

    # A persistent guest must never outlive the host-address snapshot encoded
    # in its immutable firewall set. A real address-set change or monitor
    # failure terminates QEMU; harmless lifetime notifications with an
    # unchanged set leave the VM running.
    attempts=0
    while [ ! -S "$broker_socket" ]; do
      kill -0 "$vm_pid" 2>/dev/null \
        || fail "sandbox VM exited before opening its broker channel"
      attempts=$((attempts + 1))
      [ "$attempts" -lt 600 ] \
        || fail "sandbox VM broker channel did not become ready"
      sleep 0.05
    done

    set +e
    socat STDIO "UNIX-CONNECT:$broker_socket"
    status=$?
    set -e
    exit "$status"
  '';
}
