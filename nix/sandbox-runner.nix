{ pkgs, rootfs }:

pkgs.writeShellApplication {
  name = "agent-sandbox-runner";
  runtimeInputs = with pkgs; [
    bash
    coreutils
    diffutils
    findutils
    gawk
    gvisor
    gnugrep
    gnused
    iproute2
    jq
    nftables
    slirp4netns
    util-linux
  ];
  text = ''
    set -euo pipefail
    umask 077

    fail() {
      printf '%s\n' "agent-sandbox-runner: $*" >&2
      exit 2
    }

    process_is_running() {
      process_pid=$1
      [ -n "$process_pid" ] || return 1
      kill -0 "$process_pid" 2>/dev/null || return 1
      [ "$(awk '{ print $3 }' "/proc/$process_pid/stat" 2>/dev/null)" != "Z" ]
    }

    wait_for_file() {
      ready_file=$1
      process_pid=$2
      description=$3
      max_attempts=$4
      attempts=0
      while [ ! -s "$ready_file" ]; do
        process_is_running "$process_pid" \
          || fail "$description exited before becoming ready"
        attempts=$((attempts + 1))
        [ "$attempts" -lt "$max_attempts" ] \
          || fail "$description did not become ready"
        sleep 0.01
      done
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

    validate_host_id() {
      kind=$1
      value=$2
      case "$value" in
        ""|0|0[0-9]*|*[!0-9]*) fail "host $kind is invalid" ;;
      esac
      [ "$value" -gt 0 ] && [ "$value" -le 4294967294 ] \
        || fail "host $kind is outside the supported range"
      [ "$value" -ne 65534 ] \
        || fail "host $kind may not use the nobody identity"
    }

    host_uid="$(id -u)"
    host_gid="$(id -g)"
    validate_host_id uid "$host_uid"
    validate_host_id gid "$host_gid"
    [ "$(id -G)" = "$host_gid" ] \
      || fail "runner must not have supplementary groups"
    [ "$(awk '/^NoNewPrivs:/ { print $2 }' /proc/self/status)" = "1" ] \
      || fail "runner must inherit no-new-privileges"
    for capability_field in CapInh CapPrm CapEff CapBnd CapAmb; do
      capability_value="$(
        awk -v name="$capability_field:" \
          '$1 == name { print $2 }' /proc/self/status
      )"
      printf '%s\n' "$capability_value" | grep -Eq '^0+$' \
        || fail "runner capability set $capability_field is not empty"
    done
    stdin_flags="$(awk '$1 == "flags:" { print $2 }' /proc/self/fdinfo/0)"
    printf '%s\n' "$stdin_flags" | grep -Eq '^[0-7]+$' \
      || fail "protocol stdin flags are unavailable"
    [ "$((8#$stdin_flags & 3))" -eq 0 ] \
      || fail "protocol stdin must be read-only"

    # Open the configured workspace before resolving its name. The descriptor
    # remains authoritative if a writable ancestor is renamed or replaced.
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
      *$'\n'*) fail "mount paths may not contain newlines" ;;
    esac
    case "$state_root/" in
      "$workspace_root/"*) fail "state root may not be inside the workspace" ;;
    esac
    case "$workspace_root/" in
      "$state_root/"*) fail "workspace may not be inside the state root" ;;
    esac

    guest_state="$state_root/guest-gvisor-v1-$host_uid-$host_gid"
    control_state="$state_root/gvisor"
    for private_dir in "$guest_state" "$control_state"; do
      [ ! -L "$private_dir" ] \
        || fail "sandbox state directory may not be a symbolic link"
      if [ ! -e "$private_dir" ]; then
        mkdir -- "$private_dir"
      fi
      [ -d "$private_dir" ] \
        || fail "sandbox state path is not a directory"
      chmod 0700 -- "$private_dir"
      [ "$(stat -Lc '%u:%g:%a' "$private_dir")" = \
          "$host_uid:$host_gid:700" ] \
        || fail "sandbox state directory has an unexpected owner or mode"
    done

    exec {guest_state_fd}<"$guest_state"
    [ "$(realpath -e -- "/proc/self/fd/$guest_state_fd")" = "$guest_state" ] \
      || fail "guest state root changed while opening it"
    guest_state_device="$(stat -Lc '%d' "/proc/self/fd/$guest_state_fd")"
    guest_state_inode="$(stat -Lc '%i' "/proc/self/fd/$guest_state_fd")"

    exec 9>"$control_state/lock"
    flock -n 9 || fail "a sandbox is already active for this tenant"

    runtime_dir="$(mktemp -d "$control_state/runtime.XXXXXXXX")"
    case "$runtime_dir" in
      "$control_state"/runtime.*) ;;
      *) fail "runtime directory escaped its trusted parent" ;;
    esac
    chmod 0700 -- "$runtime_dir"
    mkdir -m 0700 \
      "$runtime_dir/bundle" \
      "$runtime_dir/mounts" \
      "$runtime_dir/overlay" \
      "$runtime_dir/runsc"
    sandbox_pid=
    slirp_pid=
    address_monitor_pid=
    address_watch_pid=
    cgroup_leaf=

    cgroup_populated() {
      awk '
        $1 == "populated" {
          print $2
          found = 1
        }
        END {
          if (!found) exit 1
        }
      ' "$1/cgroup.events"
    }

    # shellcheck disable=SC2329
    reap_stopped_children() {
      for child_pid in \
        "$address_monitor_pid" \
        "$address_watch_pid" \
        "$sandbox_pid" \
        "$slirp_pid"
      do
        if [ -n "$child_pid" ] && ! process_is_running "$child_pid"; then
          wait "$child_pid" 2>/dev/null || true
        fi
      done
    }

    # shellcheck disable=SC2329
    sandbox_descendants_stopped() {
      reap_stopped_children
      for child_pid in \
        "$address_monitor_pid" \
        "$address_watch_pid" \
        "$sandbox_pid" \
        "$slirp_pid"
      do
        if process_is_running "$child_pid"; then
          return 1
        fi
      done
      if [ -n "$cgroup_leaf" ]; then
        [ -d "$cgroup_leaf" ] || return 1
        [ "$(cgroup_populated "$cgroup_leaf" 2>/dev/null)" = "0" ] \
          || return 1
      fi
      return 0
    }

    # shellcheck disable=SC2329
    signal_known_children() {
      signal_name=$1
      for child_pid in \
        "$address_monitor_pid" \
        "$address_watch_pid" \
        "$sandbox_pid" \
        "$slirp_pid"
      do
        if process_is_running "$child_pid"; then
          kill "-$signal_name" "$child_pid" 2>/dev/null || true
        fi
      done
    }

    # shellcheck disable=SC2329
    wait_for_sandbox_descendants() {
      attempts=0
      while ! sandbox_descendants_stopped; do
        attempts=$((attempts + 1))
        [ "$attempts" -lt 200 ] || return 1
        sleep 0.01
      done
    }

    # shellcheck disable=SC2329
    fail_closed_hold() {
      printf '%s\n' \
        "agent-sandbox-runner: cleanup did not quiesce; holding tenant lock for supervisor kill" >&2
      trap : HUP INT TERM
      while :; do
        kill -STOP "$BASHPID"
      done
    }

    # shellcheck disable=SC2329
    cleanup() {
      status=$?
      cleanup_failed=0
      trap - EXIT HUP INT TERM
      : >"$runtime_dir/cleanup-requested" 2>/dev/null || true

      signal_known_children TERM
      if ! wait_for_sandbox_descendants; then
        if [ -n "$cgroup_leaf" ] && [ -d "$cgroup_leaf" ]; then
          printf '1\n' >"$cgroup_leaf/cgroup.kill" 2>/dev/null \
            || cleanup_failed=1
        fi
        signal_known_children KILL
        if ! wait_for_sandbox_descendants; then
          fail_closed_hold
        fi
      fi
      reap_stopped_children

      if [ -n "$cgroup_leaf" ] && [ -d "$cgroup_leaf" ]; then
        if [ "$(cgroup_populated "$cgroup_leaf" 2>/dev/null)" = "0" ]
        then
          if ! rmdir -- "$cgroup_leaf" 2>/dev/null; then
            cleanup_failed=1
          fi
        else
          fail_closed_hold
        fi
      fi

      case "$runtime_dir" in
        "$control_state"/runtime.*)
          find "$runtime_dir" -depth -delete 2>/dev/null || cleanup_failed=1
          ;;
        *) cleanup_failed=1 ;;
      esac

      if [ "$cleanup_failed" -ne 0 ]; then
        printf '%s\n' \
          "agent-sandbox-runner: sandbox cleanup was incomplete" >&2
        [ "$status" -ne 0 ] || status=2
      fi
      exit "$status"
    }
    trap cleanup EXIT HUP INT TERM

    read_process_cgroup() {
      target_pid=$1
      [ "$(wc -l <"/proc/$target_pid/cgroup")" -eq 1 ] || return 1
      IFS=: read -r hierarchy controllers path <"/proc/$target_pid/cgroup"
      [ "$hierarchy" = "0" ] || return 1
      [ -z "$controllers" ] || return 1
      printf '%s\n' "$path"
    }

    verify_empty_procs() {
      if IFS= read -r _ <"$1/cgroup.procs"; then
        return 1
      fi
    }

    enable_sandbox_controllers() {
      cgroup_dir=$1
      for controller in cpu memory pids; do
        grep -qw -- "$controller" "$cgroup_dir/cgroup.controllers" \
          || fail "required $controller cgroup controller is unavailable"
      done
      printf '+cpu +memory +pids\n' >"$cgroup_dir/cgroup.subtree_control"
      actual_controllers="$(
        tr ' ' '\n' <"$cgroup_dir/cgroup.subtree_control" \
          | sed '/^$/d' \
          | sort \
          | tr '\n' ' ' \
          | sed 's/ $//'
      )"
      [ "$actual_controllers" = "cpu memory pids" ] \
        || fail "unexpected delegated cgroup controller set"
    }

    write_cgroup_limit() {
      limit_file=$1
      limit_value=$2
      printf '%s\n' "$limit_value" >"$cgroup_leaf/$limit_file"
      [ "$(cat "$cgroup_leaf/$limit_file")" = "$limit_value" ] \
        || fail "cgroup limit $limit_file did not read back exactly"
    }

    current_cgroup="$(read_process_cgroup "$$")" \
      || fail "runner is not in a unified cgroup"
    case "$current_cgroup" in
      /*.service/supervisor) ;;
      *) fail "runner is not in a delegated systemd supervisor subgroup" ;;
    esac
    unit_cgroup="''${current_cgroup%/supervisor}"
    unit_name="''${unit_cgroup##*/}"
    case "$unit_name" in
      *.service) ;;
      *) fail "delegation parent is not a systemd service" ;;
    esac
    unit_dir="/sys/fs/cgroup$unit_cgroup"
    supervisor_dir="$unit_dir/supervisor"
    [ "$(realpath -e -- "$unit_dir")" = "$unit_dir" ] \
      || fail "service cgroup path is not canonical"
    [ "$(findmnt -n -o FSTYPE -T "$unit_dir")" = "cgroup2" ] \
      || fail "service delegation is not on cgroup v2"
    [ "$(cat "$unit_dir/cgroup.type")" = "domain" ] \
      || fail "service delegation is not a domain cgroup"
    verify_empty_procs "$unit_dir" \
      || fail "service cgroup contains processes outside the supervisor subgroup"
    grep -Fxq -- "$$" "$supervisor_dir/cgroup.procs" \
      || fail "runner is not owned by the supervisor subgroup"

    enable_sandbox_controllers "$unit_dir"
    sandboxes_dir="$unit_dir/sandboxes"
    if [ ! -e "$sandboxes_dir" ] \
      && ! mkdir -- "$sandboxes_dir" 2>/dev/null \
      && [ ! -d "$sandboxes_dir" ]
    then
      fail "could not create the sandbox cgroup parent"
    fi
    [ -d "$sandboxes_dir" ] \
      || fail "sandbox cgroup parent is not a directory"
    [ "$(realpath -e -- "$sandboxes_dir")" = "$sandboxes_dir" ] \
      || fail "sandbox cgroup parent path is not canonical"
    [ "$(stat -Lc '%u:%g' "$sandboxes_dir")" = "$host_uid:$host_gid" ] \
      || fail "sandbox cgroup parent has an unexpected owner"
    verify_empty_procs "$sandboxes_dir" \
      || fail "sandbox cgroup parent directly contains processes"
    enable_sandbox_controllers "$sandboxes_dir"

    candidate_cgroup_leaf="$sandboxes_dir/tenant-$tenant_id"
    if [ -e "$candidate_cgroup_leaf" ]; then
      [ -d "$candidate_cgroup_leaf" ] \
        || fail "tenant sandbox cgroup leaf is not a directory"
      [ "$(realpath -e -- "$candidate_cgroup_leaf")" = \
          "$candidate_cgroup_leaf" ] \
        || fail "tenant sandbox cgroup leaf path is not canonical"
      [ "$(stat -Lc '%u:%g' "$candidate_cgroup_leaf")" = \
          "$host_uid:$host_gid" ] \
        || fail "tenant sandbox cgroup leaf has an unexpected owner"
      cgroup_leaf="$candidate_cgroup_leaf"
      [ "$(cgroup_populated "$cgroup_leaf")" = "0" ] \
        || fail "a previous tenant sandbox cgroup is still populated"
      rmdir -- "$cgroup_leaf" \
        || fail "could not remove an empty tenant sandbox cgroup"
      cgroup_leaf=
    fi
    mkdir -- "$candidate_cgroup_leaf" \
      || fail "could not create the tenant sandbox cgroup"
    cgroup_leaf="$candidate_cgroup_leaf"
    [ "$(dirname -- "$(realpath -e -- "$cgroup_leaf")")" = \
        "$sandboxes_dir" ] \
      || fail "sandbox cgroup leaf escaped its parent"
    [ "$(cat "$cgroup_leaf/cgroup.type")" = "domain" ] \
      || fail "sandbox cgroup leaf is not a domain cgroup"
    [ "$(stat -Lc '%u:%g' "$cgroup_leaf")" = "$host_uid:$host_gid" ] \
      || fail "sandbox cgroup leaf has an unexpected owner"
    [ "$(cgroup_populated "$cgroup_leaf")" = "0" ] \
      || fail "new sandbox cgroup leaf is already populated"
    write_cgroup_limit cpu.max "200000 100000"
    write_cgroup_limit memory.high "1879048192"
    write_cgroup_limit memory.max "2147483648"
    write_cgroup_limit memory.swap.max "0"
    write_cgroup_limit memory.oom.group "1"
    write_cgroup_limit pids.max "512"
    write_cgroup_limit cgroup.max.depth "0"
    write_cgroup_limit cgroup.max.descendants "0"
    leaf_cgroup_path="''${cgroup_leaf#/sys/fs/cgroup}"

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

    address_events="$runtime_dir/address-events"
    address_watch_ready="$runtime_dir/address-watch-ready"
    address_snapshot="$runtime_dir/host-addresses"
    address_snapshot_ready="$runtime_dir/host-addresses-ready"
    address_current="$runtime_dir/host-addresses-current"
    address_monitor_gate="$runtime_dir/address-monitor-gate"
    address_watch_gate="$runtime_dir/address-watch-gate"
    helper_failure="$runtime_dir/helper-failure"
    mkfifo -- \
      "$address_events" \
      "$address_monitor_gate" \
      "$address_watch_gate"
    exec 8<>"$address_events"
    (
      exec {workspace_fd}<&-
      exec {guest_state_fd}<&-
      exec 9>&-
      exec </dev/null
      trap '
        if [ ! -e "$runtime_dir/cleanup-requested" ]; then
          printf "%s\n" "host address monitor exited" >"$helper_failure"
          printf "1\n" >"$cgroup_leaf/cgroup.kill" 2>/dev/null || true
        fi
      ' EXIT
      IFS= read -r gate_value <"$address_monitor_gate"
      [ "$gate_value" = "go" ] || exit 2
      exec ip -o monitor address
    ) >&8 8>&- 2>/dev/null &
    address_monitor_pid=$!
    printf '%s\n' "$address_monitor_pid" >"$cgroup_leaf/cgroup.procs"
    [ "$(read_process_cgroup "$address_monitor_pid")" = "$leaf_cgroup_path" ] \
      || fail "host address monitor did not enter the sandbox resource cgroup"
    printf 'go\n' >"$address_monitor_gate"
    (
      exec {workspace_fd}<&-
      exec {guest_state_fd}<&-
      exec 9>&-
      exec 8>&-
      exec >/dev/null
      terminate_monitored_sandbox() {
        failure_reason=$1
        if [ ! -e "$runtime_dir/cleanup-requested" ]; then
          printf '%s\n' "$failure_reason" >"$helper_failure"
          printf '1\n' >"$cgroup_leaf/cgroup.kill" 2>/dev/null || true
        fi
      }
      trap 'terminate_monitored_sandbox "host address watcher exited"' EXIT
      IFS= read -r gate_value <"$address_watch_gate"
      [ "$gate_value" = "go" ] || exit 2
      : >"$address_watch_ready"
      while IFS= read -r address_event; do
        while [ ! -e "$address_snapshot_ready" ]; do
          sleep 0.01
        done
        case "$address_event" in
          Deleted*)
            terminate_monitored_sandbox "host address set changed"
            exit 0
            ;;
        esac
        if ! event_address="$(address_from_event "$address_event")"; then
          terminate_monitored_sandbox "host address event was invalid"
          exit 0
        fi
        if ! grep -Fxq -- "$event_address" "$address_snapshot"; then
          terminate_monitored_sandbox "host address set changed"
          exit 0
        fi
        if ! snapshot_host_addresses "$address_current"; then
          terminate_monitored_sandbox "host address rescan failed"
          exit 0
        fi
        if ! cmp -s -- "$address_snapshot" "$address_current"; then
          terminate_monitored_sandbox "host address set changed"
          exit 0
        fi
      done
      terminate_monitored_sandbox "host address event stream closed"
    ) <"$address_events" &
    address_watch_pid=$!
    printf '%s\n' "$address_watch_pid" >"$cgroup_leaf/cgroup.procs"
    [ "$(read_process_cgroup "$address_watch_pid")" = "$leaf_cgroup_path" ] \
      || fail "host address watcher did not enter the sandbox resource cgroup"
    printf 'go\n' >"$address_watch_gate"

    attempts=0
    while [ ! -e "$address_watch_ready" ] \
      || ! address_monitor_subscribed "$address_monitor_pid"
    do
      process_is_running "$address_monitor_pid" \
        || fail "host address monitor failed during startup"
      process_is_running "$address_watch_pid" \
        || fail "host address watcher failed during startup"
      attempts=$((attempts + 1))
      [ "$attempts" -lt 100 ] \
        || fail "host address monitor did not become ready"
      sleep 0.01
    done
    exec 8>&-

    snapshot_host_addresses "$address_snapshot" \
      || fail "could not snapshot host addresses"
    [ -s "$address_snapshot" ] \
      || fail "host address snapshot is empty"
    : >"$address_snapshot_ready"

    nft_rules="$runtime_dir/network.nft"
    cat >"$nft_rules" <<'EOF'
    flush ruleset
    table inet agent_sandbox {
      chain input {
        type filter hook input priority 0; policy drop;
        iifname "lo" accept
        ct state established,related accept
      }
      chain forward {
        type filter hook forward priority 0; policy drop;
      }
      chain output {
        type filter hook output priority 0; policy drop;
        oifname "lo" accept
        ip daddr 10.0.2.3 udp dport 53 accept
        ip daddr 10.0.2.3 tcp dport 53 accept
    EOF
    while read -r address_family host_address; do
      [ -n "$host_address" ] || continue
      case "$address_family" in
        4)
          printf '        ip daddr %s drop\n' \
            "$host_address" >>"$nft_rules"
          ;;
        6)
          printf '        ip6 daddr %s drop\n' \
            "$host_address" >>"$nft_rules"
          ;;
        *) fail "host address snapshot is invalid" ;;
      esac
    done <"$address_snapshot"
    cat >>"$nft_rules" <<'EOF'
        ip daddr { 0.0.0.0/8, 10.0.0.0/8, 100.64.0.0/10, 127.0.0.0/8, 169.254.0.0/16, 172.16.0.0/12, 192.0.0.0/24, 192.0.2.0/24, 192.88.99.0/24, 192.168.0.0/16, 198.18.0.0/15, 198.51.100.0/24, 203.0.113.0/24, 224.0.0.0/4, 240.0.0.0/4 } drop
        meta nfproto ipv6 drop
        meta nfproto ipv4 accept
      }
    }
    EOF
    chmod 0400 -- "$nft_rules"

    cgroup_gate="$runtime_dir/cgroup-gate"
    network_gate="$runtime_dir/network-gate"
    namespace_ready="$runtime_dir/namespace-ready"
    slirp_gate="$runtime_dir/slirp-gate"
    slirp_ready="$runtime_dir/slirp-ready"
    mkfifo -- "$cgroup_gate" "$network_gate" "$slirp_gate"

    exec 3<&0
    exec 4>&1
    (
      IFS= read -r gate_value <"$cgroup_gate"
      [ "$gate_value" = "go" ] || exit 2
      exec 3<&-
      exec 4>&-
      exec 9>&-
      # The script is intentionally single-quoted so the namespace child,
      # rather than the host runner, expands its positional parameters.
      # shellcheck disable=SC2016
      exec setpriv --no-new-privs \
        unshare --user --map-root-user --mount --net -- \
        bash -ceu '
          runtime_dir=$1
          workspace_fd=$2
          guest_state_fd=$3
          workspace_device=$4
          workspace_inode=$5
          guest_state_device=$6
          guest_state_inode=$7
          tenant_id=$8
          host_uid=$9
          shift 9
          host_gid=$1
          address_snapshot=$2
          nft_rules=$3
          network_gate=$4
          namespace_ready=$5
          sandbox_rootfs=$6

          [ "$(awk "NR == 1 { print \$1 \":\" \$2 \":\" \$3 }" /proc/self/uid_map)" = \
              "0:$host_uid:1" ]
          [ "$(awk "NR == 1 { print \$1 \":\" \$2 \":\" \$3 }" /proc/self/gid_map)" = \
              "0:$host_gid:1" ]
          [ "$(awk "/^NoNewPrivs:/ { print \$2 }" /proc/self/status)" = "1" ]

          mount --make-rprivate /
          workspace_stage="$runtime_dir/mounts/workspace"
          state_stage="$runtime_dir/mounts/state"
          mkdir -m 0700 -- "$workspace_stage" "$state_stage"
          mount --bind "/proc/self/fd/$workspace_fd" "$workspace_stage"
          mount -o remount,bind,rw,nosuid,nodev "$workspace_stage"
          mount --bind "/proc/self/fd/$guest_state_fd" "$state_stage"
          mount -o remount,bind,rw,nosuid,nodev "$state_stage"
          [ "$(stat -Lc "%d:%i" "$workspace_stage")" = \
              "$workspace_device:$workspace_inode" ]
          [ "$(stat -Lc "%d:%i" "$state_stage")" = \
              "$guest_state_device:$guest_state_inode" ]
          exec {workspace_fd}<&-
          exec {guest_state_fd}<&-

          namespace_tmp="$runtime_dir/namespace-ready.tmp"
          {
            printf "%s\n" "$$"
            readlink /proc/self/ns/user
            readlink /proc/self/ns/net
            readlink /proc/self/ns/mnt
          } >"$namespace_tmp"
          mv -- "$namespace_tmp" "$namespace_ready"

          IFS= read -r gate_value <"$network_gate"
          [ "$gate_value" = "go" ]
          ip link show tap0 >/dev/null
          ip -4 route show default | grep -Eq " dev tap0( |$)"
          nft -f "$nft_rules"
          nft list table inet agent_sandbox >/dev/null

          worker_launch="
            set -eux
            bootstrap_failure() {
              rc=\$?
              set +e
              if [ \"\$rc\" -ne 0 ]; then
                tail -c 16384 /run/agent-bootstrap-trace \
                  | tail -n 80 \
                  | sed \"s/^/agent-sandbox-runner: bootstrap: /\" >&8
              fi
            }
            trap bootstrap_failure EXIT
            setpriv_dump=\$(/bin/setpriv --dump)
            printf \"%s\\n\" \"\$setpriv_dump\" \
              | grep -Fx \"no_new_privs: 1\" >/dev/null
            printf \"%s\\n\" \"\$setpriv_dump\" \
              | grep -Fx \"Inheritable capabilities: [none]\" >/dev/null
            # gVisor/arm64 reports ambient capabilities as unsupported. That
            # means the ambient set is unavailable rather than non-empty.
            printf \"%s\\n\" \"\$setpriv_dump\" \
              | grep -Ex \"Ambient capabilities: \\[(none|unsupported)\\]\" >/dev/null
            ambient_capabilities=\$(awk \"/^CapAmb:/ { print \\\$2 }\" /proc/self/status)
            if [ -n \"\$ambient_capabilities\" ]; then
              printf \"%s\\n\" \"\$ambient_capabilities\" | grep -Eq \"^0+\$\"
            else
              printf \"%s\\n\" \"\$setpriv_dump\" \
                | grep -Fx \"Ambient capabilities: [unsupported]\" >/dev/null
            fi
            for field in CapInh CapPrm CapEff CapBnd; do
              test \"\$(awk -v name=\"\$field:\" \"\\\$1 == name { print \\\$2 }\" /proc/self/status)\" = 0000000000000000
            done
            for state_dir in /state/home /state/home/.cache /state/tmp; do
              test ! -L \"\$state_dir\"
              if [ ! -e \"\$state_dir\" ]; then
                mkdir -- \"\$state_dir\"
              fi
              test -d \"\$state_dir\"
              case \"\$(realpath -e -- \"\$state_dir\")/\" in
                /state/*/) ;;
                *) exit 2 ;;
              esac
              chmod 0700 -- \"\$state_dir\"
              test \"\$(stat -Lc \"%u:%g:%a\" \"\$state_dir\")\" = 0:0:700
            done
            test -x /bin/agent-sandbox-worker
            set +x
            unset BASH_XTRACEFD
            exec 2>&8
            rm -f -- /run/agent-bootstrap-trace
            trap - EXIT
            exec 7>&-
            exec 8>&-
            exec /bin/agent-sandbox-worker \
              --protocol-version 1 \
              --tenant-id \"\$1\" \
              --workspace /workspace \
              --state /state
          "

          guest_launch="
            umask 077
            exec 7>/run/agent-bootstrap-trace
            exec 8>&2
            exec 2>&7
            bootstrap_failure() {
              rc=\$?
              set +e
              if [ \"\$rc\" -ne 0 ]; then
                tail -c 16384 /run/agent-bootstrap-trace \
                  | tail -n 80 \
                  | sed \"s/^/agent-sandbox-runner: bootstrap: /\" >&8
              fi
            }
            trap bootstrap_failure EXIT
            export BASH_XTRACEFD=7
            set -eux
            test -r /proc/gvisor/kernel_is_gvisor
            test \"\$(id -u):\$(id -g)\" = 0:0
            setpriv_dump=\$(/bin/setpriv --dump)
            printf \"%s\\n\" \"\$setpriv_dump\" >&7
            printf \"%s\\n\" \"\$setpriv_dump\" \
              | grep -Fx \"no_new_privs: 1\" >/dev/null
            printf \"%s\\n\" \"\$setpriv_dump\" \
              | grep -Fx \"Inheritable capabilities: [none]\" >/dev/null
            printf \"%s\\n\" \"\$setpriv_dump\" \
              | grep -Ex \"Ambient capabilities: \\[(none|unsupported)\\]\" >/dev/null
            test \"\$(awk \"/^CapInh:/ { print \\\$2 }\" /proc/self/status)\" = 0000000000000000
            ambient_capabilities=\$(awk \"/^CapAmb:/ { print \\\$2 }\" /proc/self/status)
            if [ -n \"\$ambient_capabilities\" ]; then
              printf \"%s\\n\" \"\$ambient_capabilities\" | grep -Eq \"^0+\$\"
            else
              printf \"%s\\n\" \"\$setpriv_dump\" \
                | grep -Fx \"Ambient capabilities: [unsupported]\" >/dev/null
            fi
            for field in CapPrm CapEff CapBnd; do
              test \"\$(awk -v name=\"\$field:\" \"\\\$1 == name { print \\\$2 }\" /proc/self/status)\" = 0000000000000108
            done
            test -s /nix-state-seed/db/db.sqlite
            install -d -m 0755 \
              /nix/var/log/nix/drvs \
              /nix/var/nix
            cp -a --no-preserve=ownership /nix-state-seed/. /nix/var/nix/
            chmod -R u+w /nix/var/log
            chmod a+w /nix /nix/store
            chmod -R u+w /nix/var/nix
            # Current Nix requires these shared parents to be exactly 0755.
            # Set that mode before dropping CAP_FOWNER so LocalStore can skip
            # an otherwise forbidden no-op chmod in the zero-cap worker.
            chmod 0755 \
              /nix/var/nix/gcroots/per-user \
              /nix/var/nix/profiles/per-user
            test -w /nix/store
            test -w /nix/var/nix/db/db.sqlite
            test ! -S /nix/var/nix/daemon-socket/socket
            # gVisor/arm64 does not implement securebits or ambient-capability
            # mutation. Drop the bounding and inheritable sets directly; the
            # second shell above attests every capability set as empty before
            # it can execute the worker.
            exec /bin/setpriv \
              --bounding-set=-all \
              --inh-caps=-all \
              --no-new-privs \
              /bin/bash -ceux \"\$2\" agent-sandbox-worker \"\$1\"
          "

          jq -n \
            --arg root "$sandbox_rootfs" \
            --arg workspace "$workspace_stage" \
            --arg state "$state_stage" \
            --arg tenant "$tenant_id" \
            --arg launch "$guest_launch" \
            --arg worker "$worker_launch" \
            "{
              ociVersion: \"1.0.0\",
              process: {
                terminal: false,
                user: { uid: 0, gid: 0, additionalGids: [] },
                args: [\"/bin/bash\", \"-ceu\", \$launch, \"agent-sandbox-bootstrap\", \$tenant, \$worker],
                env: [
                  \"HOME=/state/home\",
                  \"USER=root\",
                  \"LOGNAME=root\",
                  \"TMPDIR=/state/tmp\",
                  \"XDG_CACHE_HOME=/state/home/.cache\",
                  \"PATH=/bin:/sbin:/usr/bin\",
                  \"SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt\",
                  \"NIX_REMOTE=local\",
                  \"GIT_PAGER=cat\",
                  \"LANG=C.UTF-8\"
                ],
                cwd: \"/workspace\",
                noNewPrivileges: true,
                capabilities: {
                  bounding: [\"CAP_FOWNER\", \"CAP_SETPCAP\"],
                  effective: [\"CAP_FOWNER\", \"CAP_SETPCAP\"],
                  inheritable: [],
                  permitted: [\"CAP_FOWNER\", \"CAP_SETPCAP\"],
                  ambient: []
                },
                rlimits: [
                  { type: \"RLIMIT_CORE\", hard: 0, soft: 0 },
                  { type: \"RLIMIT_NOFILE\", hard: 8192, soft: 8192 },
                  { type: \"RLIMIT_NPROC\", hard: 512, soft: 512 }
                ]
              },
              root: { path: \$root, readonly: false },
              hostname: \"agent-sandbox\",
              mounts: [
                {
                  destination: \"/proc\",
                  type: \"proc\",
                  source: \"proc\",
                  options: [\"nosuid\", \"noexec\", \"nodev\"]
                },
                {
                  destination: \"/dev\",
                  type: \"tmpfs\",
                  source: \"tmpfs\",
                  options: [\"nosuid\", \"strictatime\", \"mode=755\", \"size=65536k\"]
                },
                {
                  destination: \"/sys\",
                  type: \"sysfs\",
                  source: \"sysfs\",
                  options: [\"nosuid\", \"noexec\", \"nodev\", \"ro\"]
                },
                {
                  destination: \"/tmp\",
                  type: \"tmpfs\",
                  source: \"tmpfs\",
                  options: [\"nosuid\", \"nodev\", \"mode=1777\", \"size=536870912\"]
                },
                {
                  destination: \"/nix/var\",
                  type: \"tmpfs\",
                  source: \"tmpfs\",
                  options: [\"nosuid\", \"nodev\", \"mode=755\", \"size=268435456\"]
                },
                {
                  destination: \"/run\",
                  type: \"tmpfs\",
                  source: \"tmpfs\",
                  options: [\"nosuid\", \"nodev\", \"mode=755\", \"size=67108864\"]
                },
                {
                  destination: \"/workspace\",
                  type: \"bind\",
                  source: \$workspace,
                  options: [\"rbind\", \"rw\", \"nosuid\", \"nodev\"]
                },
                {
                  destination: \"/state\",
                  type: \"bind\",
                  source: \$state,
                  options: [\"rbind\", \"rw\", \"nosuid\", \"nodev\"]
                }
              ],
              linux: {
                namespaces: [
                  { type: \"pid\" },
                  { type: \"ipc\" },
                  { type: \"uts\" },
                  { type: \"mount\" }
                ],
                maskedPaths: [
                  \"/proc/acpi\",
                  \"/proc/asound\",
                  \"/proc/kcore\",
                  \"/proc/keys\",
                  \"/proc/latency_stats\",
                  \"/proc/timer_list\",
                  \"/proc/timer_stats\",
                  \"/proc/sched_debug\",
                  \"/sys/firmware\"
                ],
                readonlyPaths: [
                  \"/proc/bus\",
                  \"/proc/fs\",
                  \"/proc/irq\",
                  \"/proc/sys\",
                  \"/proc/sysrq-trigger\"
                ]
              }
            }" >"$runtime_dir/bundle/config.json"
          jq -e . "$runtime_dir/bundle/config.json" >/dev/null

          exec env GOMAXPROCS=2 runsc \
            --root="$runtime_dir/runsc" \
            --log="$runtime_dir/runsc.log" \
            --log-format=text \
            --rootless=true \
            --ignore-cgroups=true \
            --platform=systrap \
            --network=host \
            --directfs=false \
            --file-access=exclusive \
            --file-access-mounts=shared \
            --overlay2="root:dir=$runtime_dir/overlay,size=4294967296" \
            --host-uds=none \
            --host-fifo=none \
            --net-raw=false \
            --allow-packet-socket-write=false \
            --allow-suid=false \
            --oci-seccomp=false \
            --gvisor-marker-file=true \
            --fdlimit=8192 \
            run \
              --bundle="$runtime_dir/bundle" \
              --user-log="$runtime_dir/user.log" \
              sandbox
        ' _ \
          "$runtime_dir" \
          "$workspace_fd" \
          "$guest_state_fd" \
          "$workspace_device" \
          "$workspace_inode" \
          "$guest_state_device" \
          "$guest_state_inode" \
          "$tenant_id" \
          "$host_uid" \
          "$host_gid" \
          "$address_snapshot" \
          "$nft_rules" \
          "$network_gate" \
          "$namespace_ready" \
          "${rootfs}"
    ) <&3 >&4 &
    sandbox_pid=$!
    exec 3<&-
    exec 4>&-

    printf '%s\n' "$sandbox_pid" >"$cgroup_leaf/cgroup.procs"
    [ "$(read_process_cgroup "$sandbox_pid")" = "$leaf_cgroup_path" ] \
      || fail "sandbox launcher did not enter its resource cgroup"
    printf 'go\n' >"$cgroup_gate"

    wait_for_file "$namespace_ready" "$sandbox_pid" \
      "sandbox namespace launcher" 500
    namespace_pid="$(sed -n '1p' "$namespace_ready")"
    [ "$namespace_pid" = "$sandbox_pid" ] \
      || fail "sandbox namespace process identity changed"
    [ "$(sed -n '2p' "$namespace_ready")" = \
        "$(readlink "/proc/$sandbox_pid/ns/user")" ] \
      || fail "sandbox user namespace attestation failed"
    [ "$(sed -n '3p' "$namespace_ready")" = \
        "$(readlink "/proc/$sandbox_pid/ns/net")" ] \
      || fail "sandbox network namespace attestation failed"
    [ "$(sed -n '4p' "$namespace_ready")" = \
        "$(readlink "/proc/$sandbox_pid/ns/mnt")" ] \
      || fail "sandbox mount namespace attestation failed"
    [ "$(readlink "/proc/$sandbox_pid/ns/user")" != \
        "$(readlink /proc/self/ns/user)" ] \
      || fail "sandbox did not enter a private user namespace"
    [ "$(readlink "/proc/$sandbox_pid/ns/net")" != \
        "$(readlink /proc/self/ns/net)" ] \
      || fail "sandbox did not enter a private network namespace"
    [ "$(read_process_cgroup "$sandbox_pid")" = "$leaf_cgroup_path" ] \
      || fail "sandbox namespace escaped its resource cgroup"

    (
      exec {workspace_fd}<&-
      exec {guest_state_fd}<&-
      exec 9>&-
      exec </dev/null >/dev/null
      IFS= read -r gate_value <"$slirp_gate"
      [ "$gate_value" = "go" ] || exit 2
      exec slirp4netns \
        --configure \
        --cidr=10.0.2.0/24 \
        --mtu=1500 \
        --disable-host-loopback \
        --enable-sandbox \
        --enable-seccomp \
        --ready-fd=3 \
        "$sandbox_pid" \
        tap0 \
        3>"$slirp_ready" \
        >"$runtime_dir/slirp.stdout" \
        2>"$runtime_dir/slirp.stderr"
    ) &
    slirp_pid=$!
    printf '%s\n' "$slirp_pid" >"$cgroup_leaf/cgroup.procs"
    [ "$(read_process_cgroup "$slirp_pid")" = "$leaf_cgroup_path" ] \
      || fail "network helper did not enter the sandbox resource cgroup"
    printf 'go\n' >"$slirp_gate"
    wait_for_file "$slirp_ready" "$slirp_pid" \
      "sandbox network helper" 500
    process_is_running "$sandbox_pid" \
      || fail "sandbox exited while its network was being configured"
    printf 'go\n' >"$network_gate"

    runtime_failure=
    while process_is_running "$sandbox_pid"; do
      if [ -s "$helper_failure" ]; then
        runtime_failure="$(head -n 1 -- "$helper_failure")"
        printf '1\n' >"$cgroup_leaf/cgroup.kill" 2>/dev/null || true
        break
      fi
      if ! process_is_running "$address_monitor_pid"; then
        runtime_failure="host address monitor exited unexpectedly"
      elif ! process_is_running "$address_watch_pid"; then
        runtime_failure="host address watcher exited unexpectedly"
      elif ! process_is_running "$slirp_pid"; then
        runtime_failure="sandbox network helper exited unexpectedly"
      fi
      if [ -n "$runtime_failure" ]; then
        printf '%s\n' "$runtime_failure" >"$helper_failure"
        printf '1\n' >"$cgroup_leaf/cgroup.kill" 2>/dev/null || true
        break
      fi
      sleep 0.05
    done

    set +e
    wait "$sandbox_pid"
    sandbox_status=$?
    set -e
    sandbox_pid=
    if [ -n "$runtime_failure" ]; then
      printf 'agent-sandbox-runner: %s\n' "$runtime_failure" >&2
      if [ "$runtime_failure" = "sandbox network helper exited unexpectedly" ] \
        && [ -s "$runtime_dir/slirp.stderr" ]
      then
        tail -n 40 "$runtime_dir/slirp.stderr" \
          | sed 's/^/agent-sandbox-runner: slirp4netns: /' >&2
      fi
      [ "$sandbox_status" -ne 0 ] || sandbox_status=2
    fi
    if [ "$sandbox_status" -ne 0 ] \
      && [ ! -e "$runtime_dir/cleanup-requested" ]
    then
      printf 'agent-sandbox-runner: runsc exited with status %s\n' \
        "$sandbox_status" >&2
      for sandbox_log in "$runtime_dir/user.log" "$runtime_dir/runsc.log"; do
        if [ -s "$sandbox_log" ]; then
          log_name="$(basename -- "$sandbox_log")"
          tail -c 16384 "$sandbox_log" \
            | tail -n 80 \
            | sed "s/^/agent-sandbox-runner: $log_name: /" >&2
        fi
      done
    fi
    exit "$sandbox_status"
  '';
}
