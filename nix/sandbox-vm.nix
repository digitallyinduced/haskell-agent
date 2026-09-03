{
  config,
  lib,
  pkgs,
  modulesPath,
  agentServer,
  ...
}:

let
  worker = "${agentServer}/bin/agent-sandbox-worker";
  workerLauncher = pkgs.writeShellScript "agent-sandbox-worker-launch" ''
    set -eu

    device=/dev/virtio-ports/agent-broker
    while [ ! -c "$device" ]; do
      sleep 0.05
    done

    tenant_id=
    for argument in $(${pkgs.coreutils}/bin/cat /proc/cmdline); do
      case "$argument" in
        agent.tenant_id=*) tenant_id="''${argument#agent.tenant_id=}" ;;
      esac
    done
    case "$tenant_id" in
      ????????-????-????-????-????????????) ;;
      *) exit 2 ;;
    esac
    exec 3<>"$device"
    exec ${worker} \
      --protocol-version 1 \
      --tenant-id "$tenant_id" \
      --workspace /workspace \
      --state /state \
      <&3 >&3 3>&-
  '';
in
{
  imports = [ "${modulesPath}/virtualisation/qemu-vm.nix" ];

  system.stateVersion = "25.11";
  system.name = "agent-tenant-sandbox";

  virtualisation = {
    cores = 2;
    memorySize = 2048;
    diskImage = null;
    # The runner supplies an explicit headless display/serial configuration.
    # Avoid qemu-vm.nix adding its own -nographic serial mux.
    graphics = true;
    writableStore = true;
    useNixStoreImage = true;
    useHostCerts = false;
    qemu.forceAccel = true;

    # The runner adds only these two shares dynamically. Declaring their
    # guest mount points here keeps host paths out of the Nix derivation.
    # Leave the qemu-vm module's root and copied Nix-store filesystems intact.
    fileSystems = {
      "/workspace" = {
        device = "workspace";
        fsType = "9p";
        options = [
          "trans=virtio"
          "version=9p2000.L"
          "msize=104857600"
          "access=any"
        ];
      };
      "/state" = {
        device = "state";
        fsType = "9p";
        options = [
          "trans=virtio"
          "version=9p2000.L"
          "msize=104857600"
          "access=any"
        ];
      };
    };
  };

  boot.kernelParams = [
    "quiet"
    "panic=1"
    "boot.panic_on_fail"
  ];

  users.mutableUsers = false;
  users.allowNoPasswordLogin = true;
  users.users.root.hashedPassword = "!";
  users.groups.agent = { };
  users.users.agent = {
    isSystemUser = true;
    group = "agent";
    home = "/state/home";
    createHome = false;
    shell = pkgs.bashInteractive;
  };

  systemd.tmpfiles.rules = [
    "d /state/home 0700 agent agent -"
    "d /state/tmp 0700 agent agent -"
    "d /state/sessions 0700 agent agent -"
  ];

  services.udev.extraRules = ''
    SUBSYSTEM=="virtio-ports", ATTR{name}=="agent-broker", OWNER="agent", GROUP="agent", MODE="0600"
  '';

  systemd.services.agent-sandbox-worker = {
    description = "Tenant sandbox tool worker";
    wantedBy = [ "multi-user.target" ];
    after = [
      "agent-egress-host-block.service"
      "local-fs.target"
      "systemd-tmpfiles-setup.service"
    ];
    requires = [ "agent-egress-host-block.service" ];
    unitConfig.RequiresMountsFor = [
      "/workspace"
      "/state"
    ];
    path = with pkgs; [
      bashInteractive
      coreutils
      curl
      file
      findutils
      gawk
      gitMinimal
      gnugrep
      gnused
      jq
      nix
      openssh
      patch
      ripgrep
      which
    ];
    environment = {
      HOME = "/state/home";
      TMPDIR = "/state/tmp";
      SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
      NIX_SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
      LANG = "C.UTF-8";
    };
    serviceConfig = {
      Type = "simple";
      User = "agent";
      Group = "agent";
      WorkingDirectory = "/workspace";
      ExecStart = workerLauncher;
      Restart = "no";
      UMask = "0077";

      NoNewPrivileges = true;
      CapabilityBoundingSet = "";
      AmbientCapabilities = "";
      LockPersonality = true;
      PrivateMounts = true;
      PrivateTmp = false;
      ProcSubset = "pid";
      ProtectClock = true;
      ProtectControlGroups = true;
      ProtectHome = true;
      ProtectHostname = true;
      ProtectKernelLogs = true;
      ProtectKernelModules = true;
      ProtectKernelTunables = true;
      ProtectProc = "invisible";
      ProtectSystem = "strict";
      ReadWritePaths = [
        "/workspace"
        "/state"
      ];
      RemoveIPC = true;
      RestrictAddressFamilies = [
        "AF_UNIX"
        "AF_INET"
        "AF_INET6"
      ];
      RestrictRealtime = true;
      RestrictSUIDSGID = true;
      SystemCallArchitectures = "native";
      TasksMax = 512;
    };
  };

  # SLIRP provides outbound networking without a listening host interface.
  # The guest-wide policy blocks host, metadata, link-local, and private
  # destinations. The unprivileged worker cannot change this ruleset.
  networking.useDHCP = lib.mkDefault true;
  networking.firewall.enable = false;
  networking.nftables = {
    enable = true;
    ruleset = ''
      table inet agent_egress {
        set host_ipv4 {
          type ipv4_addr
        }

        set host_ipv6 {
          type ipv6_addr
        }

        chain output {
          type filter hook output priority 0; policy accept;

          oifname "lo" accept
          ct state established,related accept

          ip daddr { 255.255.255.255, 10.0.2.2 } \
            udp sport 68 udp dport 67 accept
          ip daddr 10.0.2.3 udp dport 53 accept
          ip daddr 10.0.2.3 tcp dport 53 accept

          ip daddr @host_ipv4 reject
          ip6 daddr @host_ipv6 reject

          ip daddr {
            0.0.0.0/8,
            10.0.0.0/8,
            100.64.0.0/10,
            127.0.0.0/8,
            169.254.0.0/16,
            172.16.0.0/12,
            192.0.0.0/24,
            192.0.2.0/24,
            192.168.0.0/16,
            198.18.0.0/15,
            198.51.100.0/24,
            203.0.113.0/24,
            224.0.0.0/4,
            240.0.0.0/4
          } reject

          ip6 daddr {
            ::/128,
            ::1/128,
            ::ffff:0:0/96,
            64:ff9b::/96,
            100::/64,
            2001:db8::/32,
            fc00::/7,
            fec0::/10,
            fe80::/10,
            ff00::/8
          } reject
        }
      }
    '';
  };

  systemd.services.agent-egress-host-block = {
    description = "Block the sandbox host's routable addresses";
    wantedBy = [ "multi-user.target" ];
    after = [ "nftables.service" ];
    requires = [ "nftables.service" ];
    before = [ "agent-sandbox-worker.service" ];
    path = with pkgs; [
      coreutils
      gnugrep
      nftables
    ];
    script = ''
      set -eu
      for argument in $(cat /proc/cmdline); do
        case "$argument" in
          agent.block_ipv4=*)
            address="''${argument#agent.block_ipv4=}"
            printf '%s\n' "$address" \
              | grep -Eq '^[0-9]{1,3}(\.[0-9]{1,3}){3}$'
            nft add element inet agent_egress host_ipv4 "{ $address }"
            ;;
          agent.block_ipv6=*)
            address="''${argument#agent.block_ipv6=}"
            printf '%s\n' "$address" \
              | grep -Eqi '^[0-9a-f:]+$'
            nft add element inet agent_egress host_ipv6 "{ $address }"
            ;;
        esac
      done
    '';
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      NoNewPrivileges = true;
      CapabilityBoundingSet = [ "CAP_NET_ADMIN" ];
      AmbientCapabilities = [ "CAP_NET_ADMIN" ];
    };
  };

  nix.settings = {
    allowed-users = [ "agent" ];
    trusted-users = [ "root" ];
    sandbox = true;
    experimental-features = [
      "nix-command"
      "flakes"
    ];
  };

  services.openssh.enable = false;
  documentation.enable = false;
  programs.command-not-found.enable = false;
  security.sudo.enable = false;
}
