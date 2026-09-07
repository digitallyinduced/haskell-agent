{ pkgs, agentServer }:

let
  sandboxNix = pkgs.nix.appendPatches [ ./nix-gvisor-pty-keepalive.patch ];

  rootContents = pkgs.buildEnv {
    name = "agent-sandbox-root-contents";
    paths = with pkgs; [
      agentServer
      bashInteractive
      cacert
      coreutils
      curl
      dockerTools.fakeNss
      dockerTools.usrBinEnv
      file
      findutils
      gawk
      gh
      gitMinimal
      gnugrep
      gnused
      gnutar
      gzip
      iana-etc
      iproute2
      jq
      libcap
      sandboxNix
      openssh
      patch
      procps
      ripgrep
      util-linux
      which
    ];
    pathsToLink = [
      "/bin"
      "/etc"
      "/sbin"
      "/usr"
    ];
  };

  image = pkgs.dockerTools.buildImageWithNixDb {
    name = "agent-sandbox-rootfs";
    compressor = "none";
    copyToRoot = rootContents;
    extraCommands = ''
      # copyToRoot preserves the read-only mode of its Nix-store symlink farm.
      # Make only the temporary layer staging directories writable before
      # adding deterministic guest configuration files.
      chmod u+w . etc
      rm -f etc/nsswitch.conf
      mkdir -p \
        dev proc root run state sys tmp workspace \
        etc/nix
      chmod 0700 root
      chmod 1777 tmp

      cat >etc/hosts <<'EOF'
      127.0.0.1 localhost
      ::1 localhost
      EOF
      cat >etc/resolv.conf <<'EOF'
      nameserver 10.0.2.3
      options edns0 timeout:2 attempts:2
      EOF
      cat >etc/nsswitch.conf <<'EOF'
      passwd: files
      group: files
      hosts: files dns
      networks: files
      protocols: files
      services: files
      ethers: files
      rpc: files
      EOF
      cat >etc/nix/nix.conf <<'EOF'
      allowed-users = *
      build-users-group =
      experimental-features = nix-command flakes
      max-jobs = 2
      cores = 2
      sandbox = false
      trusted-users = root
      EOF
      chmod 0444 \
        etc/hosts \
        etc/resolv.conf \
        etc/nsswitch.conf \
        etc/nix/nix.conf
    '';
  };
in
pkgs.runCommand "agent-sandbox-rootfs" { } ''
  set -euo pipefail

  image="$TMPDIR/image"
  mkdir -p "$image" "$out"
  ${pkgs.gnutar}/bin/tar -xf ${image} -C "$image"
  layer="$(${pkgs.jq}/bin/jq -er '
    if length == 1 and (.[0].Layers | length) == 1 then
      .[0].Layers[0]
    else
      error("expected one image containing one layer")
    end
  ' "$image/manifest.json")"
  case "$layer" in
    ""|/*|../*|*/../*|*/..) echo "unsafe image layer path: $layer" >&2; exit 1 ;;
  esac
  test -f "$image/$layer"
  ${pkgs.gnutar}/bin/tar \
    --extract \
    --file="$image/$layer" \
    --directory="$out" \
    --delay-directory-restore \
    --no-same-owner

  # Bootstrap copies this exact registration database into the guest's fresh
  # mutable /nix/var tmpfs before dropping its temporary capabilities.
  cp -a "$out/nix/var/nix" "$out/nix-state-seed"

  test -x "$out/bin/agent-sandbox-worker"
  test -x "$out/bin/bash"
  test -x "$out/bin/nix"
  test -s "$out/etc/ssl/certs/ca-bundle.crt"
  test -s "$out/nix/var/nix/db/db.sqlite"
  test -s "$out/nix-state-seed/db/db.sqlite"
''
