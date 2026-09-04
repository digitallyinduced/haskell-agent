{
    pkgs,
    agentCli,
    agentCliSource,
    agentCliRuntimeSource,
    agentCoreSource,
    skylightingSyntaxes,
    bun,
    sourceDateEpoch ? 1,
}:
assert pkgs.stdenv.hostPlatform.isDarwin;
let
    lib = pkgs.lib;
    architecture =
        if pkgs.stdenv.hostPlatform.isAarch64 then "arm64"
        else "x86_64";
    archiveBaseName = "haskell-agent-macos-${architecture}";
    portableFfmpeg =
        (pkgs.ffmpeg.override {
            ffmpegVariant = "headless";
            withHeadlessDeps = false;
            withSmallDeps = false;
            withFullDeps = false;
            withGPL = false;
            withVersion3 = false;
            withStatic = true;
            withShared = false;
            withPixelutils = true;
            buildFfmpeg = true;
            buildFfplay = false;
            buildFfprobe = false;
            buildAvcodec = true;
            buildAvdevice = true;
            buildAvfilter = true;
            buildAvformat = true;
            buildAvutil = true;
            buildPostproc = false;
            buildSwresample = true;
            buildSwscale = false;
            withHtmlDoc = false;
            withManPages = false;
            withPodDoc = false;
            withTxtDoc = false;
        }).overrideAttrs (_: {
            # FFmpeg's check-only scale tools assume libswscale is enabled.
            # The bundle's install check exercises the audio conversion used
            # by agent-cli instead.
            doCheck = false;
        });
    bunLicense = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/oven-sh/bun/bun-v${bun.version}/LICENSE.md";
        hash = "sha256-ucr1JyhpG0BX43EjLCIaEyiDGYvi89Ld+SyQQEyYSxo=";
    };

    bundle = pkgs.stdenvNoCC.mkDerivation {
        pname = "haskell-agent-macos-bundle";
        inherit (agentCli) version;

        dontUnpack = true;
        dontStrip = true;

        nativeBuildInputs = [
            pkgs.darwin.autoSignDarwinBinariesHook
            pkgs.darwin.cctools
            pkgs.darwin.xattr
            pkgs.file
            pkgs.nukeReferences
            pkgs.python3
        ];

        installPhase = ''
            runHook preInstall

            postgresRoot="$out/libexec/postgresql"
            mkdir -p \
                "$out/bin" \
                "$out/lib/deps" \
                "$postgresRoot/bin" \
                "$postgresRoot/lib" \
                "$postgresRoot/share" \
                "$out/share/agent-cli" \
                "$out/share/agent-cli-runtime" \
                "$out/share/agent-core" \
                "$out/share/haskell-agent" \
                "$out/share/skylighting" \
                "$out/share/doc/haskell-agent" \
                "$out/share/zoneinfo"

            # Keep the real Mach-O rather than the Nix shell wrapper.
            install -m 755 ${agentCli}/bin/agent-cli "$out/bin/agent-cli"

            # Preserve the capabilities of the normal Nix package without
            # requiring any of these tools to be installed on the host.
            install -m 755 ${portableFfmpeg}/bin/ffmpeg "$out/bin/ffmpeg"
            install -m 755 ${bun}/bin/bun "$out/bin/bun"
            install -m 755 ${pkgs.ripgrep}/bin/rg "$out/bin/rg"
            install -m 755 ${pkgs.zstd}/bin/zstd "$out/bin/zstd"

            # PostgreSQL discovers its share and library directories relative
            # to its executable after relocation. Keep its complete runtime
            # layout so initdb, pg_ctl, and dynamically loaded server modules
            # continue to work.
            cp -RL ${pkgs.postgresql_18}/bin/. "$postgresRoot/bin/"
            cp -RL ${pkgs.postgresql_18}/lib/. "$postgresRoot/lib/"
            cp -RL ${pkgs.postgresql_18.lib}/lib/. "$postgresRoot/lib/"
            cp -RL ${pkgs.postgresql_18}/share/. "$postgresRoot/share/"

            cp -R ${agentCliSource}/data "$out/share/agent-cli/"
            cp -R ${agentCliSource}/skills "$out/share/agent-cli/"
            cp -R ${agentCliRuntimeSource}/config \
                "$out/share/agent-cli-runtime/"
            cp -R ${agentCoreSource}/data "$out/share/agent-core/"
            cp -R ${skylightingSyntaxes}/share/skylighting/xml \
                "$out/share/skylighting/"
            cp -R ${pkgs.tzdata}/share/zoneinfo/. "$out/share/zoneinfo/"
            install -m 644 ${agentCliSource}/LICENSE \
                "$out/share/doc/haskell-agent/LICENSE"
            touch "$out/share/haskell-agent/portable"

            licenses="$out/share/doc/haskell-agent/licenses"
            mkdir -p "$licenses"
            install -m 644 ${portableFfmpeg.src}/LICENSE.md \
                "$licenses/FFmpeg-LICENSE.md"
            install -m 644 ${portableFfmpeg.src}/COPYING.LGPLv2.1 \
                "$licenses/FFmpeg-COPYING.LGPLv2.1"
            install -m 644 ${bunLicense} "$licenses/Bun-LICENSE.md"
            install -m 644 ${pkgs.postgresql_18.src}/COPYRIGHT \
                "$licenses/PostgreSQL-COPYRIGHT"
            install -m 644 ${pkgs.ripgrep.src}/COPYING \
                "$licenses/ripgrep-COPYING"
            install -m 644 ${pkgs.ripgrep.src}/LICENSE-MIT \
                "$licenses/ripgrep-LICENSE-MIT"
            install -m 644 ${pkgs.zstd.src}/COPYING \
                "$licenses/zstd-COPYING"
            install -m 644 ${pkgs.zstd.src}/LICENSE \
                "$licenses/zstd-LICENSE"
            cat > "$out/share/doc/haskell-agent/THIRD_PARTY_NOTICES.md" <<'EOF'
            # Third-party notices

            This standalone archive includes these directly distributed tools:

            - FFmpeg ${portableFfmpeg.version} (LGPL-2.1-or-later)
            - Bun ${bun.version} (MIT; its JavaScriptCore component is LGPL-2.1-only)
            - PostgreSQL ${pkgs.postgresql_18.version} (PostgreSQL License)
            - ripgrep ${pkgs.ripgrep.version} (MIT OR Unlicense)
            - zstd ${pkgs.zstd.version} (BSD-3-Clause OR GPL-2.0-only)

            The corresponding available license texts are in the `licenses`
            directory. Bun's complete notices and source are available from:

            - https://github.com/oven-sh/bun/blob/main/LICENSE.md
            - https://bun.sh/docs/project/licensing
            EOF

            chmod -R u+w "$out"

            # Nix's PostgreSQL build records its build-host locale command in
            # the server. Replace that command without changing the Mach-O's
            # size before removing all remaining inert store references.
            python3 - \
                "$postgresRoot/bin/postgres" \
                '${pkgs.darwin.adv_cmds}/bin/locale -a' \
                '/usr/bin/locale -a' \
                <<'PY'
            from pathlib import Path
            import sys

            path = Path(sys.argv[1])
            old = sys.argv[2].encode()
            new = sys.argv[3].encode()
            if len(new) > len(old):
                raise SystemExit("replacement locale command is too long")
            data = path.read_bytes()
            count = data.count(old)
            if count != 1:
                raise SystemExit(f"expected one locale command, found {count}")
            path.write_bytes(data.replace(old, new + b" " * (len(old) - len(new))))
            PY

            # Nix store paths in Mach-O load commands are not usable after the
            # archive is copied to another Mac. Copy each non-system dylib
            # under a path-identity-preserving name and rewrite all load
            # commands. Keeping distinct source identities avoids silently
            # merging ABI-incompatible libraries that share a basename.
            is_macho() {
                file -b "$1" | grep -q '^Mach-O'
            }

            python3 ${./bundle-macos-dylibs.py} \
                --destination "$out/lib/deps" \
                --install-prefix '@executable_path/../lib/deps/' \
                "$out/bin/agent-cli" \
                "$out/bin/ffmpeg" \
                "$out/bin/bun" \
                "$out/bin/rg" \
                "$out/bin/zstd"

            postgresTargets=()
            while IFS= read -r -d $'\0' binary; do
                if is_macho "$binary"; then
                    postgresTargets+=("$binary")
                fi
            done < <(
                find "$postgresRoot/bin" "$postgresRoot/lib" \
                    -type f -print0 | sort -z
            )
            python3 ${./bundle-macos-dylibs.py} \
                --destination "$postgresRoot/lib/deps" \
                --install-prefix '@executable_path/../lib/deps/' \
                "''${postgresTargets[@]}"

            # Cabal data paths are overridden by Main when the portable marker
            # is present. Nuke inert build-time store hashes after all live load
            # commands have been made relative to the extracted bundle.
            while IFS= read -r -d $'\0' binary; do
                if is_macho "$binary"; then
                    nuke-refs "$binary"
                fi
            done < <(find "$out" -type f -print0 | sort -z)

            bad_load_commands=0
            while IFS= read -r -d $'\0' binary; do
                if is_macho "$binary" \
                    && otool -l "$binary" \
                        | grep -E '^[[:space:]]+(name|path) /nix/store/' \
                            >/dev/null
                then
                    echo "error: Nix store load command remains in $binary" >&2
                    bad_load_commands=1
                fi
            done < <(find "$out" -type f -print0 | sort -z)
            if [[ "$bad_load_commands" -ne 0 ]]; then
                exit 1
            fi

            # Files copied out of the store can carry provenance metadata on a
            # local Darwin builder. Clear it before the post-fixup hook applies
            # a fresh ad-hoc signature to every Mach-O in the bundle.
            xattr -cr "$out"

            runHook postInstall
        '';

        doInstallCheck = true;
        installCheckPhase = ''
            runHook preInstallCheck

            home="$TMPDIR/home"
            postgresRoot="$out/libexec/postgresql"
            mkdir -p "$home"

            run_clean() {
                env -i \
                    HOME="$home" \
                    PATH=/usr/bin:/bin \
                    LC_ALL=C \
                    TZDIR="$out/share/zoneinfo" \
                    "$@"
            }

            run_clean "$out/bin/agent-cli" --version
            run_clean "$out/bin/ffmpeg" -version
            run_clean "$out/bin/ffmpeg" \
                -v error \
                -f lavfi \
                -i 'sine=frequency=1000:duration=0.02' \
                -f s16le \
                -ac 1 \
                -ar 24000 \
                -y "$TMPDIR/tone.pcm"
            test -s "$TMPDIR/tone.pcm"
            run_clean "$out/bin/bun" --version
            run_clean "$out/bin/bun" \
                -e 'if (1 + 1 !== 2) process.exit(1)'
            run_clean "$out/bin/rg" --version
            run_clean "$out/bin/zstd" --version
            run_clean "$postgresRoot/bin/postgres" --version
            run_clean "$postgresRoot/bin/initdb" --version
            run_clean "$postgresRoot/bin/initdb" \
                --pgdata="$TMPDIR/postgres-data" \
                --no-locale \
                --encoding=UTF8 \
                --auth=trust

            test -f "$out/share/agent-cli-runtime/config/models.default.json"
            test -f "$out/share/agent-core/data/code-mode/worker.mjs"
            test -f "$out/share/haskell-agent/portable"

            is_macho() {
                file -b "$1" | grep -q '^Mach-O'
            }
            while IFS= read -r -d $'\0' binary; do
                if is_macho "$binary"; then
                    /usr/bin/codesign --verify --strict "$binary"
                fi
            done < <(find "$out" -type f -print0 | sort -z)

            runHook postInstallCheck
        '';

        meta = {
            description =
                "Relocatable ${architecture} macOS bundle for agent-cli";
            license = lib.unique (lib.flatten [
                agentCli.meta.license
                portableFfmpeg.meta.license
                bun.meta.license
                pkgs.postgresql_18.meta.license
                pkgs.ripgrep.meta.license
                pkgs.zstd.meta.license
            ]);
            mainProgram = "agent-cli";
            platforms = lib.platforms.darwin;
        };
    };

    archive = pkgs.runCommand "${archiveBaseName}-archive" {
        nativeBuildInputs = [
            pkgs.gnutar
            pkgs.gzip
        ];
    } ''
        stage="$TMPDIR/archive"
        mkdir -p "$stage/${archiveBaseName}" "$out"
        cp -R ${bundle}/. "$stage/${archiveBaseName}/"
        # Nix store objects are read-only. Restore owner write permission so a
        # non-Nix installation can be replaced or removed normally.
        chmod -R u+w "$stage/${archiveBaseName}"

        tar \
            --sort=name \
            --mtime='@${toString sourceDateEpoch}' \
            --owner=0 \
            --group=0 \
            --numeric-owner \
            -C "$stage" \
            -cf - \
            "${archiveBaseName}" \
            | gzip -9n > "$out/${archiveBaseName}.tar.gz"

        (
            cd "$out"
            sha256sum "${archiveBaseName}.tar.gz" \
                > "${archiveBaseName}.tar.gz.sha256"
        )
    '';
in
{
    inherit archive archiveBaseName bundle;
}
