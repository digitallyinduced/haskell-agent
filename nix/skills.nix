{ pkgs }:

let
  inherit (pkgs) lib;
in
{
  # Build one Agent Skill as a store path whose immediate children are skills.
  # The source may be a fetched repository; skillPath selects the skill within it.
  mkSkill =
    {
      name,
      src,
      skillPath ? ".",
      patches ? [ ],
      licensePath ? null,
      postPatch ? "",
    }:
    pkgs.runCommand "${name}-skill"
      {
        nativeBuildInputs = lib.optional (patches != [ ]) pkgs.patch;
      }
      ''
        sourceSkill="${src}/${skillPath}"
        targetSkill="$out/${name}"

        if [ ! -f "$sourceSkill/SKILL.md" ]; then
          echo "mkSkill: missing $sourceSkill/SKILL.md" >&2
          exit 1
        fi

        mkdir -p "$out"
        cp -R "$sourceSkill" "$targetSkill"
        chmod -R u+w "$targetSkill"
        ${lib.concatMapStringsSep "\n" (patch: ''
          patch -l -d "$targetSkill" -p1 < ${patch}
        '') patches}
        ${lib.optionalString (licensePath != null) ''
          cp "${src}/${licensePath}" "$targetSkill/LICENSE"
        ''}
        ${lib.optionalString (postPatch != "") ''
          (cd "$targetSkill"; ${postPatch})
        ''}
      '';

  # Merge independently built skill roots into one root consumable by the agent.
  mkSkillBundle =
    {
      name ? "agent-skills",
      skills,
    }:
    pkgs.symlinkJoin {
      inherit name;
      paths = skills;
    };
}
