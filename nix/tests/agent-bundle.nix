{ self, pkgs }:

let
  mkAgentBundle = import ../lib/mk-agent-bundle.nix { inherit self; };

  fakeAgentCli = pkgs.writeShellScriptBin "agent-cli" ''
    printf '%s\n' "$@" > "$BUNDLE_TEST_CAPTURE"
  '';

  reviewSkill = pkgs.runCommand "review-skill" { } ''
    mkdir -p "$out"
    cat > "$out/SKILL.md" <<'EOF'
    ---
    name: review
    description: Review a patch.
    ---
    Review the current patch carefully.
    EOF
  '';
  literalReviewSkill = ./fixtures/review-skill;
  oversizedInstructions = builtins.foldl' (text: _: text + text) "x" (
    builtins.genList (index: index) 20
  );

  bundle = mkAgentBundle {
    inherit pkgs;
    agentPackage = fakeAgentCli;
    name = "review-bundle";
    defaultAgent = "reviewer";
    environments.dev.packages = [ pkgs.hello ];
    skills = {
      review = reviewSkill;
      literal = literalReviewSkill;
    };
    agents.reviewer = {
      instructionsText = "Review the current patch carefully.";
      description = "A reproducible reviewer";
      model = "review";
      effort = "high";
      environment = "dev";
      skills = [ "review" ];
      tools.ghci = true;
      maxTurns = 4;
    };
  };

  invalidProvider = builtins.tryEval (
    (mkAgentBundle {
      inherit pkgs;
      agentPackage = fakeAgentCli;
      name = "invalid-provider";
      defaultAgent = "main";
      agents.main = {
        instructionsText = "Do the task.";
        provider = "openai";
      };
    }).drvPath
  );

  invalidEnvironment = builtins.tryEval (
    (mkAgentBundle {
      inherit pkgs;
      agentPackage = fakeAgentCli;
      name = "invalid-environment";
      defaultAgent = "main";
      agents.main = {
        instructionsText = "Do the task.";
        environment = "missing";
      };
    }).drvPath
  );

  ambiguousInstructions = builtins.tryEval (
    (mkAgentBundle {
      inherit pkgs;
      agentPackage = fakeAgentCli;
      name = "ambiguous-instructions";
      defaultAgent = "main";
      agents.main = {
        instructions = ./telegram-module.nix;
        instructionsText = "Do the task.";
      };
    }).drvPath
  );

  blankInstructions = builtins.tryEval (
    (mkAgentBundle {
      inherit pkgs;
      agentPackage = fakeAgentCli;
      name = "blank-instructions";
      defaultAgent = "main";
      agents.main.instructionsText = " \n\t";
    }).drvPath
  );

  blankInstructionFile = builtins.tryEval (
    (mkAgentBundle {
      inherit pkgs;
      agentPackage = fakeAgentCli;
      name = "blank-instruction-file";
      defaultAgent = "main";
      agents.main.instructions = ./fixtures/blank-instructions.md;
    }).drvPath
  );

  blankModel = builtins.tryEval (
    (mkAgentBundle {
      inherit pkgs;
      agentPackage = fakeAgentCli;
      name = "blank-model";
      defaultAgent = "main";
      agents.main = {
        instructionsText = "Do the task.";
        model = "  ";
      };
    }).drvPath
  );

  nonDirectorySkill = builtins.tryEval (
    (mkAgentBundle {
      inherit pkgs;
      agentPackage = fakeAgentCli;
      name = "non-directory-skill";
      defaultAgent = "main";
      skills.invalid = ./telegram-module.nix;
      agents.main.instructionsText = "Do the task.";
    }).drvPath
  );

  oversizedManifest = builtins.tryEval (
    (mkAgentBundle {
      inherit pkgs;
      agentPackage = fakeAgentCli;
      name = "oversized-manifest";
      defaultAgent = "main";
      agents.main.instructionsText = oversizedInstructions;
    }).drvPath
  );
in
assert !invalidProvider.success;
assert !invalidEnvironment.success;
assert !ambiguousInstructions.success;
assert !blankInstructions.success;
assert !blankInstructionFile.success;
assert !blankModel.success;
assert !nonDirectorySkill.success;
assert !oversizedManifest.success;
pkgs.runCommand "agent-bundle-test"
  {
    nativeBuildInputs = [ pkgs.jq ];
  }
  ''
    test -x ${bundle}/bin/review-bundle
    test -f ${bundle}/manifest.json
    test -f ${bundle}/share/haskell-agent/bundle.json
    test -f ${bundle}/runtime/skill-review/SKILL.md
    test -f ${bundle}/runtime/skill-literal/SKILL.md
    test -x ${bundle}/runtime/environment-dev-0/bin/hello

    jq -e '
      .format == "haskell-agent-bundle"
      and .version == 1
      and .name == "review-bundle"
      and .default_agent == "reviewer"
      and .environments.dev.path == "${bundle.manifest.environments.dev.path}"
      and .skills.review.path == "${bundle.manifest.skills.review.path}"
      and .skills.literal.path == "${bundle.manifest.skills.literal.path}"
      and .agents.reviewer.model == "review"
      and .agents.reviewer.effort == "high"
      and .agents.reviewer.tools.ghci == true
      and .agents.reviewer.workspace.agents_md == false
      and .agents.reviewer.workspace.ambient_skills == false
      and .agents.reviewer.max_turns == 4
      and (.agents.reviewer | has("provider") | not)
    ' ${bundle}/manifest.json

    literal_skill_path="$(jq -r '.skills.literal.path' ${bundle}/manifest.json)"
    case "$literal_skill_path" in
      /nix/store/*) ;;
      *) echo "literal skill did not resolve into the Nix store" >&2; exit 1 ;;
    esac
    test -f "$literal_skill_path/SKILL.md"
    test "$(readlink ${bundle}/runtime/skill-literal)" = "$literal_skill_path"
    environment_path="$(jq -r '.environments.dev.path' ${bundle}/manifest.json)"
    test -x "$environment_path/hello"

    export BUNDLE_TEST_CAPTURE="$TMPDIR/arguments"
    ${bundle}/bin/review-bundle -p "hello"
    test "$(sed -n '1p' "$BUNDLE_TEST_CAPTURE")" = bundle
    test "$(sed -n '2p' "$BUNDLE_TEST_CAPTURE")" = run
    test "$(sed -n '3p' "$BUNDLE_TEST_CAPTURE")" = ${bundle}/manifest.json
    test "$(sed -n '4p' "$BUNDLE_TEST_CAPTURE")" = -p
    test "$(sed -n '5p' "$BUNDLE_TEST_CAPTURE")" = hello

    touch "$out"
  ''
