{ self }:

{
  pkgs,
  name,
  defaultAgent,
  agents,
  environments ? { },
  skills ? { },
  agentPackage ? self.packages.${pkgs.stdenv.hostPlatform.system}.default,
}:

let
  inherit (pkgs) lib;

  fail = message: throw "mkAgentBundle: ${message}";
  ensure = condition: message: if condition then true else fail message;
  maxBundleManifestBytes = 1048576;
  identifierPattern = "^[A-Za-z0-9][A-Za-z0-9._-]*$";
  isIdentifier = value: builtins.isString value && builtins.match identifierPattern value != null;
  isNonBlank = value: builtins.isString value && builtins.match "[[:space:]]*" value == null;

  validateKeys =
    label: allowed: value:
    let
      unknown = lib.subtractLists allowed (builtins.attrNames value);
    in
    ensure (unknown == [ ]) "${label} has unknown attributes: ${lib.concatStringsSep ", " unknown}";

  validateName =
    label: value:
    ensure (isIdentifier value) "${label} must start with a letter or digit and contain only letters, digits, '.', '_', or '-'";

  validateEnvironment =
    environmentName: environment:
    if !builtins.isAttrs environment then
      fail "environment '${environmentName}' must be an attribute set"
    else
      validateName "environment name '${environmentName}'" environmentName
      && validateKeys "environment '${environmentName}'" [ "packages" ] environment
      && ensure (environment ? packages) "environment '${environmentName}' must declare packages"
      && ensure (
        builtins.isList environment.packages && environment.packages != [ ]
      ) "environment '${environmentName}'.packages must be a non-empty list"
      && ensure (lib.all lib.isDerivation environment.packages) "environment '${environmentName}'.packages must contain only packages";

  validateSkill =
    skillName: skillRoot:
    validateName "skill name '${skillName}'" skillName
    && ensure (
      builtins.typeOf skillRoot == "path" || lib.isDerivation skillRoot
    ) "skill '${skillName}' must be a path or package"
    && ensure (
      builtins.typeOf skillRoot != "path" || builtins.readFileType skillRoot == "directory"
    ) "skill '${skillName}' must be a directory";

  validateBoolOption =
    label: options: option:
    ensure (
      !(builtins.hasAttr option options) || builtins.isBool options.${option}
    ) "${label}.${option} must be a boolean";

  validateAgent =
    agentName: agent:
    if !builtins.isAttrs agent then
      fail "agent '${agentName}' must be an attribute set"
    else
      let
        allowedAgentKeys = [
          "description"
          "instructions"
          "instructionsText"
          "model"
          "effort"
          "environment"
          "skills"
          "tools"
          "workspace"
          "maxTurns"
        ];
        tools = agent.tools or { };
        workspace = agent.workspace or { };
        selectedSkills = agent.skills or [ ];
        environment = agent.environment or null;
        model = agent.model or null;
        effort = agent.effort or null;
        description = agent.description or null;
        maxTurns = agent.maxTurns or null;
        hasInstructions = agent ? instructions;
        hasInstructionsText = agent ? instructionsText;
        renderedInstructions =
          if hasInstructions then builtins.readFile agent.instructions else agent.instructionsText;
      in
      validateName "agent name '${agentName}'" agentName
      && validateKeys "agent '${agentName}'" allowedAgentKeys agent
      && ensure (
        hasInstructions != hasInstructionsText
      ) "agent '${agentName}' must set exactly one of instructions or instructionsText"
      && ensure (
        !hasInstructions || builtins.typeOf agent.instructions == "path"
      ) "agent '${agentName}'.instructions must be a literal path"
      && ensure (
        !hasInstructionsText || builtins.isString agent.instructionsText
      ) "agent '${agentName}'.instructionsText must be a string"
      && ensure (isNonBlank renderedInstructions) "agent '${agentName}' instructions must not be blank"
      && ensure (
        description == null || builtins.isString description
      ) "agent '${agentName}'.description must be a string"
      && ensure (
        model == null || isNonBlank model
      ) "agent '${agentName}'.model must be a non-empty logical alias"
      && ensure (
        effort == null
        || lib.elem effort [
          "none"
          "low"
          "medium"
          "high"
          "xhigh"
          "max"
        ]
      ) "agent '${agentName}'.effort must be none, low, medium, high, xhigh, or max"
      && ensure (
        environment == null || (builtins.isString environment && builtins.hasAttr environment environments)
      ) "agent '${agentName}' references an unknown environment"
      && ensure (
        builtins.isList selectedSkills && lib.all builtins.isString selectedSkills
      ) "agent '${agentName}'.skills must be a list of names"
      && ensure (lib.all (
        skill: builtins.hasAttr skill skills
      ) selectedSkills) "agent '${agentName}' references an unknown skill"
      && ensure (
        lib.unique selectedSkills == selectedSkills
      ) "agent '${agentName}'.skills contains duplicates"
      && ensure (builtins.isAttrs tools) "agent '${agentName}'.tools must be an attribute set"
      && validateKeys "agent '${agentName}'.tools" [ "bash" "ghci" "computerUse" "codeMode" ] tools
      && validateBoolOption "agent '${agentName}'.tools" tools "bash"
      && validateBoolOption "agent '${agentName}'.tools" tools "ghci"
      && validateBoolOption "agent '${agentName}'.tools" tools "computerUse"
      && validateBoolOption "agent '${agentName}'.tools" tools "codeMode"
      && ensure (builtins.isAttrs workspace) "agent '${agentName}'.workspace must be an attribute set"
      && validateKeys "agent '${agentName}'.workspace" [ "worktree" "agentsMd" "ambientSkills" ] workspace
      && validateBoolOption "agent '${agentName}'.workspace" workspace "worktree"
      && validateBoolOption "agent '${agentName}'.workspace" workspace "agentsMd"
      && validateBoolOption "agent '${agentName}'.workspace" workspace "ambientSkills"
      && ensure (
        maxTurns == null || (builtins.isInt maxTurns && maxTurns > 0)
      ) "agent '${agentName}'.maxTurns must be a positive integer";

  validation =
    validateName "bundle name" name
    && ensure (lib.isDerivation agentPackage) "agentPackage must be a package"
    && ensure (builtins.isAttrs environments) "environments must be an attribute set"
    && ensure (builtins.isAttrs skills) "skills must be an attribute set"
    && ensure (builtins.isAttrs agents && agents != { }) "agents must be a non-empty attribute set"
    && validateName "defaultAgent" defaultAgent
    && ensure (builtins.hasAttr defaultAgent agents) "defaultAgent '${defaultAgent}' is not declared"
    && lib.all lib.id (lib.mapAttrsToList validateEnvironment environments)
    && lib.all lib.id (lib.mapAttrsToList validateSkill skills)
    && lib.all lib.id (lib.mapAttrsToList validateAgent agents)
    && ensure (
      builtins.stringLength manifestJSON <= maxBundleManifestBytes
    ) "manifest exceeds ${toString maxBundleManifestBytes} bytes";

  checkedEnvironmentPackages = lib.mapAttrs (
    environmentName: environment:
    lib.imap0 (
      index: package:
      pkgs.runCommand "${name}-environment-${environmentName}-${toString index}" { } ''
        if [ ! -d ${package}/bin ]; then
          echo "mkAgentBundle: environment '${environmentName}' package has no bin directory: ${package}" >&2
          exit 1
        fi
        mkdir -p "$out"
        ln -s ${package}/bin "$out/bin"
      ''
    ) environment.packages
  ) environments;

  checkedSkills = lib.mapAttrs (
    skillName: skillRoot:
    pkgs.runCommand "${name}-skill-${skillName}" { } ''
      if [ ! -d ${skillRoot} ]; then
        echo "mkAgentBundle: skill '${skillName}' is not a directory: ${skillRoot}" >&2
        exit 1
      fi
      ln -s ${skillRoot} "$out"
    ''
  ) skills;

  manifestAgents = lib.mapAttrs (
    _agentName: agent:
    let
      tools = agent.tools or { };
      workspace = agent.workspace or { };
    in
    {
      description = agent.description or null;
      instructions =
        if agent ? instructions then builtins.readFile agent.instructions else agent.instructionsText;
      model = agent.model or null;
      effort = agent.effort or null;
      environment = agent.environment or null;
      skills = agent.skills or [ ];
      tools = {
        bash = tools.bash or true;
        ghci = tools.ghci or false;
        computer_use = tools.computerUse or false;
        code_mode = tools.codeMode or false;
      };
      workspace = {
        worktree = workspace.worktree or false;
        agents_md = workspace.agentsMd or false;
        ambient_skills = workspace.ambientSkills or false;
      };
      max_turns = agent.maxTurns or null;
    }
  ) agents;

  manifest = {
    format = "haskell-agent-bundle";
    version = 1;
    system = pkgs.stdenv.hostPlatform.system;
    inherit name;
    default_agent = defaultAgent;
    environments = lib.mapAttrs (environmentName: _environment: {
      path = lib.makeBinPath checkedEnvironmentPackages.${environmentName};
    }) environments;
    skills = lib.mapAttrs (skillName: _skillRoot: {
      path = "${checkedSkills.${skillName}}";
    }) skills;
    agents = manifestAgents;
  };

  manifestJSON = builtins.toJSON manifest;
  manifestFile = pkgs.writeText "${name}-agent-bundle.json" manifestJSON;

  environmentRuntimeEntries = lib.concatLists (
    lib.mapAttrsToList (
      environmentName: environment:
      lib.imap0 (index: package: {
        name = "environment-${environmentName}-${toString index}";
        path = package;
      }) checkedEnvironmentPackages.${environmentName}
    ) environments
  );
  skillRuntimeEntries = lib.mapAttrsToList (skillName: _skillRoot: {
    name = "skill-${skillName}";
    path = checkedSkills.${skillName};
  }) skills;
  runtime = pkgs.linkFarm "${name}-agent-bundle-runtime" (
    [
      {
        name = "agent-cli";
        path = agentPackage;
      }
      {
        name = "manifest.json";
        path = manifestFile;
      }
    ]
    ++ environmentRuntimeEntries
    ++ skillRuntimeEntries
  );
in
assert validation;
pkgs.runCommand name
  {
    nativeBuildInputs = [ pkgs.makeWrapper ];
    passthru = {
      inherit manifest manifestFile runtime;
    };
    meta = {
      description = "Declarative haskell-agent bundle ${name}";
      mainProgram = name;
      platforms = [ pkgs.stdenv.hostPlatform.system ];
    };
  }
  ''
    mkdir -p "$out/bin" "$out/share/haskell-agent"
    cp ${manifestFile} "$out/manifest.json"
    ln -s "$out/manifest.json" "$out/share/haskell-agent/bundle.json"
    ln -s ${runtime} "$out/runtime"
    makeWrapper ${lib.getExe' agentPackage "agent-cli"} "$out/bin/${name}" \
      --add-flags bundle \
      --add-flags run \
      --add-flags "$out/manifest.json"
  ''
