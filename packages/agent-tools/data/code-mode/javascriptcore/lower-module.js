/*
 * Loaded with the pinned Acorn distribution in a trusted parser context.
 * Only the returned source string crosses into the fresh execution context.
 *
 * This implements the single-module contract: no namespace is exported and
 * static dependencies are forbidden. The execution context must independently
 * prohibit string code generation and provide no dynamic module loader.
 * Dynamic import expressions intentionally remain engine syntax: rewriting
 * them to function calls changes exception, await, and generator-yield
 * semantics during argument evaluation.
 */
"use strict";

globalThis.lowerModule = (() => {
  const parse = acorn.parse;

  function childNodes(node) {
    const children = [];
    for (const key of Object.keys(node)) {
      const value = node[key];
      if (Array.isArray(value)) {
        for (const item of value) {
          if (item && typeof item.type === "string") children.push(item);
        }
      } else if (value && typeof value.type === "string") {
        children.push(value);
      }
    }
    return children;
  }

  return function lowerModule(source) {
    if (typeof source !== "string") {
      throw new TypeError("module source must be a string");
    }

    // Parsing as a module is essential: an async function alone would accept
    // top-level return and miss module declaration/strict-mode early errors.
    const defaultPrefixEnds = new Map();
    let exportTokenStart = -1;
    const program = parse(source, {
      ecmaVersion: "latest",
      sourceType: "module",
      allowHashBang: true,
      onToken(token) {
        if (exportTokenStart !== -1 && token.type.label === "default") {
          defaultPrefixEnds.set(exportTokenStart, token.end);
        }
        exportTokenStart = token.type.label === "export" ? token.start : -1;
      },
    });
    const identifiers = new Set();
    const pending = [program];
    while (pending.length !== 0) {
      const node = pending.pop();
      if (node.type === "Identifier") identifiers.add(node.name);
      if (
        node.type === "ImportDeclaration" ||
        node.type === "ExportAllDeclaration" ||
        (node.type === "ExportNamedDeclaration" && node.source !== null)
      ) {
        throw new Error(
          `module imports are unavailable: ${String(node.source.value)}`,
        );
      }
      pending.push(...childNodes(node));
    }

    // Acorn supplies normalized names, including escaped identifier spellings.
    // The private binding is lexical, never a discoverable global property.
    let sequence = 0;
    let metadataIdentifier;
    do {
      metadataIdentifier = `__codeModeModuleMetadata${sequence++}`;
    } while (identifiers.has(metadataIdentifier));

    const edits = [];
    let usesMetadata = false;
    function replace(start, end, replacement) {
      edits.push({ start, end, replacement });
    }

    function visit(node, moduleThis) {
      switch (node.type) {
        case "ThisExpression":
          if (moduleThis) replace(node.start, node.end, "(void 0)");
          return;
        case "MetaProperty":
          if (node.meta.name === "import") {
            usesMetadata = true;
            replace(node.start, node.end, metadataIdentifier);
          }
          return;
        case "FunctionDeclaration":
        case "FunctionExpression":
          for (const parameter of node.params) visit(parameter, false);
          visit(node.body, false);
          return;
        case "ClassDeclaration":
        case "ClassExpression":
          if (node.superClass) visit(node.superClass, moduleThis);
          for (const element of node.body.body) {
            if (element.type === "StaticBlock") {
              visit(element, false);
            } else {
              // Computed names run in the enclosing lexical environment.
              // Field initializers and method bodies have their own this.
              if (element.computed) visit(element.key, moduleThis);
              if (element.value) visit(element.value, false);
            }
          }
          return;
        case "ExportNamedDeclaration":
          if (node.declaration) {
            replace(node.start, node.declaration.start, "");
            visit(node.declaration, moduleThis);
          } else {
            // Module parsing already checked missing and duplicate exports.
            replace(node.start, node.end, ";");
          }
          return;
        case "ExportDefaultDeclaration": {
          const declaration = node.declaration;
          if (
            (declaration.type === "FunctionDeclaration" ||
              declaration.type === "ClassDeclaration") &&
            declaration.id !== null
          ) {
            // Retain the declaration, including function hoisting and its
            // local name binding, rather than turning it into an expression.
            replace(node.start, declaration.start, "");
          } else {
            // An object property performs NamedEvaluation with "default",
            // including anonymous class naming before static initializers.
            // Its value is discarded because this host exposes no namespace.
            // Preserve parentheses omitted from Acorn expression ranges.
            const prefixEnd = defaultPrefixEnds.get(node.start);
            if (prefixEnd === undefined) throw new Error("missing export prefix");
            const expressionEnd =
              source[node.end - 1] === ";" ? node.end - 1 : node.end;
            replace(node.start, prefixEnd, "({default: ");
            replace(expressionEnd, expressionEnd, "});");
          }
          visit(declaration, moduleThis);
          return;
        }
        default:
          // Arrows intentionally retain the surrounding lexical this.
          for (const child of childNodes(node)) visit(child, moduleThis);
      }
    }
    visit(program, true);

    if (source.startsWith("#!")) replace(0, 2, "//");
    edits.sort((left, right) => left.start - right.start || left.end - right.end);
    let position = 0;
    const segments = [];
    for (const edit of edits) {
      if (edit.start < position) throw new Error("overlapping module source edits");
      segments.push(source.slice(position, edit.start), edit.replacement);
      position = edit.end;
    }
    segments.push(source.slice(position));

    // An arrow at script scope introduces no arguments binding. Do not place
    // this expression inside an ordinary-function bootstrap. There are no
    // native hooks or parser objects in the generated execution environment.
    const parameters = usesMetadata ? metadataIdentifier : "";
    const argumentsSource = usesMetadata ? "{__proto__: null}" : "";
    return (
      `(async (${parameters}) => {\n"use strict";\n` +
      segments.join("") +
      `\n})(${argumentsSource})`
    );
  };
})();
