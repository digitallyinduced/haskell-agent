import vm from "node:vm";
import readline from "node:readline";

const pendingCalls = new Map();
let nextCallId = 0;
let executionStarted = false;

function send(message) {
  process.stdout.write(`${JSON.stringify(message)}\n`);
}

function fail(id, message) {
  send({
    jsonrpc: "2.0",
    id,
    error: { code: -32000, message: String(message) },
  });
}

function toolPathProxy(path, allowedTools) {
  const callable = () => undefined;
  Object.setPrototypeOf(callable, null);

  return new Proxy(callable, {
    get(_target, property) {
      if (typeof property !== "string" || property === "then") {
        return undefined;
      }
      const childPath = path ? `${path}.${property}` : property;
      return toolPathProxy(childPath, allowedTools);
    },
    apply(_target, _this, argumentsList) {
      if (!allowedTools.has(path)) {
        return Promise.reject(new Error(`tool is not available: ${path}`));
      }
      if (argumentsList.length > 1) {
        return Promise.reject(
          new TypeError("tool calls accept at most one argument object"),
        );
      }
      const args = argumentsList.length === 0 ? {} : argumentsList[0];
      if (
        typeof args !== "string" &&
        (args === null || typeof args !== "object" || Array.isArray(args))
      ) {
        return Promise.reject(
          new TypeError("tool call arguments must be a string or object"),
        );
      }

      const id = `tool-${++nextCallId}`;
      return new Promise((resolve, reject) => {
        pendingCalls.set(id, { resolve, reject });
        send({
          jsonrpc: "2.0",
          id,
          method: "tool/call",
          params: { name: path, arguments: args },
        });
      });
    },
  });
}

async function execute(id, params) {
  if (executionStarted) {
    fail(id, "this worker accepts exactly one execution");
    return;
  }
  executionStarted = true;

  if (
    params === null ||
    typeof params !== "object" ||
    typeof params.source !== "string" ||
    !Array.isArray(params.tools) ||
    !params.tools.every(
      (tool) =>
        (typeof tool === "string" && tool.length > 0) ||
        (tool !== null &&
          typeof tool === "object" &&
          typeof tool.name === "string" &&
          tool.name.length > 0 &&
          typeof tool.description === "string"),
    ) ||
    params.stored_values === null ||
    typeof params.stored_values !== "object" ||
    Array.isArray(params.stored_values) ||
    typeof params.image_detail_visible !== "boolean"
  ) {
    fail(id, "invalid exec parameters");
    return;
  }

  const toolMetadata = params.tools.map((tool) =>
    typeof tool === "string"
      ? { name: tool, description: "" }
      : { name: tool.name, description: tool.description },
  );
  const allowedTools = new Set(toolMetadata.map((tool) => tool.name));
  const storedValues = new Map(Object.entries(params.stored_values));
  const imageDetailVisible = params.image_detail_visible;
  const storedValueWrites = Object.create(null);
  const content = [];
  let rejectTimerFailure;
  const timerFailure = new Promise((_resolve, reject) => {
    rejectTimerFailure = reject;
  });
  const appendContent = (value) => {
    send({
      jsonrpc: "2.0",
      method: "content",
      params: { value },
    });
  };
  const stringify = (value) => {
    if (typeof value === "string") return value;
    if (typeof value === "bigint") return String(value);
    if (value === undefined) return "undefined";
    const encoded = JSON.stringify(value);
    return encoded === undefined ? String(value) : encoded;
  };
  const appendText = (value) => {
    appendContent({ type: "text", text: stringify(value) });
  };
  const dataUrl = (value, helperName) => {
    if (
      typeof value !== "string" ||
      value.length === 0 ||
      !value.slice(0, 5).toLowerCase().startsWith("data:")
    ) {
      throw new TypeError(`${helperName} expects a base64 data URL`);
    }
    return value;
  };
  const imageDetail = (value) => {
    if (value === undefined || value === null) return undefined;
    if (typeof value !== "string") {
      throw new TypeError("image detail must be a string when provided");
    }
    const normalized = value.toLowerCase();
    if (!["auto", "low", "high", "original"].includes(normalized)) {
      throw new TypeError(
        "image detail must be one of: auto, low, high, original",
      );
    }
    return normalized;
  };
  const appendImage = (value, detail) => {
    const detailOverride = imageDetailVisible ? imageDetail(detail) : undefined;
    if (typeof value === "string") {
      const item = {
        type: "image",
        image_url: dataUrl(value, "image"),
      };
      if (imageDetailVisible) item.detail = detailOverride ?? "high";
      appendContent(item);
      return;
    }
    if (
      value !== null &&
      typeof value === "object" &&
      typeof value.image_url === "string"
    ) {
      const item = {
        type: "image",
        image_url: dataUrl(value.image_url, "image"),
      };
      if (imageDetailVisible) {
        item.detail = detailOverride ?? imageDetail(value.detail) ?? "high";
      }
      appendContent(item);
      return;
    }
    if (
      value !== null &&
      typeof value === "object" &&
      value.type === "image" &&
      typeof value.data === "string" &&
      value.data.length > 0
    ) {
      const mimeType =
        typeof value.mimeType === "string" && value.mimeType.length > 0
          ? value.mimeType
          : typeof value.mime_type === "string" && value.mime_type.length > 0
            ? value.mime_type
            : "application/octet-stream";
      const metadataDetail =
        value._meta !== null &&
        typeof value._meta === "object" &&
        typeof value._meta["codex/imageDetail"] === "string"
          ? value._meta["codex/imageDetail"]
          : undefined;
      const item = {
        type: "image",
        image_url: value.data.slice(0, 5).toLowerCase() === "data:"
          ? value.data
          : `data:${mimeType};base64,${value.data}`,
      };
      if (imageDetailVisible) {
        item.detail = detailOverride ?? imageDetail(metadataDetail) ?? "high";
      }
      appendContent(item);
      return;
    }
    throw new TypeError(
      "image expects a non-empty image URL string, an object with image_url and optional detail, or a raw MCP image block",
    );
  };
  const drainContent = () => ({
    content: content.splice(0, content.length),
  });
  const storedValue = (key, value) => {
    let encoded;
    try {
      encoded = JSON.stringify(value);
    } catch {
      encoded = undefined;
    }
    if (encoded === undefined) {
      throw new TypeError(
        `Unable to store ${JSON.stringify(
          key,
        )}. Only plain serializable objects can be stored.`,
      );
    }
    return JSON.parse(encoded);
  };
  const exitSignal = Object.freeze({ codeModeExit: true });
  const safeSetTimeout = globalThis.setTimeout.bind(globalThis);
  const safeClearTimeout = globalThis.clearTimeout.bind(globalThis);
  const activeTimeouts = new Map();
  let nextTimeoutId = 0;
  const scheduleTimeout = (callback, delayMs = 0) => {
    const timeoutId = ++nextTimeoutId;
    const nativeTimeout = safeSetTimeout(() => {
      activeTimeouts.delete(timeoutId);
      try {
        Promise.resolve(callback()).catch((error) => {
          rejectTimerFailure(error);
        });
      } catch (error) {
        rejectTimerFailure(error);
      }
    }, delayMs);
    // The worker's protocol pipe, not an unawaited timer, owns cell lifetime.
    nativeTimeout.unref?.();
    activeTimeouts.set(timeoutId, nativeTimeout);
    return timeoutId;
  };
  const cancelTimeout = (timeoutId) => {
    const nativeTimeout = activeTimeouts.get(timeoutId);
    if (nativeTimeout !== undefined) {
      activeTimeouts.delete(timeoutId);
      safeClearTimeout(nativeTimeout);
    }
  };
  const contextGlobal = Object.create(null);
  Object.defineProperties(contextGlobal, {
    tools: {
      value: toolPathProxy("", allowedTools),
      enumerable: true,
      writable: false,
      configurable: false,
    },
    console: { value: undefined, writable: false, configurable: false },
    process: { value: undefined, writable: false, configurable: false },
    global: { value: undefined, writable: false, configurable: false },
    require: { value: undefined, writable: false, configurable: false },
    module: { value: undefined, writable: false, configurable: false },
    Buffer: { value: undefined, writable: false, configurable: false },
    fs: { value: undefined, writable: false, configurable: false },
    net: { value: undefined, writable: false, configurable: false },
    http: { value: undefined, writable: false, configurable: false },
    https: { value: undefined, writable: false, configurable: false },
    child_process: { value: undefined, writable: false, configurable: false },
    fetch: { value: undefined, writable: false, configurable: false },
    WebSocket: { value: undefined, writable: false, configurable: false },
    Atomics: { value: undefined, writable: false, configurable: false },
    SharedArrayBuffer: {
      value: undefined,
      writable: false,
      configurable: false,
    },
    WebAssembly: { value: undefined, writable: false, configurable: false },
    text: {
      value: appendText,
      writable: false,
      configurable: false,
    },
    image: {
      value: appendImage,
      writable: false,
      configurable: false,
    },
    audio: {
      value: (value) => {
        if (typeof value === "string") {
          appendContent({
            type: "audio",
            audio_url: dataUrl(value, "audio"),
          });
          return;
        }
        if (
          value === null ||
          typeof value !== "object" ||
          (typeof value.audio_url !== "string" &&
            !(
              value.type === "audio" &&
              typeof value.data === "string" &&
              value.data.length > 0
            ))
        ) {
          throw new TypeError(
            "audio expects a non-empty audio URL string, an object with audio_url, or a raw MCP audio block",
          );
        }
        const mimeType =
          typeof value.mimeType === "string" && value.mimeType.length > 0
            ? value.mimeType
            : typeof value.mime_type === "string" && value.mime_type.length > 0
              ? value.mime_type
              : "application/octet-stream";
        appendContent({
          type: "audio",
          audio_url:
            typeof value.audio_url === "string"
              ? dataUrl(value.audio_url, "audio")
              : value.data.slice(0, 5).toLowerCase() === "data:"
                ? value.data
                : `data:${mimeType};base64,${value.data}`,
        });
      },
      writable: false,
      configurable: false,
    },
    generatedImage: {
      value: (value) => {
        if (
          value !== null &&
          typeof value === "object" &&
          value.output_hint !== undefined
        ) {
          if (typeof value.output_hint !== "string") {
            throw new TypeError(
              "generatedImage output_hint must be a string when provided",
            );
          }
        }
        appendImage(value);
        if (
          value !== null &&
          typeof value === "object" &&
          value.output_hint !== undefined
        ) {
          appendText(value.output_hint);
        }
      },
      writable: false,
      configurable: false,
    },
    notify: {
      value: (value) => {
        const textValue = stringify(value);
        if (textValue.trim().length === 0) {
          throw new TypeError("notify expects non-empty text");
        }
        send({
          jsonrpc: "2.0",
          method: "notify",
          params: { text: textValue },
        });
      },
      writable: false,
      configurable: false,
    },
    exit: {
      value: () => {
        throw exitSignal;
      },
      writable: false,
      configurable: false,
    },
    ALL_TOOLS: {
      value: Object.freeze(
        toolMetadata.map((tool) =>
          Object.freeze({
            name: tool.name,
            description: tool.description,
          }),
        ),
      ),
      writable: false,
      configurable: false,
    },
    setTimeout: {
      value: scheduleTimeout,
      writable: false,
      configurable: false,
    },
    clearTimeout: {
      value: cancelTimeout,
      writable: false,
      configurable: false,
    },
    setInterval: { value: undefined, writable: false, configurable: false },
    yield_control: {
      value: () => {
        send({
          jsonrpc: "2.0",
          method: "yield",
          params: { value: drainContent() },
        });
      },
      writable: false,
      configurable: false,
    },
    store: {
      value: (key, value) => {
        const normalizedKey = String(key);
        const normalizedValue = storedValue(normalizedKey, value);
        storedValues.set(normalizedKey, normalizedValue);
        storedValueWrites[normalizedKey] = normalizedValue;
      },
      writable: false,
      configurable: false,
    },
    load: {
      value: (key) => {
        const value = storedValues.get(String(key));
        return value === undefined
          ? undefined
          : JSON.parse(JSON.stringify(value));
      },
      writable: false,
      configurable: false,
    },
  });

  const context = vm.createContext(contextGlobal, {
    name: "code-mode-cell",
    codeGeneration: { strings: false, wasm: false },
  });

  try {
    const module = new vm.SourceTextModule(params.source, {
      context,
      identifier: "exec_main.mjs",
    });
    await module.link((specifier) => {
      throw new Error(`module imports are unavailable: ${specifier}`);
    });
    try {
      // Observation yield deadlines are owned by the host. Evaluation itself
      // remains live until it finishes or the host terminates this process,
      // including for CPU-bound cells that cannot service JavaScript timers.
      await Promise.race([module.evaluate(), timerFailure]);
    } catch (error) {
      if (error !== exitSignal) throw error;
    }
    send({
      jsonrpc: "2.0",
      id,
      result: drainContent(),
      stored_value_writes: storedValueWrites,
    });
  } catch (error) {
    send({
      jsonrpc: "2.0",
      id,
      error: {
        code: -32000,
        message: error instanceof Error ? error.message : String(error),
      },
      partial_result: drainContent(),
      stored_value_writes: storedValueWrites,
    });
  } finally {
    for (const nativeTimeout of activeTimeouts.values()) {
      safeClearTimeout(nativeTimeout);
    }
    activeTimeouts.clear();
  }
}

function handleResponse(message) {
  const pending = pendingCalls.get(message.id);
  if (!pending) {
    throw new Error(`unexpected response id: ${message.id}`);
  }
  pendingCalls.delete(message.id);
  if (Object.prototype.hasOwnProperty.call(message, "result")) {
    pending.resolve(message.result);
  } else if (
    message.error &&
    typeof message.error.message === "string"
  ) {
    pending.reject(new Error(message.error.message));
  } else {
    pending.reject(new Error("malformed tool response"));
  }
}

const lines = readline.createInterface({
  input: process.stdin,
  crlfDelay: Infinity,
  terminal: false,
});

lines.on("line", (line) => {
  let message;
  try {
    message = JSON.parse(line);
  } catch {
    process.exitCode = 1;
    lines.close();
    return;
  }

  if (
    message === null ||
    typeof message !== "object" ||
    message.jsonrpc !== "2.0"
  ) {
    process.exitCode = 1;
    lines.close();
    return;
  }

  if (message.method === "exec") {
    void execute(message.id, message.params);
  } else if (
    typeof message.id === "string" &&
    (Object.prototype.hasOwnProperty.call(message, "result") ||
      Object.prototype.hasOwnProperty.call(message, "error"))
  ) {
    try {
      handleResponse(message);
    } catch {
      process.exitCode = 1;
      lines.close();
    }
  } else {
    process.exitCode = 1;
    lines.close();
  }
});

send({ jsonrpc: "2.0", method: "ready" });
