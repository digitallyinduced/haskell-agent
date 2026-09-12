// Evaluated before user code. Returns a private factory, never installed globally.
(function (nativeSend, parameters, executionId, firstCallId, nowMilliseconds) {
  "use strict";
  const stringifyJSON = JSON.stringify;
  const parseJSON = JSON.parse;
  const StringValue = String;
  const ErrorValue = Error;
  const TypeErrorValue = TypeError;
  const EvalErrorValue = EvalError;
  const ProxyValue = Proxy;
  const NumberValue = Number;
  const isFiniteNumber = Number.isFinite;
  const minimum = Math.min;
  const maximum = Math.max;
  const PromiseValue = Promise;
  const apply = Reflect.apply;
  const promiseThen = Promise.prototype.then;
  const resolvePromise = Promise.resolve.bind(Promise);
  const mapGet = Function.prototype.call.bind(Map.prototype.get);
  const mapSet = Function.prototype.call.bind(Map.prototype.set);
  const mapDelete = Function.prototype.call.bind(Map.prototype.delete);
  const mapForEach = Function.prototype.call.bind(Map.prototype.forEach);
  const regexTest = Function.prototype.call.bind(RegExp.prototype.test);
  const lowerCase = Function.prototype.call.bind(String.prototype.toLowerCase);
  const trim = Function.prototype.call.bind(String.prototype.trim);
  const includes = Function.prototype.call.bind(Array.prototype.includes);
  const hasOwn = Function.prototype.call.bind(Object.prototype.hasOwnProperty);
  const arrayIsArray = Array.isArray;
  const setPrototype = Object.setPrototypeOf;
  const defineProperty = Object.defineProperty;
  const keys = Object.keys;
  const clean = value => setPrototype(value, null);
  const send = message => {
    clean(message);
    if (message.params) clean(message.params);
    if (message.error) clean(message.error);
    nativeSend(stringifyJSON(message));
  };
  const then = (promise, yes, no) => apply(promiseThen, promise, [yes, no]);
  const pending = new Map();
  const timers = new Map();
  const values = new Map(Object.entries(parameters.stored_values));
  const writes = Object.create(null);
  const available = new Set(parameters.tools.map(tool => typeof tool === "string" ? tool : tool.name));
  const setHas = Function.prototype.call.bind(Set.prototype.has);
  const exitSignal = Object.freeze({});
  let nextCallId = firstCallId;
  let nextTimerId = 0;
  let pendingCount = 0;
  let timerCount = 0;
  let finished = false;
  const output = value => send({jsonrpc: "2.0", method: "content", params: {value: clean(value)}});
  const printable = value => {
    if (typeof value === "string") return value;
    if (value === undefined) return "undefined";
    if (typeof value === "bigint") return StringValue(value);
    const encoded = stringifyJSON(value);
    return encoded === undefined ? StringValue(value) : encoded;
  };
  const text = value => output({type: "text", text: printable(value)});
  const empty = () => clean({content: clean([])});
  const finish = error => {
    if (finished) return;
    finished = true;
    const response = clean({jsonrpc: "2.0", id: executionId, stored_value_writes: writes});
    if (error === undefined || error === exitSignal) response.result = empty();
    else {
      let message;
      try { message = StringValue(error instanceof ErrorValue ? error.message : error); }
      catch { message = "Unable to format JavaScript exception"; }
      response.error = {code: -32000, message};
      response.partial_result = empty();
    }
    send(response);
  };
  const reject = error => new PromiseValue((_resolve, reject) => reject(error));
  const toolProxy = path => {
    const callable = () => undefined;
    setPrototype(callable, null);
    return new ProxyValue(callable, {
      get(_target, name) {
        if (typeof name !== "string" || name === "then") return undefined;
        return toolProxy(path ? `${path}.${name}` : name);
      },
      apply(_target, _this, args) {
        if (!setHas(available, path)) return reject(new ErrorValue(`tool is not available: ${path}`));
        if (args.length > 1) return reject(new TypeErrorValue("tool calls accept at most one argument object"));
        const argument = args.length ? args[0] : {};
        if (typeof argument !== "string" && (argument === null || typeof argument !== "object" || arrayIsArray(argument))) {
          return reject(new TypeErrorValue("tool call arguments must be a string or object"));
        }
        if (pendingCount >= 4096) return reject(new ErrorValue("pending tool call limit exceeded"));
        const id = `tool-${++nextCallId}`;
        return new PromiseValue((resolve, reject) => {
          mapSet(pending, id, {resolve, reject});
          pendingCount++;
          try { send({jsonrpc: "2.0", id, method: "tool/call", params: {name: path, arguments: argument}}); }
          catch (error) { mapDelete(pending, id); pendingCount--; reject(error); }
        });
      },
    });
  };
  const dataUrl = (value, helper) => {
    if (typeof value !== "string" || !regexTest(/^data:/i, value)) throw new TypeErrorValue(`${helper} expects a base64 data URL`);
    return value;
  };
  const detailValue = value => {
    if (value == null) return undefined;
    if (typeof value !== "string") throw new TypeErrorValue("image detail must be a string when provided");
    const normalized = lowerCase(value);
    if (!includes(["auto", "low", "high", "original"], normalized)) throw new TypeErrorValue("image detail must be one of: auto, low, high, original");
    return normalized;
  };
  const media = (value, kind, detail) => {
    const field = `${kind}_url`;
    let url;
    let metadata;
    if (typeof value === "string") url = dataUrl(value, kind);
    else if (value && typeof value === "object" && typeof value[field] === "string") {
      url = dataUrl(value[field], kind);
      metadata = value.detail;
    } else if (value && typeof value === "object" && value.type === kind && typeof value.data === "string" && value.data.length) {
      const mime = typeof value.mimeType === "string" && value.mimeType.length ? value.mimeType
        : typeof value.mime_type === "string" && value.mime_type.length ? value.mime_type : "application/octet-stream";
      url = regexTest(/^data:/i, value.data) ? value.data : `data:${mime};base64,${value.data}`;
      metadata = value._meta?.["codex/imageDetail"];
    } else throw new TypeErrorValue(`${kind} expects a non-empty data URL or raw MCP ${kind} block`);
    const item = clean({type: kind, [field]: url});
    if (kind === "image" && parameters.image_detail_visible) item.detail = detailValue(detail) ?? detailValue(metadata) ?? "high";
    output(item);
  };
  const protectStored = value => {
    if (value && typeof value === "object") {
      clean(value);
      const names = keys(value);
      for (let i = 0; i < names.length; i++) protectStored(value[names[i]]);
    }
    return value;
  };
  mapForEach(values, value => protectStored(value));
  const clone = value => parseJSON(stringifyJSON(value));
  const helpers = {
    tools: toolProxy(""),
    ALL_TOOLS: Object.freeze(parameters.tools.map(tool => Object.freeze(typeof tool === "string" ? {name: tool, description: ""} : {name: tool.name, description: tool.description}))),
    text,
    image: (value, detail) => media(value, "image", detail),
    audio: value => media(value, "audio"),
    generatedImage: value => {
      if (value && typeof value === "object" && value.output_hint !== undefined && typeof value.output_hint !== "string") throw new TypeErrorValue("generatedImage output_hint must be a string when provided");
      media(value, "image");
      if (value && typeof value === "object" && value.output_hint !== undefined) text(value.output_hint);
    },
    notify: value => {
      const text = printable(value);
      if (!trim(text)) throw new TypeErrorValue("notify expects non-empty text");
      send({jsonrpc: "2.0", method: "notify", params: {text}});
    },
    exit: () => { throw exitSignal; },
    yield_control: () => send({jsonrpc: "2.0", method: "yield", params: {value: empty()}}),
    store: (key, value) => {
      const normalized = StringValue(key);
      let encoded;
      try { encoded = stringifyJSON(value); } catch {}
      if (encoded === undefined) throw new TypeErrorValue(`Unable to store ${stringifyJSON(normalized)}. Only plain serializable objects can be stored.`);
      const copy = protectStored(parseJSON(encoded));
      mapSet(values, normalized, copy);
      defineProperty(writes, normalized, {value: copy, writable: true, configurable: true, enumerable: true});
    },
    load: key => {
      const value = mapGet(values, StringValue(key));
      return value === undefined ? undefined : clone(value);
    },
    setTimeout: (callback, delay = 0) => {
      if (typeof callback !== "function") throw new TypeErrorValue("setTimeout callback must be a function");
      if (timerCount >= 4096) throw new ErrorValue("timer limit exceeded");
      const id = ++nextTimerId;
      const numeric = NumberValue(delay);
      mapSet(timers, id, {callback, due: nowMilliseconds() + (isFiniteNumber(numeric) ? maximum(0, minimum(numeric, 2147483647)) : 0)});
      timerCount++;
      return id;
    },
    clearTimeout: id => { if (mapDelete(timers, id)) timerCount--; },
  };
  for (const name of ["console", "process", "global", "require", "module", "Buffer", "fs", "net", "http", "https", "child_process", "fetch", "WebSocket", "Atomics", "SharedArrayBuffer", "WebAssembly", "setInterval"]) helpers[name] = undefined;
  for (const [name, value] of Object.entries(helpers)) Object.defineProperty(globalThis, name, {value, writable: false, configurable: false});
  // Capture every code-generating intrinsic before severing all constructor paths.
  const constructors = [Function, (async function(){}).constructor, (function*(){}).constructor, (async function*(){}).constructor];
  const denied = function () { throw new EvalErrorValue("Code generation from strings disallowed for this context"); };
  for (const constructor of constructors) Object.defineProperty(constructor.prototype, "constructor", {value: denied, writable: false, configurable: false});
  Object.defineProperty(globalThis, "Function", {value: denied, writable: false, configurable: false});
  Object.defineProperty(globalThis, "eval", {value: denied, writable: false, configurable: false});
  // Driver methods are retained only by native protected references.
  return {
    observe(promise) {
      try { then(promise, () => finish(), error => finish(error === undefined ? new ErrorValue("undefined") : error)); }
      catch (error) { finish(error === undefined ? new ErrorValue("undefined") : error); }
    },
    fail(message) { finish(new ErrorValue(message)); },
    // null means busy exec; strings are validated unknown response IDs.
    responseRaw(raw) {
      let message;
      try { message = parseJSON(raw); } catch { return false; }
      if (message === null || typeof message !== "object" || arrayIsArray(message) ||
          !hasOwn(message, "jsonrpc") || message.jsonrpc !== "2.0") return false;
      if (hasOwn(message, "method") && message.method === "exec") return null;
      if (!hasOwn(message, "id") || typeof message.id !== "string" ||
          (!hasOwn(message, "result") && !hasOwn(message, "error"))) return false;
      const entry = mapGet(pending, message.id);
      if (!entry) return message.id;
      mapDelete(pending, message.id);
      pendingCount--;
      if (hasOwn(message, "result")) entry.resolve(message.result);
      else {
        const error = message.error;
        const detail = error !== null && typeof error === "object" && hasOwn(error, "message")
          ? error.message : undefined;
        entry.reject(new ErrorValue(typeof detail === "string" ? detail : "malformed tool response"));
      }
      return true;
    },
    tick(now) {
      if (timerCount === 0 || finished) return;
      mapForEach(timers, (timer, id) => {
        if (!finished && timer.due <= now) {
          mapDelete(timers, id); timerCount--;
          const failed = error => finish(error === undefined ? new ErrorValue("undefined") : error);
          try { then(resolvePromise(apply(timer.callback, undefined, [])), undefined, failed); } catch (error) { failed(error); }
        }
      });
    },
    state() {
      if (!finished) return null;
      const retired = clean([]);
      if (finished) mapForEach(pending, (_entry, id) => { retired[retired.length] = id; });
      return clean({finished, nextCallId, retired});
    },
  };
})
