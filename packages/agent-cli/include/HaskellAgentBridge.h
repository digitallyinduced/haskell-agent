#ifndef HASKELL_AGENT_BRIDGE_H
#define HASKELL_AGENT_BRIDGE_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef void (*ha_event_callback)(
    void *context,
    /*
     * Events are normally UTF-8 JSON. Native loop events use a versioned
     * binary HAEV frame:
     *   magic "HAEV" (4), version (u8), kind (u8), flags (u16 BE),
     *   turn-id and kind-specific fields as u32-BE length-prefixed UTF-8.
     * Kind 1 is reasoning text, 2 text, 3 status, 4 tool-start, and 5
     * tool-finish. Tool-start flags use bit 0 for encrypted arguments and
     * bit 1 for truncation; tool-finish uses bit 1 for truncation.
     * The bytes are valid only for the duration of this callback; copy before
     * returning.
     */
    const uint8_t *bytes,
    size_t length
);
enum {
    HA_BROWSER_NAVIGATE = 1,
    HA_BROWSER_SNAPSHOT = 2,
    HA_BROWSER_CLICK = 3,
    HA_BROWSER_TYPE = 4,
    HA_BROWSER_BACK = 5,
    HA_BROWSER_FORWARD = 6,
    HA_BROWSER_RELOAD = 7,
    HA_BROWSER_KEY = 8,
    HA_BROWSER_SCROLL = 9
};

enum {
    HA_BROWSER_TYPE_SUBMIT = 1
};

enum {
    HA_BROWSER_STATUS_SUCCESS = 0,
    HA_BROWSER_STATUS_INVALID_ARGUMENT = 1,
    HA_BROWSER_STATUS_UNAVAILABLE = 2,
    HA_BROWSER_STATUS_TIMEOUT = 3,
    HA_BROWSER_STATUS_PERMISSION_DENIED = 4,
    HA_BROWSER_STATUS_UNSUPPORTED = 5,
    HA_BROWSER_STATUS_FAILED = 6,
    HA_BROWSER_STATUS_OUTPUT_TOO_LARGE = 7
};

enum {
    HA_BROWSER_URL_MAX_BYTES = 8192,
    HA_BROWSER_SELECTOR_MAX_BYTES = 4096,
    HA_BROWSER_TEXT_MAX_BYTES = 65536,
    HA_BROWSER_KEY_MAX_BYTES = 128,
    HA_BROWSER_SCROLL_MAX_ABS_DELTA = 10000,
    HA_BROWSER_OUTPUT_CAPACITY = 262144
};

/*
 * Host-browser callback used by browser tools in native agent turns.
 *
 * Commands use these typed fields:
 *   NAVIGATE: argument1 is an absolute HTTP(S) URL without userinfo, at most
 *             HA_BROWSER_URL_MAX_BYTES UTF-8 bytes.
 *   SNAPSHOT: no arguments.
 *   CLICK: argument1 is a CSS selector, at most
 *          HA_BROWSER_SELECTOR_MAX_BYTES UTF-8 bytes.
 *   TYPE: argument1 is a selector with the same limit as CLICK; argument2 is
 *         at most HA_BROWSER_TEXT_MAX_BYTES UTF-8 bytes; flags bit
 *         HA_BROWSER_TYPE_SUBMIT requests form submission.
 *   KEY: argument1 is a nonempty DOM KeyboardEvent.key value, at most
 *        HA_BROWSER_KEY_MAX_BYTES UTF-8 bytes.
 *   SCROLL: scroll_delta_x and scroll_delta_y are finite CSS-pixel deltas,
 *           each with absolute value at most
 *           HA_BROWSER_SCROLL_MAX_ABS_DELTA; at least one must be nonzero.
 *   BACK, FORWARD, RELOAD: no arguments.
 *
 * Unused text fields have length zero. Unused scroll fields and flags are
 * zero. Unknown commands or nonzero unused fields must be rejected with
 * HA_BROWSER_STATUS_INVALID_ARGUMENT.
 *
 * Input buffers are nonnull, callback-scoped UTF-8 and may have zero length.
 * output is a nonnull, callback-scoped writable buffer. Set *output_length to
 * the UTF-8 bytes written, never above output_capacity. Return 0 for success
 * or a HA_BROWSER_STATUS_* value for failure; on failure, write a useful UTF-8
 * error to output when possible. The runtime supplies
 * HA_BROWSER_OUTPUT_CAPACITY bytes. The callback runs synchronously on an
 * agent tool worker, never the setter's caller or AppKit main thread. It must
 * not call engine functions.
 *
 * The runtime does not retain input/output buffers. The host owns callback
 * and context. ha_engine_set_browser_callback may wait for an in-flight
 * callback; after it returns, a replaced callback/context will not be used
 * again. The currently installed callback/context must remain valid until
 * replaced, disabled, or ha_engine_destroy returns.
 */
typedef int32_t (*ha_browser_callback)(
    void *context,
    int32_t command,
    const uint8_t *argument1, size_t argument1_length,
    const uint8_t *argument2, size_t argument2_length,
    double scroll_delta_x,
    double scroll_delta_y,
    int32_t flags,
    uint8_t *output, size_t output_capacity, size_t *output_length
);

/*
 * Installs browser support for future turns. A turn exposes browser tools only
 * when its turn.start processing observes a nonnull callback. Passing NULL
 * disables browser support; context is ignored in that case. Tools retained
 * by an already-started turn fail as inactive after disabling.
 *
 * Returns 0 on success, 1 for a null engine, or 2 for an internal failure.
 * Calls must be serialized with ha_engine_destroy. This function may block
 * until a running browser callback returns.
 */
int32_t ha_engine_set_browser_callback(
    void *engine,
    ha_browser_callback callback,
    void *context
);

/*
 * A conversation-search callback. status is 0 for a result, 1 for terminal
 * success, and -1 for terminal failure (with error populated). All UTF-8
 * buffers are valid only during the callback and must be copied before it
 * returns. A result has error == NULL. Completion has no result fields.
 *
 * turn_index is -1 for a metadata hit. occurred_at_ms is 0 when absent. role
 * is 0 for metadata, 1 for user, and 2 for assistant. Timestamps are
 * milliseconds since the Unix epoch. Callbacks run serially on the engine
 * worker thread, not the caller or main thread. An accepted request receives
 * exactly one terminal callback unless engine destruction has begun.
 */
typedef void (*ha_conversation_search_callback)(
    void *context,
    int32_t status,
    const uint8_t *session_id, size_t session_id_length,
    const uint8_t *title, size_t title_length,
    const uint8_t *cwd, size_t cwd_length,
    const uint8_t *provider, size_t provider_length,
    const uint8_t *model, size_t model_length,
    int64_t updated_at_ms,
    int32_t archived,
    int64_t turn_index,
    int64_t occurred_at_ms,
    int32_t role,
    const uint8_t *user_text, size_t user_text_length,
    const uint8_t *assistant_text, size_t assistant_text_length,
    double rank,
    const uint8_t *error, size_t error_length
);

/*
 * Session mutation callbacks use status 0 for success and -1 for failure.
 * The error buffer is UTF-8, callback-scoped, and populated only on failure.
 * Callbacks run serially on the engine worker thread. An accepted mutation
 * receives exactly one callback unless engine destruction has begun.
 */
typedef void (*ha_session_result_callback)(
    void *context,
    int32_t status,
    const uint8_t *error,
    size_t error_length
);

/*
 * Learned-skill list callbacks emit current rows from all applicable scopes,
 * including archived skills. Status is 0 for an item, 1 for end-of-list, and
 * -1 for an error. UTF-8 buffers are callback-scoped and must be copied.
 */
typedef void (*ha_learned_skills_list_callback)(
    void *context, int32_t status,
    const uint8_t *scope, size_t scope_length,
    const uint8_t *slug, size_t slug_length,
    int64_t revision,
    const uint8_t *title, size_t title_length,
    const uint8_t *description, size_t description_length,
    const uint8_t *applies_when, size_t applies_when_length,
    const uint8_t *instructions, size_t instructions_length,
    const uint8_t *activation, size_t activation_length,
    const uint8_t *status_text, size_t status_text_length,
    int32_t priority,
    const uint8_t *updated_at, size_t updated_at_length,
    const uint8_t *error, size_t error_length
);

int32_t ha_learned_skills_list(
    const uint8_t *cwd, size_t cwd_length,
    ha_learned_skills_list_callback callback, void *context
);

typedef struct ha_utf8_slice {
    const uint8_t *bytes;
    size_t length;
} ha_utf8_slice;

typedef struct ha_mcp_env_entry {
    ha_utf8_slice key;
    ha_utf8_slice value;
} ha_mcp_env_entry;

/*
 * MCP catalog reads expose redacted typed rows. Environment values are never
 * returned: field callbacks use kind 0 for an argument and kind 1 for an
 * environment key. Fields for a row are emitted before that row. List uses
 * status 0 for rows, 1 for terminal completion, and -1 for terminal failure.
 * Read/status invokes exactly one callback: status 0 for its row or -1 for
 * failure, with no trailing status 1. revision is an opaque optimistic-
 * concurrency token; a conflict reports the current revision. Callback
 * buffers are valid only until that callback returns.
 */
typedef void (*ha_mcp_server_callback)(
    void *context, int32_t status, uint64_t revision,
    const uint8_t *name, size_t name_length,
    int32_t enabled,
    const uint8_t *command, size_t command_length,
    const uint8_t *cwd, size_t cwd_length,
    int32_t startup_timeout_seconds,
    int32_t request_timeout_seconds,
    size_t argument_count,
    size_t environment_key_count,
    const uint8_t *error, size_t error_length
);

typedef void (*ha_mcp_server_field_callback)(
    void *context,
    const uint8_t *name, size_t name_length,
    int32_t kind, size_t index,
    const uint8_t *value, size_t value_length
);

typedef void (*ha_mcp_result_callback)(
    void *context, int32_t status, uint64_t revision,
    const uint8_t *error, size_t error_length
);

int32_t ha_mcp_servers_list(
    ha_mcp_server_callback callback,
    ha_mcp_server_field_callback field_callback,
    void *context
);
/*
 * Status is a side-effect-free catalog status: enabled means configured for
 * the next turn, disabled means intentionally stopped. It does not start a
 * server or probe its process. Like read, it invokes exactly one callback and
 * does not emit a separate completion callback.
 */
int32_t ha_mcp_server_status(
    const uint8_t *name, size_t name_length,
    ha_mcp_server_callback callback,
    ha_mcp_server_field_callback field_callback,
    void *context
);
int32_t ha_mcp_server_read(
    const uint8_t *name, size_t name_length,
    ha_mcp_server_callback callback,
    ha_mcp_server_field_callback field_callback,
    void *context
);

/*
 * Adds and edits copy all inputs before returning. Environment values are
 * write-only secrets. edit preserves enabled state; use the explicit
 * enable/disable operations to change it. expected_revision must come from
 * the most recent list/read/mutation callback. Text fields are limited to
 * 1 MiB each and argument/environment arrays to 4096 entries. These calls
 * return 0 when accepted, 1 for a missing callback, and 2 for invalid or
 * over-limit input.
 */
int32_t ha_mcp_server_add(
    uint64_t expected_revision,
    const uint8_t *name, size_t name_length,
    const uint8_t *command, size_t command_length,
    const ha_utf8_slice *arguments, size_t argument_count,
    const uint8_t *cwd, size_t cwd_length,
    const ha_mcp_env_entry *environment, size_t environment_count,
    int32_t startup_timeout_seconds,
    int32_t request_timeout_seconds,
    ha_mcp_result_callback callback, void *context
);
/*
 * Restart discards the engine's warm MCP fleet after validating name and
 * revision. It is rejected asynchronously if a turn is active; the next turn
 * starts servers from the current catalog. A 0 return accepts the callback:
 * it is delivered exactly once before a concurrent ha_engine_destroy returns,
 * with success or an error (including shutdown cancellation if the worker
 * exits early). Once destruction has won the acceptance race, restart returns
 * 3 synchronously and no callback is delivered.
 */
int32_t ha_engine_mcp_server_restart(
    void *engine,
    uint64_t expected_revision,
    const uint8_t *name, size_t name_length,
    ha_mcp_result_callback callback, void *context
);
int32_t ha_mcp_server_edit(
    uint64_t expected_revision,
    const uint8_t *name, size_t name_length,
    const uint8_t *command, size_t command_length,
    const ha_utf8_slice *arguments, size_t argument_count,
    const uint8_t *cwd, size_t cwd_length,
    const ha_mcp_env_entry *environment, size_t environment_count,
    int32_t startup_timeout_seconds,
    int32_t request_timeout_seconds,
    ha_mcp_result_callback callback, void *context
);
int32_t ha_mcp_server_enable(
    uint64_t expected_revision,
    const uint8_t *name, size_t name_length,
    ha_mcp_result_callback callback, void *context
);
int32_t ha_mcp_server_disable(
    uint64_t expected_revision,
    const uint8_t *name, size_t name_length,
    ha_mcp_result_callback callback, void *context
);
int32_t ha_mcp_server_remove(
    uint64_t expected_revision,
    const uint8_t *name, size_t name_length,
    ha_mcp_result_callback callback, void *context
);

/*
 * Account list callbacks use status 0 for an item, 1 for end-of-list, and
 * -1 for an error. Item strings include disabled managed credentials and
 * externally discovered accounts.
 */
typedef void (*ha_account_list_callback)(
    void *context,
    int32_t status,
    const uint8_t *provider, size_t provider_length,
    const uint8_t *billing, size_t billing_length,
    const uint8_t *selection_id, size_t selection_id_length,
    const uint8_t *account_id, size_t account_id_length,
    const uint8_t *label, size_t label_length,
    const uint8_t *detail, size_t detail_length,
    const uint8_t *managed_id, size_t managed_id_length,
    const uint8_t *source, size_t source_length,
    int32_t enabled,
    int32_t can_manage,
    const uint8_t *error, size_t error_length
);

/*
 * Usage-window callbacks are emitted after their corresponding account item.
 * reset_at_unix is an absolute UTC Unix timestamp in seconds.
 */
typedef void (*ha_account_usage_window_callback)(
    void *context,
    const uint8_t *selection_id, size_t selection_id_length,
    const uint8_t *name, size_t name_length,
    int32_t used_percent,
    int64_t window_seconds,
    int64_t reset_at_unix
);

/*
 * Result callbacks use status 0 for success, 1 when OAuth polling remains
 * pending, and -1 for an error.
 */
typedef void (*ha_account_result_callback)(
    void *context,
    int32_t status,
    const uint8_t *account_id,
    size_t account_id_length,
    const uint8_t *error,
    size_t error_length
);

/* OAuth start callbacks use status 0 for a challenge and -1 for an error. */
typedef void (*ha_account_oauth_start_callback)(
    void *context,
    int32_t status,
    const uint8_t *verification_url,
    size_t verification_url_length,
    const uint8_t *user_code,
    size_t user_code_length,
    const uint8_t *device_auth_id,
    size_t device_auth_id_length,
    const uint8_t *device_code,
    size_t device_code_length,
    int32_t poll_interval_seconds,
    int32_t expires_in_seconds,
    const uint8_t *error,
    size_t error_length
);

/*
 * Gateway operations are process-global and do not require an engine.
 * Accepted operations run asynchronously on a Haskell worker thread; that
 * thread is not the caller or the AppKit main thread. Every string is UTF-8
 * and valid only for the duration of its callback, so callers must copy it
 * before returning. The caller owns callback/context and must keep both valid
 * until the callback returns. No gateway access token or WebSocket credential
 * crosses this ABI.
 */
enum {
    HA_GATEWAY_CONNECTED = 0,
    HA_GATEWAY_DISCONNECTED = 1,
    HA_GATEWAY_ERROR = -1
};

typedef void (*ha_gateway_status_callback)(
    void *context,
    int32_t status,
    const uint8_t *base_url,
    size_t base_url_length,
    const uint8_t *error,
    size_t error_length
);

/* Start status is HA_GATEWAY_CONNECTED (0) for a challenge, or -1 on error. */
typedef void (*ha_gateway_connect_start_callback)(
    void *context,
    int32_t status,
    const uint8_t *user_code,
    size_t user_code_length,
    const uint8_t *verification_uri,
    size_t verification_uri_length,
    const uint8_t *verification_uri_complete,
    size_t verification_uri_complete_length,
    const uint8_t *device_code,
    size_t device_code_length,
    int32_t poll_interval_seconds,
    int32_t expires_in_seconds,
    const uint8_t *error,
    size_t error_length
);

enum {
    HA_GATEWAY_POLL_AUTHORIZED = 0,
    HA_GATEWAY_POLL_PENDING = 1,
    HA_GATEWAY_POLL_SLOW_DOWN = 2,
    HA_GATEWAY_POLL_ERROR = -1
};

/*
 * An authorized poll has already persisted the credential inside the trusted
 * runtime. retry_interval_seconds is zero unless the server supplied a new
 * interval for pending/slow-down.
 */
typedef void (*ha_gateway_poll_callback)(
    void *context,
    int32_t status,
    int32_t retry_interval_seconds,
    const uint8_t *error,
    size_t error_length
);

typedef void (*ha_gateway_result_callback)(
    void *context,
    int32_t status,
    const uint8_t *error,
    size_t error_length
);
/*
 * An image submitted for a native turn. The runtime copies both buffers
 * before this call returns; the caller retains ownership of them.
 */
typedef struct ha_image_attachment {
    const uint8_t *mime;
    size_t mime_length;
    const uint8_t *bytes;
    size_t bytes_length;
} ha_image_attachment;

/*
 * Session-page callbacks use status 0 for a turn, 1 for terminal success, and
 * -1 for terminal failure. has_older/has_newer are meaningful only on terminal
 * success. Missing optional strings have length zero. Usage values are all -1
 * when a turn has no provider usage. response_items_json is the JSON array
 * from version 1 of the full-fidelity session-transfer format; it remains JSON
 * because provider response items are deliberately extensible. Every buffer is
 * callback-scoped and must be copied before returning. Callbacks run serially
 * on a background worker, never the caller or main thread.
 */
typedef void (*ha_session_turn_callback)(
    void *context, int32_t status, int64_t turn_index,
    const uint8_t *occurred_at, size_t occurred_at_length,
    const uint8_t *user_text, size_t user_text_length,
    const uint8_t *assistant_text, size_t assistant_text_length,
    const uint8_t *turn_error, size_t turn_error_length,
    const uint8_t *response_id, size_t response_id_length,
    const uint8_t *transcript_effect, size_t transcript_effect_length,
    const uint8_t *response_items_json, size_t response_items_json_length,
    int64_t input_tokens, int64_t output_tokens, int64_t cached_tokens,
    int32_t has_older, int32_t has_newer,
    const uint8_t *error, size_t error_length
);

/* Session transfer results use status 0 for success and -1 for failure. */
typedef void (*ha_session_transfer_result_callback)(
    void *context, int32_t status,
    const uint8_t *session_id, size_t session_id_length,
    const uint8_t *error, size_t error_length
);

/*
 * Export callbacks use status 0 for a document chunk, 1 for terminal success,
 * and -1 for terminal failure. Chunks concatenate to one version 1
 * haskell-agent.session-transfer JSON document. Buffers are callback-scoped.
 */
typedef void (*ha_session_export_callback)(
    void *context, int32_t status,
    const uint8_t *bytes, size_t length,
    const uint8_t *error, size_t error_length
);

/* Runtime calls are process-global and reference counted. */
int32_t ha_runtime_init(void);
void ha_runtime_exit(void);

/*
 * Engine calls must be serialized with ha_engine_destroy. Requests are copied
 * before ha_engine_send_json returns. Send status is 0 when accepted, 1 for a
 * null engine, 2 for null non-empty bytes, 3 for an internal failure, and 4
 * for an invalid request envelope.
 */
void *ha_engine_create(ha_event_callback callback, void *context);
/*
 * Stage an ordered image batch for a turn before its turn.start request.
 * A later call for the same turn replaces the previous batch. turn_id is
 * required, must be non-null even when turn_id_length is zero, and must have
 * a non-zero length; a null or empty turn ID returns 2. Invalid UTF-8 returns
 * 4. images may be null only when image_count is zero. Passing zero images
 * discards that turn's batch, which callers should do if they abandon the
 * request. For every image, mime and bytes are required, non-null, and have
 * non-zero lengths; violating those requirements returns 4. All pointers are
 * borrowed for this call and the runtime copies accepted data before return.
 * Staging is also discarded when a request envelope or turn.start parameters
 * are rejected, and is consumed by the matching valid turn.start. Returns 0
 * when accepted, 1 for a null engine, 2 for a null or empty turn ID, 3 for an
 * internal failure, and 4 for invalid image fields or UTF-8.
 */
int32_t ha_engine_stage_turn_images(
    void *engine,
    const uint8_t *turn_id,
    size_t turn_id_length,
    const ha_image_attachment *images,
    size_t image_count
);
int32_t ha_engine_send_json(
    void *engine,
    const uint8_t *bytes,
    size_t length
);
/*
 * Search active and archived (but not deleted) conversations. The query is
 * copied before return and limit is clamped to 1...100. Returns 0 when
 * accepted, 1 for a null engine, 2 for invalid query/callback, and 3 for an
 * internal failure. Calls must be serialized with ha_engine_destroy.
 */
int32_t ha_engine_search_conversations(
    void *engine,
    const uint8_t *query,
    size_t query_length,
    size_t limit,
    ha_conversation_search_callback callback,
    void *context
);
/*
 * Session mutation inputs are copied before return. Returns 0 when accepted,
 * 1 for a null engine, 2 for an invalid callback/input pointer, and 3 for an
 * internal failure. Calls must be serialized with ha_engine_destroy.
 */
int32_t ha_engine_session_rename(
    void *engine,
    const uint8_t *session_id,
    size_t session_id_length,
    const uint8_t *title,
    size_t title_length,
    ha_session_result_callback callback,
    void *context
);
int32_t ha_engine_session_delete(
    void *engine,
    const uint8_t *session_id,
    size_t session_id_length,
    ha_session_result_callback callback,
    void *context
);
int32_t ha_engine_session_archive(
    void *engine,
    const uint8_t *session_id,
    size_t session_id_length,
    int32_t archived,
    ha_session_result_callback callback,
    void *context
);
void ha_engine_destroy(void *engine);

/*
 * Typed session operations copy all inputs before returning and invoke exactly
 * one terminal callback after returning 0. A return of 2 rejects a null/empty
 * required input, invalid UTF-8 session id, invalid index/radius, null
 * callback, or import larger than 512 MiB; no callback follows a rejected
 * call. Malformed import documents, including invalid UTF-8, report callback
 * failure after acceptance. Radius is clamped to 500.
 *
 * Fork copies through the inclusive durable turn index into a fresh session;
 * the source is immutable. Import always remaps the source id to a fresh id.
 */
int32_t ha_session_load_around(
    const uint8_t *session_id, size_t session_id_length,
    int64_t center_turn_index, int32_t radius,
    ha_session_turn_callback callback, void *context
);
int32_t ha_session_fork(
    const uint8_t *session_id, size_t session_id_length,
    int64_t through_turn_index,
    ha_session_transfer_result_callback callback, void *context
);
int32_t ha_session_export(
    const uint8_t *session_id, size_t session_id_length,
    ha_session_export_callback callback, void *context
);
int32_t ha_session_import(
    const uint8_t *bytes, size_t length,
    ha_session_transfer_result_callback callback, void *context
);

/*
 * Account operations use typed callbacks. Callback buffers are valid only
 * during the callback and must be copied by the caller. All functions return
 * 0 when accepted, or a nonzero error before starting the worker.
 */
int32_t ha_accounts_list(
    ha_account_list_callback callback,
    ha_account_usage_window_callback usage_callback,
    void *context
);
int32_t ha_account_oauth_start(
    const uint8_t *provider,
    size_t provider_length,
    ha_account_oauth_start_callback callback,
    void *context
);
int32_t ha_account_oauth_poll(
    const uint8_t *provider,
    size_t provider_length,
    const uint8_t *verification_url,
    size_t verification_url_length,
    const uint8_t *user_code,
    size_t user_code_length,
    const uint8_t *device_auth_id,
    size_t device_auth_id_length,
    const uint8_t *device_code,
    size_t device_code_length,
    int32_t poll_interval_seconds,
    int32_t expires_in_seconds,
    ha_account_result_callback callback,
    void *context
);

/*
 * Gateway inputs are copied before return. Immediate return is 0 when the
 * worker was accepted, 1 for a null callback, or 2 for a null pointer paired
 * with a nonzero length. Semantic and network failures arrive asynchronously.
 */
int32_t ha_gateway_status(
    ha_gateway_status_callback callback,
    void *context
);
int32_t ha_gateway_connect_start(
    const uint8_t *base_url,
    size_t base_url_length,
    const uint8_t *client_name,
    size_t client_name_length,
    ha_gateway_connect_start_callback callback,
    void *context
);
int32_t ha_gateway_connect_poll(
    const uint8_t *base_url,
    size_t base_url_length,
    const uint8_t *device_code,
    size_t device_code_length,
    ha_gateway_poll_callback callback,
    void *context
);
/*
 * Exchanges a browser authorization code using its PKCE verifier. On success
 * the trusted runtime has validated the returned Bearer credential and
 * gateway origins, then atomically persisted it; no secret is returned.
 * The result callback receives status 0 for success or -1 with an error.
 */
int32_t ha_gateway_connect_exchange(
    const uint8_t *base_url,
    size_t base_url_length,
    const uint8_t *client_id,
    size_t client_id_length,
    const uint8_t *authorization_code,
    size_t authorization_code_length,
    const uint8_t *code_verifier,
    size_t code_verifier_length,
    const uint8_t *redirect_uri,
    size_t redirect_uri_length,
    ha_gateway_result_callback callback,
    void *context
);
int32_t ha_gateway_disconnect(
    ha_gateway_result_callback callback,
    void *context
);
int32_t ha_account_api_key_connect(
    const uint8_t *provider,
    size_t provider_length,
    const uint8_t *api_key,
    size_t api_key_length,
    ha_account_result_callback callback,
    void *context
);
int32_t ha_account_set_enabled(
    const uint8_t *managed_id,
    size_t managed_id_length,
    int32_t enabled,
    ha_account_result_callback callback,
    void *context
);
int32_t ha_account_delete(
    const uint8_t *managed_id,
    size_t managed_id_length,
    ha_account_result_callback callback,
    void *context
);

#ifdef __cplusplus
}
#endif

#endif
