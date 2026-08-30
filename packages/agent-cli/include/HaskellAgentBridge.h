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
     * Turn IDs are stable task IDs for the lifetime of an engine. Events for
     * one task are delivered in order, but callbacks for different tasks may
     * run concurrently on runtime worker threads. The callback must be
     * thread-safe and return promptly. The bytes are valid only for the
     * duration of this callback; copy before returning.
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

typedef void (*ha_repository_check_output_callback)(
    void *context,
    int32_t stream,
    const uint8_t *bytes,
    size_t length
);

typedef void (*ha_repository_check_exit_callback)(
    void *context,
    int32_t cancelled,
    int32_t exit_code,
    const uint8_t *error,
    size_t error_length
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

/* A borrowed UTF-8 slice used for typed arrays such as process argv. */
typedef struct ha_utf8_string {
    const uint8_t *bytes;
    size_t length;
} ha_utf8_string;

/*
 * Repository review operations run asynchronously on runtime worker threads.
 * Every accepted operation invokes its result callback exactly once. Callback
 * buffers are borrowed only for the callback duration and must be copied.
 *
 * A successful snapshot first invokes snapshot_callback once, then
 * file_callback once per changed file, and finally result_callback. HEAD is
 * empty for an unborn branch. original_path is NULL with length zero unless
 * Git reports a rename/copy. File status values are unsigned Git porcelain
 * status bytes.
 *
 * A successful diff invokes diff_callback with ordered patch chunks of at
 * most 64 KiB, then hunk_callback for each parsed hunk, then result_callback.
 * is_binary applies to the complete file diff. An empty patch emits no chunks.
 *
 * result_callback status is 0 for success, -1 for failure, -2 when the
 * supplied snapshot is stale, and -3 when cancel_all cancels an accepted
 * operation. Success and stale results populate snapshot_id;
 * failures populate error. Callbacks for one operation are serialized.
 */
typedef void (*ha_repository_snapshot_callback)(
    void *context,
    const uint8_t *snapshot_id, size_t snapshot_id_length,
    const uint8_t *root, size_t root_length,
    const uint8_t *head, size_t head_length,
    const uint8_t *index_fingerprint, size_t index_fingerprint_length,
    const uint8_t *worktree_fingerprint, size_t worktree_fingerprint_length
);

typedef void (*ha_repository_file_callback)(
    void *context,
    const uint8_t *path, size_t path_length,
    const uint8_t *original_path, size_t original_path_length,
    int32_t index_status,
    int32_t worktree_status
);

typedef void (*ha_repository_diff_callback)(
    void *context,
    const uint8_t *bytes,
    size_t length,
    int32_t is_binary
);

typedef void (*ha_repository_hunk_callback)(
    void *context,
    int64_t old_start,
    int64_t old_count,
    int64_t new_start,
    int64_t new_count,
    const uint8_t *header,
    size_t header_length
);

typedef void (*ha_repository_result_callback)(
    void *context,
    int32_t status,
    const uint8_t *snapshot_id,
    size_t snapshot_id_length,
    const uint8_t *error,
    size_t error_length
);

/*
 * Active-task snapshots emit status 0 for each task, status 1 exactly once on
 * completion, and status -1 for failure. state is 0 for queued or 1 for
 * running. session_id is NULL/zero until a new task creates its session; all
 * other item pointers are non-NULL. Error is populated only for status -1.
 * Buffers are callback-scoped UTF-8 and must be copied before returning.
 * Callbacks run serially on the engine command worker, not the caller thread.
 */
typedef void (*ha_task_snapshot_callback)(
    void *context,
    int32_t status,
    const uint8_t *task_id,
    size_t task_id_length,
    const uint8_t *session_id,
    size_t session_id_length,
    int32_t state,
    const uint8_t *error,
    size_t error_length
);

/*
 * Delivery APIs never invoke a shell and never return credentials, command
 * output, or authenticated remote URLs. Preview confirmations are random,
 * one-shot, in-memory tokens bound to the canonical repository, exact
 * snapshot/HEAD, upstream configuration, and remote OID. They expire after
 * ten minutes. Confirm rechecks that state immediately before mutation.
 * Push targets the reviewed commit OID and uses an exact
 * --force-with-lease=<destination>:<expected> server-side CAS only after
 * proving expected is an ancestor of that OID. This cannot approve a history
 * rewrite and never deletes a ref. Repository hooks are disabled for preview
 * and confirmation.
 *
 * Status is 0 for success, -1 for failure, -2 for stale state, -3 for
 * cancellation, and -4 for an invalid/expired/already-used confirmation.
 * Every buffer is callback-scoped UTF-8. An accepted call emits exactly one
 * callback. Delivery callbacks use the repository worker lifecycle and have
 * the same cancellation/reentrancy rules documented below.
 */
typedef void (*ha_repository_delivery_status_callback)(
    void *context, int32_t status,
    const uint8_t *snapshot_id, size_t snapshot_id_length,
    const uint8_t *head_oid, size_t head_oid_length,
    const uint8_t *branch, size_t branch_length,
    const uint8_t *remote, size_t remote_length,
    const uint8_t *upstream_ref, size_t upstream_ref_length,
    const uint8_t *upstream_oid, size_t upstream_oid_length,
    int64_t ahead, int64_t behind,
    const uint8_t *error, size_t error_length
);

typedef void (*ha_repository_push_preview_callback)(
    void *context, int32_t status,
    const uint8_t *confirmation, size_t confirmation_length,
    int64_t expires_at_unix,
    const uint8_t *head_oid, size_t head_oid_length,
    const uint8_t *branch, size_t branch_length,
    const uint8_t *remote, size_t remote_length,
    const uint8_t *upstream_ref, size_t upstream_ref_length,
    int64_t ahead, int64_t behind,
    const uint8_t *error, size_t error_length
);

typedef void (*ha_repository_push_result_callback)(
    void *context, int32_t status,
    const uint8_t *snapshot_id, size_t snapshot_id_length,
    const uint8_t *pushed_oid, size_t pushed_oid_length,
    const uint8_t *error, size_t error_length
);

typedef void (*ha_repository_pr_preview_callback)(
    void *context, int32_t status,
    const uint8_t *confirmation, size_t confirmation_length,
    int64_t expires_at_unix,
    const uint8_t *repository, size_t repository_length,
    const uint8_t *base_ref, size_t base_ref_length,
    const uint8_t *head_ref, size_t head_ref_length,
    const uint8_t *title, size_t title_length,
    const uint8_t *error, size_t error_length
);

typedef void (*ha_repository_pr_result_callback)(
    void *context, int32_t status,
    const uint8_t *url, size_t url_length,
    const uint8_t *error, size_t error_length
);

enum ha_repository_operation {
    HA_REPOSITORY_STAGE = 0,
    HA_REPOSITORY_UNSTAGE = 1,
    HA_REPOSITORY_RESTORE = 2
};

enum ha_repository_diff_kind {
    HA_REPOSITORY_DIFF_WORKTREE = 0,
    HA_REPOSITORY_DIFF_STAGED = 1
};

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
 * Queue cancellation for a copied, non-empty UTF-8 task ID. Return 0 means
 * the command was accepted, not that the task necessarily still exists.
 * Terminal task outcome continues through ha_event_callback. Returns 1 for a
 * null engine, 2 for an invalid task ID, and 3 for an internal failure.
 */
int32_t ha_engine_cancel_task(
    void *engine,
    const uint8_t *task_id,
    size_t task_id_length
);
/*
 * List currently queued and running tasks. Returns 0 when accepted, 1 for a
 * null engine, 2 for a null callback, and 3 for an internal failure. An
 * accepted request receives exactly one terminal callback unless destruction
 * has begun. Calls must be serialized with ha_engine_destroy.
 */
int32_t ha_engine_list_tasks(
    void *engine,
    ha_task_snapshot_callback callback,
    void *context
);
/*
 * Set the engine-wide cross-session concurrency limit for future scheduling.
 * Existing tasks are not cancelled when lowering it. Valid limits are 1...32;
 * the default is 3. Returns 0 when accepted, 1 for a null engine, 2 for an
 * invalid limit, and 3 for an internal failure.
 */
int32_t ha_engine_set_task_limit(void *engine, size_t limit);
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
 * Repository input text is required, non-null, non-empty UTF-8 and is copied
 * before these functions return. Hunk indices identify the immutable hunk
 * ordering returned by ha_repository_diff for the supplied snapshot and file;
 * arbitrary patch bytes never cross the ABI. Callers must keep callback
 * context valid until its terminal result callback. Each input text is capped
 * at 8 MiB, combined text per request at 16 MiB, and hunk_count at 4096;
 * oversized values are rejected before pointer arithmetic or allocation.
 *
 * Return values are 0 when accepted, 1 for a null required callback, 2 for an
 * invalid pointer/length/UTF-8/operation, and 3 for an internal start failure.
 * All reads and mutations compare the supplied snapshot fingerprint while
 * holding canonical per-repository in-process and OS advisory locks in the
 * common Git directory. Cooperating runtime processes therefore serialize.
 * Stale operations do not modify the repository.
 * File paths must be normalized, literal, repository-relative paths; pathspec
 * magic, globs, absolute paths, and traversal are rejected, and the path must
 * exactly match a changed-file path in that snapshot. Hunk mutations rebuild
 * the patch server-side and reject binary, deletion, and rename diffs.
 * Restore never deletes an untracked file. Unrelated Git clients do not honor
 * the advisory lock, so the fingerprint is revalidated immediately before
 * Git's single transactional apply or commit command; Git's own index/ref
 * locks and patch context validation close index mutation races. A
 * non-cooperating process that directly edits worktree files remains outside
 * any advisory-lock guarantee.
 *
 * ha_repository_cancel_all cancels and joins accepted snapshot, diff, and
 * mutation operations before returning. Checks are owned and cancelled by
 * their explicit handles. Cancellation delivers exactly one -3 terminal
 * result unless the operation's terminal callback already completed. No
 * callback starts after cancel_all returns, so contexts may then be released.
 * New repository operations are rejected while cancel_all owns its admission
 * barrier and may be retried after it returns.
 * A streaming callback failure stops that stream and is followed by one
 * failure terminal attempt; terminal callbacks are emitted separately from
 * action execution and are never retried, including when success emission
 * throws.
 *
 * Repository callbacks must return promptly and must not call
 * ha_repository_cancel_all reentrantly. A reentrant call is detected and is a
 * no-op; call cancellation from another thread after the callback returns.
 */
int32_t ha_repository_snapshot(
    const uint8_t *path,
    size_t path_length,
    ha_repository_snapshot_callback snapshot_callback,
    ha_repository_file_callback file_callback,
    ha_repository_result_callback result_callback,
    void *context
);

int32_t ha_repository_diff(
    const uint8_t *path,
    size_t path_length,
    const uint8_t *snapshot_id,
    size_t snapshot_id_length,
    int32_t diff_kind,
    const uint8_t *file_path,
    size_t file_path_length,
    ha_repository_diff_callback diff_callback,
    ha_repository_hunk_callback hunk_callback,
    ha_repository_result_callback result_callback,
    void *context
);

int32_t ha_repository_apply_path(
    const uint8_t *path,
    size_t path_length,
    const uint8_t *snapshot_id,
    size_t snapshot_id_length,
    int32_t operation,
    const uint8_t *file_path,
    size_t file_path_length,
    ha_repository_result_callback result_callback,
    void *context
);

int32_t ha_repository_apply_hunks(
    const uint8_t *path,
    size_t path_length,
    const uint8_t *snapshot_id,
    size_t snapshot_id_length,
    int32_t operation,
    const uint8_t *file_path,
    size_t file_path_length,
    const size_t *hunk_indices,
    size_t hunk_count,
    ha_repository_result_callback result_callback,
    void *context
);

int32_t ha_repository_commit(
    const uint8_t *path,
    size_t path_length,
    const uint8_t *snapshot_id,
    size_t snapshot_id_length,
    const uint8_t *message,
    size_t message_length,
    ha_repository_result_callback result_callback,
    void *context
);

/*
 * Delivery input buffers are required non-empty UTF-8 and copied before the
 * call returns. PR base/title/body are bounded to a 1 KiB base, 512-character
 * title, and 1 MiB body. Return codes are 0 accepted, 1 null callback, 2
 * invalid input, and 3 internal start failure.
 */
int32_t ha_repository_delivery_status(
    const uint8_t *path, size_t path_length,
    const uint8_t *snapshot_id, size_t snapshot_id_length,
    ha_repository_delivery_status_callback callback, void *context
);
int32_t ha_repository_push_preview(
    const uint8_t *path, size_t path_length,
    const uint8_t *snapshot_id, size_t snapshot_id_length,
    ha_repository_push_preview_callback callback, void *context
);
int32_t ha_repository_push_confirm(
    const uint8_t *path, size_t path_length,
    const uint8_t *confirmation, size_t confirmation_length,
    ha_repository_push_result_callback callback, void *context
);
int32_t ha_repository_pr_preview(
    const uint8_t *path, size_t path_length,
    const uint8_t *snapshot_id, size_t snapshot_id_length,
    const uint8_t *base, size_t base_length,
    const uint8_t *title, size_t title_length,
    const uint8_t *body, size_t body_length,
    ha_repository_pr_preview_callback callback, void *context
);
int32_t ha_repository_pr_confirm(
    const uint8_t *path, size_t path_length,
    const uint8_t *confirmation, size_t confirmation_length,
    ha_repository_pr_result_callback callback, void *context
);

void ha_repository_cancel_all(void);

/*
 * Start an argv-based repository check without a shell. executable and every
 * argument are required non-empty UTF-8; arguments may be NULL only when
 * argument_count is zero. At most 4096 arguments, 1 MiB per argument, and
 * 8 MiB total argument bytes are accepted. stream is 1 for stdout and 2 for
 * stderr. Output
 * buffers are callback-scoped and ordered within each stream; the two streams
 * may interleave. Output callback failures are not retried and do not prevent
 * pipe draining. exit_callback is invoked exactly once with the process exit
 * code, or -1 plus an error if launch fails. Its failure is not retried.
 *
 * On status 0, out_check receives an owned opaque handle. Cancel targets the
 * check's process group (including descendants) and joins teardown before
 * returning.
 * The handle is stored before callbacks can begin; callbacks may begin before
 * ha_repository_check_start itself returns and must return promptly.
 * Destroy waits for readers/process completion, frees the
 * handle, and must be called exactly once; no callbacks occur after it
 * returns. Cancel uses a short termination-escalation grace period; destroy
 * also assumes callbacks return promptly. A check callback must not call
 * ha_repository_check_destroy for its own handle; schedule destruction on a
 * different thread after the callback returns. Reentrant destroy is detected
 * and is a no-op, so the owner must still destroy the handle later. A callback
 * reentrant check_cancel is also a no-op. Status values match the other
 * repository functions.
 */
int32_t ha_repository_check_start(
    const uint8_t *path,
    size_t path_length,
    const uint8_t *snapshot_id,
    size_t snapshot_id_length,
    const uint8_t *executable,
    size_t executable_length,
    const ha_utf8_string *arguments,
    size_t argument_count,
    ha_repository_check_output_callback output_callback,
    ha_repository_check_exit_callback exit_callback,
    void *context,
    void **out_check
);

void ha_repository_check_cancel(void *check);
void ha_repository_check_destroy(void *check);

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
