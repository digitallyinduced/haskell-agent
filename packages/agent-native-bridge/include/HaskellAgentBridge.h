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
     * Kind 1 is reasoning text, 2 text, 3 status, 4 tool-start, 5
     * tool-finish, 6 one provider response's usage, and 7 aggregate user-turn
     * usage. Kind 7 is terminal and, when a turn outcome is available,
     * precedes turn.completed or turn.failed. Usage fields are decimal UTF-8
     * input, output, and cached token counts followed by optional
     * provider-reported USD cost; the cost field is absent when unavailable
     * and is never a local estimate.
     * Kind 4 is also emitted for tool/argument updates; consumers replace
     * the card identified by call ID. Its flags use bit 0 for encrypted
     * arguments, bit 1 for truncation, and bit 2 for an asynchronous call;
     * tool-finish uses bit 1 for truncation and bit 2 for an asynchronous
     * result. Consumers must ignore unknown flag bits for forward
     * compatibility.
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

enum {
    HA_COMPUTER_ABI_VERSION = 2,
    HA_COMPUTER_QUERY_DISPLAY = 1,
    HA_COMPUTER_RUN_AND_OBSERVE = 2
};

enum {
    HA_COMPUTER_ACTION_CLICK = 1,
    HA_COMPUTER_ACTION_DOUBLE_CLICK = 2,
    HA_COMPUTER_ACTION_SCROLL = 3,
    HA_COMPUTER_ACTION_MOVE = 4,
    HA_COMPUTER_ACTION_DRAG = 5,
    HA_COMPUTER_ACTION_TYPE = 6,
    HA_COMPUTER_ACTION_KEYPRESS = 7,
    HA_COMPUTER_ACTION_WAIT = 8
};

enum {
    HA_COMPUTER_BUTTON_NONE = 0,
    HA_COMPUTER_BUTTON_LEFT = 1,
    HA_COMPUTER_BUTTON_RIGHT = 2,
    HA_COMPUTER_BUTTON_MIDDLE = 3,
    HA_COMPUTER_BUTTON_BACK = 4,
    HA_COMPUTER_BUTTON_FORWARD = 5
};

enum {
    HA_COMPUTER_MODIFIER_SHIFT = 1 << 0,
    HA_COMPUTER_MODIFIER_CONTROL = 1 << 1,
    HA_COMPUTER_MODIFIER_OPTION = 1 << 2,
    HA_COMPUTER_MODIFIER_COMMAND = 1 << 3,
    HA_COMPUTER_MODIFIER_FUNCTION = 1 << 4
};

enum {
    HA_COMPUTER_IMAGE_NONE = 0,
    HA_COMPUTER_IMAGE_PNG = 1,
    HA_COMPUTER_IMAGE_JPEG = 2
};

enum {
    HA_COMPUTER_STATUS_SUCCESS = 0,
    HA_COMPUTER_STATUS_INVALID_ARGUMENT = 1,
    HA_COMPUTER_STATUS_UNAVAILABLE = 2,
    HA_COMPUTER_STATUS_TIMEOUT = 3,
    HA_COMPUTER_STATUS_PERMISSION_DENIED = 4,
    HA_COMPUTER_STATUS_UNSUPPORTED = 5,
    HA_COMPUTER_STATUS_FAILED = 6,
    HA_COMPUTER_STATUS_OUTPUT_TOO_LARGE = 7,
    HA_COMPUTER_STATUS_SESSION_LOCKED = 8,
    HA_COMPUTER_STATUS_DISPLAY_CHANGED = 9
};

enum {
    HA_COMPUTER_ACTION_STRUCT_SIZE_V1 = 64,
    HA_COMPUTER_MAX_ACTIONS = 10,
    HA_COMPUTER_MAX_POINTS_PER_ACTION = 1024,
    HA_COMPUTER_MAX_TOTAL_POINTS = 10240,
    HA_COMPUTER_MAX_TEXT_BYTES = 327680,
    HA_COMPUTER_ERROR_CAPACITY = 65536,
    HA_COMPUTER_OUTPUT_CAPACITY = 16777216,
    HA_COMPUTER_ACCESSIBILITY_CAPACITY = 524288
};

typedef struct ha_computer_point_v1 {
    int32_t x;
    int32_t y;
} ha_computer_point_v1;

/*
 * Fixed-width action record for HA_COMPUTER_ABI_VERSION. struct_size must be
 * HA_COMPUTER_ACTION_STRUCT_SIZE_V1. text_offset/text_length select UTF-8 in
 * the callback's text buffer. point_offset/point_count select records in its
 * point buffer. Offsets and lengths are element counts, not byte pointers.
 *
 * Pointer coordinates are integer logical pixels in the final screenshot's
 * top-left coordinate space: 0 <= x < output_width and
 * 0 <= y < output_height. CLICK uses x/y, button, and modifiers.
 * DOUBLE_CLICK and MOVE use x/y and modifiers. SCROLL uses x/y,
 * delta_x/delta_y, and modifiers; positive deltas follow browser-wheel
 * convention and move the viewport right/down. DRAG uses at least two ordered
 * points, including its start and end, and the left button. TYPE uses a text
 * range. KEYPRESS uses a nonempty text range for the final key and modifiers
 * for the chord. WAIT has no fields and waits two seconds. Every field not
 * named for an action must be zero.
 */
typedef struct ha_computer_action_v1 {
    uint32_t struct_size;
    int32_t action;
    int32_t x;
    int32_t y;
    int32_t delta_x;
    int32_t delta_y;
    int32_t button;
    uint32_t modifiers;
    uint64_t text_offset;
    uint64_t text_length;
    uint64_t point_offset;
    uint64_t point_count;
} ha_computer_action_v1;

/*
 * Host computer-control callback for native agent turns.
 *
 * QUERY_DISPLAY requires zero actions/points/text, expected display token and
 * dimensions and requested_image_format zero, and a writable error buffer. On
 * success it writes a nonzero opaque lease to output_display_token and positive
 * logical main-display dimensions to output_width and output_height, sets
 * output_image_format, output_length, and accessibility_output_length to zero,
 * and does not write image or accessibility bytes.
 *
 * RUN_AND_OBSERVE executes the complete ordered action batch, waits for the UI
 * to settle, and captures exactly one final main-display observation. It must
 * verify expected_display_token is the host's current unconsumed lease for the
 * same main display and that the logical display remains expected_width by
 * expected_height before changing input state. A stale lease must fail with
 * HA_COMPUTER_STATUS_DISPLAY_CHANGED before any side effect. The host must
 * serialize leases across every callback context controlling the same desktop
 * and invalidate a lease when a transaction starts changing input state.
 * requested_image_format is PNG or JPEG. On success, output contains encoded
 * image bytes, output_length is their length, accessibility_output contains
 * an optional UTF-8 JSON accessibility snapshot shaped as
 * {"schema_version":1,"scope":<JSON>,"contents":<JSON>}, and
 * accessibility_output_length is its length. scope must identify the focused
 * application/window closely enough that a scope change invalidates a prior
 * tree; contents is the bounded, redacted accessibility tree.
 * output_display_token is a
 * nonzero successor lease distinct from expected_display_token,
 * output_width/output_height are the observed logical image dimensions, and
 * output_image_format is PNG or JPEG. The encoded image is normalized to
 * exactly output_width by output_height pixels, including on Retina displays,
 * so action coordinates map one-to-one to image pixels. Every successful RUN
 * consumes its input lease, including an observation-only run with zero
 * actions.
 *
 * Actions contain no screenshot record: the Haskell runtime removes a final
 * screenshot marker and rejects an earlier one. actions may be NULL only when
 * action_count is zero. points and text follow the same rule. All ranges must
 * be contained in their corresponding buffers. The host must reject unknown
 * versions, operations, action values, modifiers, image formats, nonzero
 * unused fields, malformed UTF-8, and counts above the HA_COMPUTER_MAX_*
 * limits with HA_COMPUTER_STATUS_INVALID_ARGUMENT.
 *
 * output/output_length/accessibility_output_length/output_display_token,
 * output_width/output_height and output_image_format are nonnull
 * callback-scoped writable pointers. accessibility_output may be NULL only
 * when accessibility_output_capacity is zero (as it is for QUERY_DISPLAY).
 * A successful RUN may report an empty accessibility snapshot without
 * failing the screenshot. On
 * failure, set the display token, dimensions, and image format to zero and
 * write a useful UTF-8 error to output when possible. Never report
 * output_length above output_capacity. On failure also set
 * accessibility_output_length to zero. The runtime supplies
 * HA_COMPUTER_ERROR_CAPACITY for QUERY_DISPLAY and HA_COMPUTER_OUTPUT_CAPACITY
 * for RUN_AND_OBSERVE, plus HA_COMPUTER_ACCESSIBILITY_CAPACITY for its
 * accessibility buffer. Never report accessibility_output_length above
 * accessibility_output_capacity.
 *
 * The callback runs synchronously on an agent tool worker, never the setter's
 * caller or AppKit main thread, and must not call engine functions. No input
 * or output buffer remains valid after it returns. The host owns callback and
 * context. ha_engine_set_computer_callback may wait for an in-flight callback;
 * after it returns, a replaced callback/context will not be used again. Keep
 * the installed callback/context valid until replacement, disable, or engine
 * destruction returns.
 */
typedef int32_t (*ha_computer_callback)(
    void *context,
    uint32_t abi_version,
    int32_t operation,
    uint64_t expected_display_token,
    int32_t expected_width,
    int32_t expected_height,
    const ha_computer_action_v1 *actions,
    size_t action_count,
    const ha_computer_point_v1 *points,
    size_t point_count,
    const uint8_t *text,
    size_t text_length,
    int32_t requested_image_format,
    uint8_t *output,
    size_t output_capacity,
    size_t *output_length,
    uint8_t *accessibility_output,
    size_t accessibility_output_capacity,
    size_t *accessibility_output_length,
    uint64_t *output_display_token,
    int32_t *output_width,
    int32_t *output_height,
    int32_t *output_image_format
);

/*
 * Installs native computer control for future turns. A turn replaces the
 * local macOS backend only when its turn.start enables computer use and this
 * callback is nonnull. Passing NULL disables native computer control; context
 * is ignored. Tools retained by an already-started turn fail as inactive.
 *
 * Returns 0 on success, 1 for a null engine, or 2 for an internal failure.
 * Calls must be serialized with ha_engine_destroy. This function may block
 * until a running computer callback returns.
 */
int32_t ha_engine_set_computer_callback(
    void *engine,
    ha_computer_callback callback,
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
 * including archived skills, capped at 1000 rows for compatibility. Status is
 * 0 for an item, 1 for end-of-list, and -1 for an error. UTF-8 buffers are
 * callback-scoped and must be copied. New clients should use the typed,
 * scope-filterable ha_learned_skill_list API below.
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

enum {
    HA_DATA_SCOPE_USER = 0,
    HA_DATA_SCOPE_REPOSITORY = 1,
    HA_DATA_SCOPE_CHECKOUT = 2
};

enum {
    HA_DATA_CATALOG_OBJECT = 0,
    HA_DATA_CATALOG_COLUMN = 1,
    HA_DATA_CATALOG_COMPLETE = 2,
    HA_DATA_CATALOG_ERROR = -1
};

enum {
    HA_DATA_OBJECT_TABLE = 0,
    HA_DATA_OBJECT_VIEW = 1
};

enum {
    HA_DATA_ROWS_VALUE = 0,
    HA_DATA_ROWS_COMPLETE = 1,
    HA_DATA_ROWS_ERROR = -1
};

enum {
    HA_DATA_VALUE_NULL = 0,
    HA_DATA_VALUE_TEXT = 1,
    HA_DATA_VALUE_NUMBER = 2,
    HA_DATA_VALUE_BOOLEAN = 3,
    HA_DATA_VALUE_JSON = 4
};

/*
 * Read-only catalog callbacks first emit an object and then its columns.
 * status is HA_DATA_CATALOG_COMPLETE exactly once on success or
 * HA_DATA_CATALOG_ERROR exactly once on failure. Optional comments have zero
 * length when absent. column_nullable is 0 or 1 for column items.
 *
 * All strings are UTF-8 and callback-scoped; copy them before returning.
 * Callbacks run serially on a runtime worker, never the caller or AppKit main
 * thread. The host owns callback/context and must keep them alive through the
 * terminal callback.
 */
typedef void (*ha_data_catalog_callback)(
    void *context,
    int32_t status,
    int32_t scope,
    int32_t object_kind,
    const uint8_t *object_name, size_t object_name_length,
    const uint8_t *object_comment, size_t object_comment_length,
    const uint8_t *column_name, size_t column_name_length,
    const uint8_t *column_type, size_t column_type_length,
    int32_t column_nullable,
    const uint8_t *column_comment, size_t column_comment_length,
    const uint8_t *error, size_t error_length
);

/*
 * Value callbacks identify a zero-based row and column within one bounded
 * preview. Text is unquoted UTF-8; numbers use JSON number syntax; booleans are
 * "true" or "false"; JSON arrays/objects are encoded as UTF-8 JSON; null has
 * no value bytes. The terminal success item supplies offset, row_count, and
 * has_more. Failure supplies only error.
 *
 * Buffers, threading, ownership, and terminal-callback guarantees match the
 * catalog callback above. The operation is strictly read-only and queries
 * only an object resolved through the selected custom-scope catalog.
 */
typedef void (*ha_data_rows_callback)(
    void *context,
    int32_t status,
    int64_t offset,
    int64_t row_index,
    int32_t column_index,
    int32_t value_kind,
    const uint8_t *value, size_t value_length,
    int64_t row_count,
    int32_t has_more,
    const uint8_t *error, size_t error_length
);

/*
 * Return 0 when accepted, 1 for a null callback, 2 for an invalid pointer/
 * length pair, or 3 for invalid scope, object name, offset, or limit. An
 * accepted request receives exactly one terminal callback. offset must be
 * nonnegative; limit is 1...500.
 */
int32_t ha_data_catalog_list(
    const uint8_t *cwd, size_t cwd_length,
    ha_data_catalog_callback callback, void *context
);

int32_t ha_data_rows_load(
    const uint8_t *cwd, size_t cwd_length,
    int32_t scope,
    const uint8_t *object_name, size_t object_name_length,
    int64_t offset,
    int32_t limit,
    ha_data_rows_callback callback, void *context
);

/*
 * Typed Skills / Memory administration.
 *
 * A learned skill is the durable, versioned memory resource used by future
 * agent sessions. Scope values are 0 user, 1 repository, and 2 checkout.
 * List also accepts -1 for all applicable scopes. Activation values are
 * 0 always, 1 relevant, and 2 manual. "archived" is 0 or 1.
 *
 * Every input buffer is copied before the call returns. UTF-8 must be valid
 * and contain no NUL. All input text pointers must be non-NULL with positive
 * length except applies_when, which may be NULL only when its length is zero.
 * context may be NULL. Callback text pointers may be NULL whenever their
 * associated length is zero and must not be dereferenced in that case;
 * fields omitted on errors/completion use NULL with zero length.
 * Fields enforce the store's character limits: slug 80, title 200,
 * description 1000, applies_when 2000, instructions 30000, and
 * change_summary 1000. Lists are explicitly limited to 1...1000 items.
 * expected_revision is exact, positive optimistic concurrency; it is never
 * interpreted as "latest". Read revision 0 selects the current revision.
 *
 * Functions return 0 when accepted, 1 for a missing callback, and 2 for an
 * invalid pointer, enum, UTF-8 buffer, bound, count, or revision. Accepted
 * calls invoke callbacks on a dedicated Haskell worker thread, never the
 * caller or main thread. Calls for different requests may overlap. Item and
 * error buffers are valid only until that callback returns and must be copied.
 * The callback function and context must remain valid through the terminal
 * callback. Accepted operations deliver their documented terminal callback
 * unless the process exits; there is no cancellation handle.
 *
 * Item/list status is 0 for an item, 1 for successful list completion, -1
 * for invalid/store failure, -2 not found, -3 revision conflict, -4 already
 * exists, and -5 revision not found. A conflict places the current revision
 * in the revision field. A read has one item or one error callback; list and
 * history have zero or more items followed by exactly one terminal callback.
 *
 * Mutation callbacks use status 0 for success and the same negative failures.
 * Their revision is the new revision on success or current revision on
 * conflict. The ABI never accepts credentials or caller-provided provenance
 * and never returns source evidence/session identifiers. Skill content itself
 * is not secret storage and callers must not place credentials in it.
 *
 * cwd selects the same canonical user/repository/checkout identities as the
 * CLI. Slugs are lower-case ASCII words separated by single hyphens. Update
 * is a full content replacement and preserves active/archive state; use the
 * explicit archive and restore operations for state changes.
 */
#define HA_LEARNED_SKILL_SCOPE_ALL        INT32_C(-1)
#define HA_LEARNED_SKILL_SCOPE_USER       INT32_C(0)
#define HA_LEARNED_SKILL_SCOPE_REPOSITORY INT32_C(1)
#define HA_LEARNED_SKILL_SCOPE_CHECKOUT   INT32_C(2)

#define HA_LEARNED_SKILL_ACTIVATION_ALWAYS   INT32_C(0)
#define HA_LEARNED_SKILL_ACTIVATION_RELEVANT INT32_C(1)
#define HA_LEARNED_SKILL_ACTIVATION_MANUAL   INT32_C(2)

#define HA_RESOURCE_STATUS_ITEM               INT32_C(0)
#define HA_RESOURCE_STATUS_COMPLETE           INT32_C(1)
#define HA_RESOURCE_STATUS_ERROR              INT32_C(-1)
#define HA_RESOURCE_STATUS_NOT_FOUND          INT32_C(-2)
#define HA_RESOURCE_STATUS_REVISION_CONFLICT  INT32_C(-3)
#define HA_RESOURCE_STATUS_ALREADY_EXISTS     INT32_C(-4)
#define HA_RESOURCE_STATUS_REVISION_NOT_FOUND INT32_C(-5)

typedef void (*ha_learned_skill_callback)(
    void *context,
    int32_t status,
    int32_t scope,
    const uint8_t *slug, size_t slug_length,
    int64_t revision,
    const uint8_t *title, size_t title_length,
    const uint8_t *description, size_t description_length,
    const uint8_t *applies_when, size_t applies_when_length,
    const uint8_t *instructions, size_t instructions_length,
    int32_t activation,
    int32_t priority,
    int32_t archived,
    int64_t created_at_ms,
    int64_t updated_at_ms,
    const uint8_t *error, size_t error_length
);

typedef void (*ha_learned_skill_revision_callback)(
    void *context,
    int32_t status,
    int64_t revision,
    const uint8_t *title, size_t title_length,
    const uint8_t *description, size_t description_length,
    const uint8_t *applies_when, size_t applies_when_length,
    const uint8_t *instructions, size_t instructions_length,
    int32_t activation,
    int32_t priority,
    int32_t archived,
    const uint8_t *change_summary, size_t change_summary_length,
    int64_t created_at_ms,
    const uint8_t *error, size_t error_length
);

typedef void (*ha_resource_result_callback)(
    void *context,
    int32_t status,
    int64_t revision,
    const uint8_t *error, size_t error_length
);

int32_t ha_learned_skill_list(
    const uint8_t *cwd, size_t cwd_length,
    int32_t scope, size_t limit,
    ha_learned_skill_callback callback, void *context
);
int32_t ha_learned_skill_read(
    const uint8_t *cwd, size_t cwd_length,
    int32_t scope,
    const uint8_t *slug, size_t slug_length,
    uint64_t revision,
    ha_learned_skill_callback callback, void *context
);
int32_t ha_learned_skill_create(
    const uint8_t *cwd, size_t cwd_length,
    int32_t scope,
    const uint8_t *slug, size_t slug_length,
    const uint8_t *title, size_t title_length,
    const uint8_t *description, size_t description_length,
    const uint8_t *applies_when, size_t applies_when_length,
    const uint8_t *instructions, size_t instructions_length,
    int32_t activation,
    int32_t priority,
    const uint8_t *change_summary, size_t change_summary_length,
    ha_resource_result_callback callback, void *context
);
int32_t ha_learned_skill_update(
    const uint8_t *cwd, size_t cwd_length,
    int32_t scope,
    const uint8_t *slug, size_t slug_length,
    uint64_t expected_revision,
    const uint8_t *title, size_t title_length,
    const uint8_t *description, size_t description_length,
    const uint8_t *applies_when, size_t applies_when_length,
    const uint8_t *instructions, size_t instructions_length,
    int32_t activation,
    int32_t priority,
    const uint8_t *change_summary, size_t change_summary_length,
    ha_resource_result_callback callback, void *context
);
int32_t ha_learned_skill_archive(
    const uint8_t *cwd, size_t cwd_length,
    int32_t scope,
    const uint8_t *slug, size_t slug_length,
    uint64_t expected_revision,
    const uint8_t *change_summary, size_t change_summary_length,
    ha_resource_result_callback callback, void *context
);
int32_t ha_learned_skill_restore(
    const uint8_t *cwd, size_t cwd_length,
    int32_t scope,
    const uint8_t *slug, size_t slug_length,
    uint64_t expected_revision,
    const uint8_t *change_summary, size_t change_summary_length,
    ha_resource_result_callback callback, void *context
);
int32_t ha_learned_skill_rollback(
    const uint8_t *cwd, size_t cwd_length,
    int32_t scope,
    const uint8_t *slug, size_t slug_length,
    uint64_t expected_revision,
    uint64_t target_revision,
    const uint8_t *change_summary, size_t change_summary_length,
    ha_resource_result_callback callback, void *context
);
int32_t ha_learned_skill_history(
    const uint8_t *cwd, size_t cwd_length,
    int32_t scope,
    const uint8_t *slug, size_t slug_length,
    size_t limit,
    ha_learned_skill_revision_callback callback, void *context
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

enum {
    HA_INTERACTION_MODE_ASK = 0,
    HA_INTERACTION_MODE_PLAN = 1,
    HA_INTERACTION_MODE_YOLO = 2
};

enum {
    HA_SHELL_MODE_NONE = 0,
    HA_SHELL_MODE_BASH = 1,
    HA_SHELL_MODE_GHCI = 2,
    HA_SHELL_MODE_BOTH = 3
};

enum {
    HA_INTERACTION_PLAN_ENTER = 1,
    HA_INTERACTION_PLAN_EXIT = 2,
    HA_INTERACTION_QUESTION = 3
};

typedef struct ha_interaction_option {
    const uint8_t *label;
    size_t label_length;
} ha_interaction_option;

/*
 * Interactive callbacks run synchronously on the native turn worker thread.
 * turn_id, interaction_id, prompt, the options array, and every option label
 * are borrowed and valid only until the callback returns; copy them before
 * returning and dispatch UI work to the main thread. options is NULL exactly
 * when option_count is zero. kind is one of HA_INTERACTION_* above.
 * PLAN_ENTER's prompt is the reason and its options are enter/stay; PLAN_EXIT's
 * prompt is the plan markdown and its options are approve/request changes/
 * cancel; QUESTION's prompt and options come from ask_user_question.
 *
 * Returning from this callback does not answer it. The turn remains paused
 * until ha_engine_resolve_interaction is called or the turn is cancelled.
 */
typedef void (*ha_interaction_callback)(
    void *context,
    const uint8_t *turn_id, size_t turn_id_length,
    const uint8_t *interaction_id, size_t interaction_id_length,
    int32_t kind,
    const uint8_t *prompt, size_t prompt_length,
    const ha_interaction_option *options, size_t option_count
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
 * a length from 1 through 1024 bytes; violating those requirements returns 2.
 * Invalid UTF-8 returns 4. images may be null only when image_count is zero.
 * Passing zero images discards that turn's batch, which callers should do if
 * they abandon the request. For every image, mime and bytes are required,
 * non-null, and have non-zero lengths; violating those requirements returns
 * 4. All pointers are borrowed for this call and the runtime copies accepted
 * data before return. Both image and execution-option staging are discarded
 * when a request envelope or turn.start parameters are rejected, or when a
 * turn.start is rejected while another turn is active, and are consumed by
 * the matching valid turn.start. Returns 0 when accepted, 1 for a null engine,
 * 2 for a null, empty, or oversized turn ID, 3 for an internal failure, and 4
 * for invalid image fields or UTF-8.
 */
int32_t ha_engine_stage_turn_images(
    void *engine,
    const uint8_t *turn_id,
    size_t turn_id_length,
    const ha_image_attachment *images,
    size_t image_count
);
/*
 * Stage execution and shell modes for one turn. The turn ID is required,
 * non-empty UTF-8, at most 1024 bytes, and is copied before return. A later
 * call for the same turn replaces the previous options; the matching valid
 * turn.start consumes them. Unstaged turns default to ASK + BASH. Returns 0
 * when accepted, 1 for a null engine, 2 for an invalid turn ID, 3 for an
 * internal failure, and 4 for an unknown mode code.
 */
int32_t ha_engine_stage_turn_options(
    void *engine,
    const uint8_t *turn_id,
    size_t turn_id_length,
    int32_t interaction_mode,
    int32_t shell_mode
);
/*
 * Atomically discard both the staged image batch and staged execution options
 * for one turn. The turn ID pointer must be non-NULL, its length must be
 * between 1 and 1024 bytes, and its bytes must be valid non-empty UTF-8; the ID
 * is copied before return. The operation is idempotent and safe while the
 * engine command worker is running, but must be serialized with
 * ha_engine_destroy.
 *
 * Returns 0 after discarding (including when nothing was staged), 1 for a null
 * engine, 2 for an invalid turn ID pointer/length/UTF-8, and 3 for an internal
 * failure.
 */
int32_t ha_engine_discard_turn_staging(
    void *engine,
    const uint8_t *turn_id,
    size_t turn_id_length
);
/*
 * Install or replace the engine's interactive callback. Passing NULL clears
 * it; future interaction requests then use safe decline/cancel defaults.
 * Replacing or clearing waits for an in-flight callback to return, cancels all
 * interactions still awaiting answers, and then makes the old callback/context
 * safe to release. Do not call this function reentrantly from the callback.
 * Engine destruction must still be serialized with all API calls. Returns 0
 * on success, 1 for a null engine, and 3 for an internal failure.
 */
int32_t ha_engine_set_interaction_callback(
    void *engine,
    ha_interaction_callback callback,
    void *context
);
/*
 * Resolve a currently pending interaction. selected_index is a zero-based
 * option index, or -1 for custom text/cancel. custom_text may be NULL only
 * when custom_text_length is zero and is copied before return. For a free-text
 * question use -1 with non-empty custom text; use -1 with empty text to
 * cancel. Plan-exit option 1 uses custom text as change-request notes.
 *
 * Returns 0 when published, 1 for a null engine, 2 for invalid pointers/UTF-8,
 * 3 for an internal failure, and 4 when the interaction is absent, already
 * resolved, or selected_index is out of range.
 */
int32_t ha_engine_resolve_interaction(
    void *engine,
    const uint8_t *turn_id,
    size_t turn_id_length,
    const uint8_t *interaction_id,
    size_t interaction_id_length,
    int32_t selected_index,
    const uint8_t *custom_text,
    size_t custom_text_length
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
