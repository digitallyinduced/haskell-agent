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
void ha_engine_destroy(void *engine);

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
