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
     * Tool-start flags use bit 0 for encrypted arguments and bit 1 for
     * truncation; tool-finish uses bit 1 for truncation.
     * The bytes are valid only for the duration of this callback; copy before
     * returning.
     */
    const uint8_t *bytes,
    size_t length
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
 * An image submitted for a native turn. The runtime copies both buffers
 * before this call returns; the caller retains ownership of them.
 */
typedef struct ha_image_attachment {
    const uint8_t *mime;
    size_t mime_length;
    const uint8_t *bytes;
    size_t bytes_length;
} ha_image_attachment;

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
 * A later call for the same turn replaces the previous batch. Passing zero
 * images discards that turn's batch, which callers should do if they abandon
 * the request. Both image and execution-option staging are discarded when
 * turn.start parameters are rejected or a turn.start is rejected while
 * another turn is active, and are consumed by the matching valid turn.start.
 * Returns 0 when accepted, 1 for a null engine, 2 for an invalid turn ID, 3
 * for an internal failure, and 4 for an invalid image array or UTF-8 MIME.
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
 * non-empty UTF-8 and is copied before return. A later call for the same turn
 * replaces the previous options; the matching valid turn.start consumes them.
 * Unstaged turns default to ASK + BASH. Returns 0 when accepted, 1 for a null
 * engine, 2 for an invalid turn ID, 3 for an internal failure, and 4 for an
 * unknown mode code.
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
 * non-zero, and its bytes must be valid non-empty UTF-8; the ID is copied
 * before return. The operation is idempotent and safe while the engine command
 * worker is running, but must be serialized with ha_engine_destroy.
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
