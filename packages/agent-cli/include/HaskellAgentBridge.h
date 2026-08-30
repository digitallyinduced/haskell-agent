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
 * A later call for the same turn replaces the previous batch. Passing zero
 * images discards that turn's batch, which callers should do if they abandon
 * the request. Staging is also discarded when a request envelope or turn.start
 * parameters are rejected, and is consumed by the matching valid turn.start.
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
