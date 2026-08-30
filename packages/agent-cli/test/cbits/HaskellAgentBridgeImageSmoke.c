#include "HaskellAgentBridge.h"

#include <stddef.h>
#include <stdatomic.h>
#include <unistd.h>

static void restart_result_callback(void *context, int32_t status,
                                    uint64_t revision, const uint8_t *error,
                                    size_t error_length) {
    (void)status;
    (void)revision;
    (void)error;
    (void)error_length;
    atomic_fetch_add((_Atomic int *)context, 1);
}

/*
 * Keep a native compile/run smoke check next to the public ABI. This catches
 * accidental field reordering or type changes before the macOS client stages
 * image buffers through the bridge.
 */
int ha_image_attachment_abi_smoke(void) {
    static const uint8_t first_mime[] = "image/png";
    static const uint8_t first_bytes[] = {0x89, 0x50, 0x4e, 0x47};
    static const uint8_t second_mime[] = "image/jpeg";
    static const uint8_t second_bytes[] = {0xff, 0xd8, 0xff};
    const ha_image_attachment images[] = {
        {
            .mime = first_mime,
            .mime_length = sizeof(first_mime) - 1,
            .bytes = first_bytes,
            .bytes_length = sizeof(first_bytes),
        },
        {
            .mime = second_mime,
            .mime_length = sizeof(second_mime) - 1,
            .bytes = second_bytes,
            .bytes_length = sizeof(second_bytes),
        },
    };

    if (offsetof(ha_image_attachment, mime) != 0
            || offsetof(ha_image_attachment, mime_length)
                != sizeof(const uint8_t *)
            || offsetof(ha_image_attachment, bytes)
                != sizeof(const uint8_t *) + sizeof(size_t)
            || offsetof(ha_image_attachment, bytes_length)
                != 2 * sizeof(const uint8_t *) + sizeof(size_t)) {
        return 1;
    }
    if (images[0].mime_length != 9 || images[1].mime_length != 10
            || images[0].bytes_length != 4 || images[1].bytes_length != 3
            || images[0].mime[0] != 'i' || images[1].mime[6] != 'j') {
        return 2;
    }
    return 0;
}

/*
 * Compile the gateway function/callback signatures as a native consumer
 * and exercise their synchronous argument validation without network or
 * credential-store access.
 */
int ha_gateway_abi_smoke(void) {
    ha_gateway_status_callback status_callback = NULL;
    ha_gateway_connect_start_callback start_callback = NULL;
    ha_gateway_poll_callback poll_callback = NULL;
    ha_gateway_result_callback result_callback = NULL;
    const uint8_t base_url[] = "https://platform.digitallyinduced.com";
    const uint8_t client_name[] = "native-smoke";
    const uint8_t device_code[] = "device-code";
    const uint8_t client_id[] = "haskell-agent-macos";
    const uint8_t authorization_code[] = "authorization-code";
    const uint8_t code_verifier[] =
        "0123456789abcdefghijklmnopqrstuvwxyzABCDEFG";
    const uint8_t redirect_uri[] =
        "haskell-agent-auth://gateway/callback";

    if (HA_GATEWAY_CONNECTED != 0 || HA_GATEWAY_DISCONNECTED != 1
            || HA_GATEWAY_ERROR != -1 || HA_GATEWAY_POLL_AUTHORIZED != 0
            || HA_GATEWAY_POLL_PENDING != 1 || HA_GATEWAY_POLL_SLOW_DOWN != 2
            || HA_GATEWAY_POLL_ERROR != -1) {
        return 20;
    }
    if (ha_gateway_status(status_callback, NULL) != 1) {
        return 21;
    }
    if (ha_gateway_connect_start(
            base_url, sizeof(base_url) - 1,
            client_name, sizeof(client_name) - 1,
            start_callback, NULL) != 1) {
        return 22;
    }
    if (ha_gateway_connect_poll(
            base_url, sizeof(base_url) - 1,
            device_code, sizeof(device_code) - 1,
            poll_callback, NULL) != 1) {
        return 23;
    }
    if (ha_gateway_connect_exchange(
            base_url, sizeof(base_url) - 1,
            client_id, sizeof(client_id) - 1,
            authorization_code, sizeof(authorization_code) - 1,
            code_verifier, sizeof(code_verifier) - 1,
            redirect_uri, sizeof(redirect_uri) - 1,
            result_callback, NULL) != 1) {
        return 24;
    }
    if (ha_gateway_disconnect(result_callback, NULL) != 1) {
        return 25;
    }
    return 0;
}

int ha_mcp_admin_abi_smoke(void) {
    static const uint8_t arg[] = "--stdio";
    static const uint8_t key[] = "TOKEN";
    static const uint8_t secret[] = "not-returned";
    const ha_utf8_slice arguments[] = {
        {.bytes = arg, .length = sizeof(arg) - 1},
    };
    const ha_mcp_env_entry environment[] = {
        {
            .key = {.bytes = key, .length = sizeof(key) - 1},
            .value = {.bytes = secret, .length = sizeof(secret) - 1},
        },
    };
    if (offsetof(ha_utf8_slice, bytes) != 0
            || offsetof(ha_utf8_slice, length) != sizeof(const uint8_t *)
            || sizeof(ha_mcp_env_entry) != 2 * sizeof(ha_utf8_slice)) {
        return 1;
    }
    if (arguments[0].length != 7 || environment[0].key.length != 5
            || environment[0].value.length != 12) {
        return 2;
    }
    return 0;
}

static void learned_skill_callback(
        void *context, int32_t status, int32_t scope,
        const uint8_t *slug, size_t slug_length, int64_t revision,
        const uint8_t *title, size_t title_length,
        const uint8_t *description, size_t description_length,
        const uint8_t *applies_when, size_t applies_when_length,
        const uint8_t *instructions, size_t instructions_length,
        int32_t activation, int32_t priority, int32_t archived,
        int64_t created_at_ms, int64_t updated_at_ms,
        const uint8_t *error, size_t error_length) {
    (void)context; (void)status; (void)scope; (void)slug; (void)slug_length;
    (void)revision; (void)title; (void)title_length; (void)description;
    (void)description_length; (void)applies_when; (void)applies_when_length;
    (void)instructions; (void)instructions_length; (void)activation;
    (void)priority; (void)archived; (void)created_at_ms; (void)updated_at_ms;
    (void)error; (void)error_length;
}

static void learned_skill_revision_callback(
        void *context, int32_t status, int64_t revision,
        const uint8_t *title, size_t title_length,
        const uint8_t *description, size_t description_length,
        const uint8_t *applies_when, size_t applies_when_length,
        const uint8_t *instructions, size_t instructions_length,
        int32_t activation, int32_t priority, int32_t archived,
        const uint8_t *change_summary, size_t change_summary_length,
        int64_t created_at_ms,
        const uint8_t *error, size_t error_length) {
    (void)context; (void)status; (void)revision; (void)title;
    (void)title_length; (void)description; (void)description_length;
    (void)applies_when; (void)applies_when_length; (void)instructions;
    (void)instructions_length; (void)activation; (void)priority;
    (void)archived; (void)change_summary; (void)change_summary_length;
    (void)created_at_ms; (void)error; (void)error_length;
}

static void learned_skill_result_callback(
        void *context, int32_t status, int64_t revision,
        const uint8_t *error, size_t error_length) {
    (void)context; (void)status; (void)revision; (void)error;
    (void)error_length;
}

/* Compile every typed entry point and verify public enum/status values. */
int ha_learned_skill_admin_abi_smoke(void) {
    ha_learned_skill_callback item = learned_skill_callback;
    ha_learned_skill_revision_callback revision =
        learned_skill_revision_callback;
    ha_resource_result_callback result = learned_skill_result_callback;
    int32_t (*list_fn)(const uint8_t *, size_t, int32_t, size_t,
        ha_learned_skill_callback, void *) = ha_learned_skill_list;
    int32_t (*read_fn)(const uint8_t *, size_t, int32_t,
        const uint8_t *, size_t, uint64_t, ha_learned_skill_callback,
        void *) = ha_learned_skill_read;
    (void)item; (void)revision; (void)result; (void)list_fn; (void)read_fn;
    if (HA_LEARNED_SKILL_SCOPE_ALL != -1
            || HA_LEARNED_SKILL_SCOPE_USER != 0
            || HA_LEARNED_SKILL_SCOPE_REPOSITORY != 1
            || HA_LEARNED_SKILL_SCOPE_CHECKOUT != 2
            || HA_LEARNED_SKILL_ACTIVATION_MANUAL != 2
            || HA_RESOURCE_STATUS_REVISION_CONFLICT != -3) {
        return 1;
    }
    return 0;
}

/*
 * Exercise synchronous validation through the actual exported symbols. None
 * of these rejected calls starts PostgreSQL or schedules a callback.
 */
int ha_learned_skill_admin_validation_smoke(void) {
    const uint8_t cwd[] = ".";
    const uint8_t slug[] = "safe-slug";
    const uint8_t text[] = "value";
    const uint8_t invalid_utf8[] = {0xff};

    if (ha_learned_skill_list(cwd, 1, HA_LEARNED_SKILL_SCOPE_ALL, 0,
            learned_skill_callback, NULL) != 2) {
        return 1;
    }
    if (ha_learned_skill_read(cwd, 1, 9, slug, sizeof(slug) - 1, 0,
            learned_skill_callback, NULL) != 2) {
        return 2;
    }
    if (ha_learned_skill_create(cwd, 1, HA_LEARNED_SKILL_SCOPE_USER,
            slug, sizeof(slug) - 1, text, sizeof(text) - 1,
            text, sizeof(text) - 1, NULL, 0, text, sizeof(text) - 1,
            9, 0, text, sizeof(text) - 1,
            learned_skill_result_callback, NULL) != 2) {
        return 3;
    }
    if (ha_learned_skill_update(cwd, 1, HA_LEARNED_SKILL_SCOPE_USER,
            slug, sizeof(slug) - 1, 0, text, sizeof(text) - 1,
            text, sizeof(text) - 1, NULL, 0, text, sizeof(text) - 1,
            HA_LEARNED_SKILL_ACTIVATION_RELEVANT, 0,
            text, sizeof(text) - 1,
            learned_skill_result_callback, NULL) != 2) {
        return 4;
    }
    if (ha_learned_skill_archive(cwd, 1, HA_LEARNED_SKILL_SCOPE_USER,
            invalid_utf8, sizeof(invalid_utf8), 1,
            text, sizeof(text) - 1,
            learned_skill_result_callback, NULL) != 2) {
        return 5;
    }
    if (ha_learned_skill_restore(cwd, 1, HA_LEARNED_SKILL_SCOPE_USER,
            NULL, 0, 1, text, sizeof(text) - 1,
            learned_skill_result_callback, NULL) != 2) {
        return 6;
    }
    if (ha_learned_skill_rollback(cwd, 1, HA_LEARNED_SKILL_SCOPE_USER,
            slug, sizeof(slug) - 1, 2, 0, text, sizeof(text) - 1,
            learned_skill_result_callback, NULL) != 2) {
        return 7;
    }
    if (ha_learned_skill_history(cwd, 1, HA_LEARNED_SKILL_SCOPE_USER,
            slug, sizeof(slug) - 1, 1001,
            learned_skill_revision_callback, NULL) != 2) {
        return 8;
    }
    return 0;
}

static void image_stage_callback(void *context, const uint8_t *bytes,
                                 size_t length) {
    (void)context;
    (void)bytes;
    (void)length;
}

/*
 * Exercise the actual exported bridge entry points as a native caller would:
 * initialize the runtime, create an engine, stage one image, and tear it down.
 * The Haskell entry point copies the buffers before returning, so these
 * stack-backed buffers are intentionally short-lived.
 */
int ha_image_attachment_stage_smoke(void) {
    const uint8_t turn_id[] = "native-smoke-turn";
    const uint8_t first_mime[] = "image/png";
    const uint8_t first_bytes[] = {0x89, 0x50, 0x4e, 0x47};
    const uint8_t second_mime[] = "image/jpeg";
    const uint8_t second_bytes[] = {0xff, 0xd8, 0xff};
    const ha_image_attachment images[] = {
        {
            .mime = first_mime,
            .mime_length = sizeof(first_mime) - 1,
            .bytes = first_bytes,
            .bytes_length = sizeof(first_bytes),
        },
        {
            .mime = second_mime,
            .mime_length = sizeof(second_mime) - 1,
            .bytes = second_bytes,
            .bytes_length = sizeof(second_bytes),
        },
    };
    if (ha_runtime_init() != 0) {
        return 10;
    }
    void *engine = ha_engine_create(image_stage_callback, NULL);
    if (engine == NULL) {
        ha_runtime_exit();
        return 11;
    }
    int32_t status = ha_engine_stage_turn_images(
        engine, turn_id, sizeof(turn_id) - 1, images, 2);
    if (status == 0) {
        status = ha_engine_stage_turn_images(
            engine, turn_id, sizeof(turn_id) - 1, images, 1);
    }
    if (status == 0) {
        status = ha_engine_stage_turn_images(
            engine, turn_id, sizeof(turn_id) - 1, NULL, 0);
    }
    _Atomic int callbacks = 0;
    if (status == 0) {
        const uint8_t missing_name[] =
            "__ha_restart_destroy_smoke_missing__";
        status = ha_engine_mcp_server_restart(
            engine, 0, missing_name, sizeof(missing_name) - 1,
            restart_result_callback, &callbacks);
    }
    ha_engine_destroy(engine);
    if (status == 0 && atomic_load(&callbacks) != 1) {
        status = 13;
    }
    ha_runtime_exit();
    return status;
}

static void repository_snapshot_callback(
        void *context,
        const uint8_t *snapshot_id, size_t snapshot_id_length,
        const uint8_t *root, size_t root_length,
        const uint8_t *head, size_t head_length,
        const uint8_t *index_fingerprint, size_t index_fingerprint_length,
        const uint8_t *worktree_fingerprint,
        size_t worktree_fingerprint_length) {
    (void)context;
    (void)snapshot_id;
    (void)snapshot_id_length;
    (void)root;
    (void)root_length;
    (void)head;
    (void)head_length;
    (void)index_fingerprint;
    (void)index_fingerprint_length;
    (void)worktree_fingerprint;
    (void)worktree_fingerprint_length;
}

static void repository_file_callback(
        void *context,
        const uint8_t *path, size_t path_length,
        const uint8_t *original_path, size_t original_path_length,
        int32_t index_status, int32_t worktree_status) {
    (void)context;
    (void)path;
    (void)path_length;
    (void)original_path;
    (void)original_path_length;
    (void)index_status;
    (void)worktree_status;
}

static void repository_diff_callback(
        void *context, const uint8_t *bytes, size_t length,
        int32_t is_binary) {
    (void)context;
    (void)bytes;
    (void)length;
    (void)is_binary;
}

static void repository_hunk_callback(
        void *context,
        int64_t old_start, int64_t old_count,
        int64_t new_start, int64_t new_count,
        const uint8_t *header, size_t header_length) {
    (void)context;
    (void)old_start;
    (void)old_count;
    (void)new_start;
    (void)new_count;
    (void)header;
    (void)header_length;
}

static void repository_result_callback(
        void *context, int32_t status,
        const uint8_t *snapshot_id, size_t snapshot_id_length,
        const uint8_t *error, size_t error_length) {
    (void)context;
    (void)status;
    (void)snapshot_id;
    (void)snapshot_id_length;
    (void)error;
    (void)error_length;
}

static void repository_delivery_status_callback(
        void *context, int32_t status,
        const uint8_t *snapshot_id, size_t snapshot_id_length,
        const uint8_t *head_oid, size_t head_oid_length,
        const uint8_t *branch, size_t branch_length,
        const uint8_t *remote, size_t remote_length,
        const uint8_t *upstream_ref, size_t upstream_ref_length,
        const uint8_t *upstream_oid, size_t upstream_oid_length,
        int64_t ahead, int64_t behind,
        const uint8_t *error, size_t error_length) {
    (void)context; (void)status;
    (void)snapshot_id; (void)snapshot_id_length;
    (void)head_oid; (void)head_oid_length;
    (void)branch; (void)branch_length;
    (void)remote; (void)remote_length;
    (void)upstream_ref; (void)upstream_ref_length;
    (void)upstream_oid; (void)upstream_oid_length;
    (void)ahead; (void)behind; (void)error; (void)error_length;
}

static void repository_push_preview_callback(
        void *context, int32_t status,
        const uint8_t *confirmation, size_t confirmation_length,
        int64_t expires_at_unix,
        const uint8_t *head_oid, size_t head_oid_length,
        const uint8_t *branch, size_t branch_length,
        const uint8_t *remote, size_t remote_length,
        const uint8_t *upstream_ref, size_t upstream_ref_length,
        int64_t ahead, int64_t behind,
        const uint8_t *error, size_t error_length) {
    (void)context; (void)status;
    (void)confirmation; (void)confirmation_length; (void)expires_at_unix;
    (void)head_oid; (void)head_oid_length;
    (void)branch; (void)branch_length;
    (void)remote; (void)remote_length;
    (void)upstream_ref; (void)upstream_ref_length;
    (void)ahead; (void)behind; (void)error; (void)error_length;
}

static void repository_push_result_callback(
        void *context, int32_t status,
        const uint8_t *snapshot_id, size_t snapshot_id_length,
        const uint8_t *pushed_oid, size_t pushed_oid_length,
        const uint8_t *error, size_t error_length) {
    (void)context; (void)status;
    (void)snapshot_id; (void)snapshot_id_length;
    (void)pushed_oid; (void)pushed_oid_length;
    (void)error; (void)error_length;
}

static void repository_pr_preview_callback(
        void *context, int32_t status,
        const uint8_t *confirmation, size_t confirmation_length,
        int64_t expires_at_unix,
        const uint8_t *repository, size_t repository_length,
        const uint8_t *base_ref, size_t base_ref_length,
        const uint8_t *head_ref, size_t head_ref_length,
        const uint8_t *title, size_t title_length,
        const uint8_t *error, size_t error_length) {
    (void)context; (void)status;
    (void)confirmation; (void)confirmation_length; (void)expires_at_unix;
    (void)repository; (void)repository_length;
    (void)base_ref; (void)base_ref_length;
    (void)head_ref; (void)head_ref_length;
    (void)title; (void)title_length;
    (void)error; (void)error_length;
}

static void repository_pr_result_callback(
        void *context, int32_t status,
        const uint8_t *url, size_t url_length,
        const uint8_t *error, size_t error_length) {
    (void)context; (void)status;
    (void)url; (void)url_length; (void)error; (void)error_length;
}

static void repository_check_output_callback(
        void *context, int32_t stream,
        const uint8_t *bytes, size_t length) {
    (void)context;
    (void)stream;
    (void)bytes;
    (void)length;
}

static void repository_check_exit_callback(
        void *context, int32_t cancelled, int32_t exit_code,
        const uint8_t *error, size_t error_length) {
    (void)context;
    (void)cancelled;
    (void)exit_code;
    (void)error;
    (void)error_length;
}

/*
 * Compile every repository-review callback and function signature and exercise
 * synchronous validation without starting a worker.
 */
int ha_repository_review_abi_smoke(void) {
    const uint8_t value[] = "x";
    if (offsetof(ha_utf8_string, bytes) != 0
            || offsetof(ha_utf8_string, length)
                != sizeof(const uint8_t *)) {
        return 19;
    }
    if (HA_REPOSITORY_STAGE != 0
            || HA_REPOSITORY_UNSTAGE != 1
            || HA_REPOSITORY_RESTORE != 2) {
        return 20;
    }
    if (ha_repository_snapshot(
            NULL, 0,
            repository_snapshot_callback,
            repository_file_callback,
            repository_result_callback,
            NULL) != 2) {
        return 21;
    }
    if (ha_repository_snapshot(
            value, SIZE_MAX,
            repository_snapshot_callback,
            repository_file_callback,
            repository_result_callback,
            NULL) != 2) {
        return 29;
    }
    if (ha_repository_diff(
            value, sizeof(value) - 1,
            value, sizeof(value) - 1,
            HA_REPOSITORY_DIFF_WORKTREE,
            value, sizeof(value) - 1,
            NULL,
            repository_hunk_callback,
            repository_result_callback,
            NULL) != 1) {
        return 22;
    }
    if (ha_repository_apply_path(
            value, sizeof(value) - 1,
            value, sizeof(value) - 1,
            99,
            value, sizeof(value) - 1,
            repository_result_callback,
            NULL) != 2) {
        return 23;
    }
    if (ha_repository_apply_hunks(
            value, sizeof(value) - 1,
            value, sizeof(value) - 1,
            HA_REPOSITORY_STAGE,
            value, sizeof(value) - 1,
            NULL, 0,
            repository_result_callback,
            NULL) != 2) {
        return 24;
    }
    const size_t oversized_hunk = SIZE_MAX;
    if (ha_repository_apply_hunks(
            value, sizeof(value) - 1,
            value, sizeof(value) - 1,
            HA_REPOSITORY_STAGE,
            value, sizeof(value) - 1,
            &oversized_hunk, 1,
            repository_result_callback,
            NULL) != 2) {
        return 30;
    }
    if (ha_repository_apply_hunks(
            value, sizeof(value) - 1,
            value, sizeof(value) - 1,
            HA_REPOSITORY_STAGE,
            value, sizeof(value) - 1,
            &oversized_hunk, 4097,
            repository_result_callback,
            NULL) != 2) {
        return 31;
    }
    if (ha_repository_commit(
            value, sizeof(value) - 1,
            value, sizeof(value) - 1,
            value, sizeof(value) - 1,
            NULL,
            NULL) != 1) {
        return 25;
    }
    if (ha_repository_delivery_status(
            value, sizeof(value) - 1,
            value, sizeof(value) - 1,
            NULL, NULL) != 1
        || ha_repository_push_preview(
            value, sizeof(value) - 1,
            value, sizeof(value) - 1,
            NULL, NULL) != 1
        || ha_repository_push_confirm(
            value, sizeof(value) - 1,
            value, sizeof(value) - 1,
            NULL, NULL) != 1
        || ha_repository_pr_preview(
            value, sizeof(value) - 1,
            value, sizeof(value) - 1,
            value, sizeof(value) - 1,
            value, sizeof(value) - 1,
            value, sizeof(value) - 1,
            NULL, NULL) != 1
        || ha_repository_pr_confirm(
            value, sizeof(value) - 1,
            value, sizeof(value) - 1,
            NULL, NULL) != 1) {
        return 29;
    }
    if (ha_repository_delivery_status(
            NULL, 0,
            value, sizeof(value) - 1,
            repository_delivery_status_callback, NULL) != 2
        || ha_repository_push_preview(
            NULL, 0,
            value, sizeof(value) - 1,
            repository_push_preview_callback, NULL) != 2
        || ha_repository_push_confirm(
            NULL, 0,
            value, sizeof(value) - 1,
            repository_push_result_callback, NULL) != 2
        || ha_repository_pr_preview(
            value, sizeof(value) - 1,
            value, sizeof(value) - 1,
            value, sizeof(value) - 1,
            value, sizeof(value) - 1,
            value, (size_t)1024 * 1024 + 1,
            repository_pr_preview_callback, NULL) != 2
        || ha_repository_pr_confirm(
            NULL, 0,
            value, sizeof(value) - 1,
            repository_pr_result_callback, NULL) != 2) {
        return 30;
    }
    void *check = NULL;
    if (ha_repository_check_start(
            value, sizeof(value) - 1,
            value, sizeof(value) - 1,
            value, sizeof(value) - 1,
            NULL, 0,
            repository_check_output_callback,
            NULL,
            NULL,
            &check) != 1) {
        return 26;
    }
    const ha_utf8_string oversized_argument = {
        value,
        (size_t)1024 * 1024 + 1
    };
    if (ha_repository_check_start(
            value, sizeof(value) - 1,
            value, sizeof(value) - 1,
            value, sizeof(value) - 1,
            &oversized_argument, 1,
            repository_check_output_callback,
            repository_check_exit_callback,
            NULL,
            &check) != 2) {
        return 27;
    }
    if (ha_repository_check_start(
            value, sizeof(value) - 1,
            value, sizeof(value) - 1,
            value, sizeof(value) - 1,
            &oversized_argument, 4097,
            repository_check_output_callback,
            repository_check_exit_callback,
            NULL,
            &check) != 2) {
        return 28;
    }
    ha_repository_check_cancel(NULL);
    ha_repository_check_destroy(NULL);
    ha_repository_cancel_all();
    return 0;
}

static void session_turn_callback(
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
        const uint8_t *error, size_t error_length) {
    (void)context; (void)status; (void)turn_index;
    (void)occurred_at; (void)occurred_at_length;
    (void)user_text; (void)user_text_length;
    (void)assistant_text; (void)assistant_text_length;
    (void)turn_error; (void)turn_error_length;
    (void)response_id; (void)response_id_length;
    (void)transcript_effect; (void)transcript_effect_length;
    (void)response_items_json; (void)response_items_json_length;
    (void)input_tokens; (void)output_tokens; (void)cached_tokens;
    (void)has_older; (void)has_newer; (void)error; (void)error_length;
}

static void session_result_callback(
        void *context, int32_t status,
        const uint8_t *session_id, size_t session_id_length,
        const uint8_t *error, size_t error_length) {
    (void)context; (void)status; (void)session_id; (void)session_id_length;
    (void)error; (void)error_length;
}

static void session_export_callback(
        void *context, int32_t status,
        const uint8_t *bytes, size_t length,
        const uint8_t *error, size_t error_length) {
    (void)context; (void)status; (void)bytes; (void)length;
    (void)error; (void)error_length;
}

static atomic_int invalid_import_result;

static void invalid_import_callback(
        void *context, int32_t status,
        const uint8_t *session_id, size_t session_id_length,
        const uint8_t *error, size_t error_length) {
    (void)context;
    (void)session_id; (void)session_id_length;
    atomic_store_explicit(
        &invalid_import_result,
        status == -1 && error != NULL && error_length > 0 ? 1 : 2,
        memory_order_release);
}

/* Compile every session callback signature and exercise synchronous rejects. */
int ha_session_continuity_abi_smoke(void) {
    const uint8_t id[] = "2026-08-30-smoke";
    const uint8_t invalid_utf8[] = {0xc3, 0x28};
    atomic_init(&invalid_import_result, 0);
    if (ha_session_load_around(
            NULL, 0, 0, 1, session_turn_callback, NULL) != 2) {
        return 1;
    }
    if (ha_session_load_around(
            id, sizeof(id) - 1, -1, 1, session_turn_callback, NULL) != 2) {
        return 2;
    }
    if (ha_session_fork(
            id, sizeof(id) - 1, -1, session_result_callback, NULL) != 2) {
        return 3;
    }
    if (ha_session_export(
            NULL, 0, session_export_callback, NULL) != 2) {
        return 4;
    }
    if (ha_session_import(
            NULL, 0, session_result_callback, NULL) != 2) {
        return 5;
    }
    if (ha_session_load_around(
            invalid_utf8, sizeof(invalid_utf8), 0, 1,
            session_turn_callback, NULL) != 2) {
        return 6;
    }
    if (ha_session_fork(
            invalid_utf8, sizeof(invalid_utf8), 0,
            session_result_callback, NULL) != 2) {
        return 7;
    }
    if (ha_session_export(
            invalid_utf8, sizeof(invalid_utf8),
            session_export_callback, NULL) != 2) {
        return 8;
    }
    if (ha_session_import(
            invalid_utf8, sizeof(invalid_utf8),
            invalid_import_callback, NULL) != 0) {
        return 9;
    }
    for (int attempt = 0; attempt < 5000; ++attempt) {
        int result = atomic_load_explicit(
            &invalid_import_result, memory_order_acquire);
        if (result != 0) {
            return result == 1 ? 0 : 10;
        }
        usleep(1000);
    }
    return 11;
}
