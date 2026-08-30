#include "HaskellAgentBridge.h"

#include <stddef.h>

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
    ha_engine_destroy(engine);
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
    if (ha_repository_apply_patch(
            value, sizeof(value) - 1,
            value, sizeof(value) - 1,
            HA_REPOSITORY_STAGE,
            value, sizeof(value) - 1,
            NULL, 0,
            repository_result_callback,
            NULL) != 2) {
        return 24;
    }
    if (ha_repository_commit(
            value, sizeof(value) - 1,
            value, sizeof(value) - 1,
            value, sizeof(value) - 1,
            NULL,
            NULL) != 1) {
        return 25;
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
