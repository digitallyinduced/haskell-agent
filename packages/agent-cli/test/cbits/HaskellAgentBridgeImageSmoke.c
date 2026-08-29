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

/* Compile every session callback signature and exercise synchronous rejects. */
int ha_session_continuity_abi_smoke(void) {
    const uint8_t id[] = "2026-08-30-smoke";
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
    return 0;
}
