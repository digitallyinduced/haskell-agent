#include "HaskellAgentBridge.h"

#include <stddef.h>

static void search_callback(
    void *context,
    int32_t status,
    const uint8_t *session_id, size_t session_id_length,
    const uint8_t *title, size_t title_length,
    const uint8_t *cwd, size_t cwd_length,
    const uint8_t *provider, size_t provider_length,
    const uint8_t *model, size_t model_length,
    int64_t updated_at_ms, int32_t archived, int64_t turn_index,
    int64_t occurred_at_ms, int32_t role,
    const uint8_t *user_text, size_t user_text_length,
    const uint8_t *assistant_text, size_t assistant_text_length,
    double rank,
    const uint8_t *error, size_t error_length
) {
    (void)context; (void)status; (void)session_id; (void)session_id_length;
    (void)title; (void)title_length; (void)cwd; (void)cwd_length;
    (void)provider; (void)provider_length; (void)model; (void)model_length;
    (void)updated_at_ms; (void)archived; (void)turn_index;
    (void)occurred_at_ms; (void)role; (void)user_text;
    (void)user_text_length; (void)assistant_text;
    (void)assistant_text_length; (void)rank; (void)error; (void)error_length;
}

/*
 * Keep a native compile/run smoke check next to the public ABI. This catches
 * accidental field reordering or type changes before the macOS client stages
 * image buffers through the bridge.
 */
int ha_image_attachment_abi_smoke(void) {
    ha_conversation_search_callback typed_search_callback = search_callback;
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

    if (typed_search_callback == NULL) {
        return 3;
    }
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
    const uint8_t mime[] = "image/png";
    const uint8_t bytes[] = {0x89, 0x50, 0x4e, 0x47};
    const ha_image_attachment image = {
        .mime = mime,
        .mime_length = sizeof(mime) - 1,
        .bytes = bytes,
        .bytes_length = sizeof(bytes),
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
        engine, turn_id, sizeof(turn_id) - 1, &image, 1);
    ha_engine_destroy(engine);
    ha_runtime_exit();
    return status;
}
