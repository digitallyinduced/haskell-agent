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
