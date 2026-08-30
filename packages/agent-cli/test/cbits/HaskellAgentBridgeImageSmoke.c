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

static void interaction_callback(
        void *context,
        const uint8_t *turn_id,
        size_t turn_id_length,
        const uint8_t *interaction_id,
        size_t interaction_id_length,
        int32_t kind,
        const uint8_t *prompt,
        size_t prompt_length,
        const ha_interaction_option *options,
        size_t option_count) {
    (void)context;
    (void)turn_id;
    (void)turn_id_length;
    (void)interaction_id;
    (void)interaction_id_length;
    (void)kind;
    (void)prompt;
    (void)prompt_length;
    (void)options;
    (void)option_count;
}

int ha_interaction_option_abi_smoke(void) {
    if (offsetof(ha_interaction_option, label) != 0
            || offsetof(ha_interaction_option, label_length)
                != sizeof(const uint8_t *)
            || sizeof(ha_interaction_option)
                != sizeof(const uint8_t *) + sizeof(size_t)) {
        return 1;
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

int ha_turn_staging_discard_smoke(void) {
    const uint8_t options_only[] = "options-only";
    const uint8_t images_only[] = "images-only";
    const uint8_t both[] = "both";
    const uint8_t rejected[] = "rejected";
    const uint8_t invalid_utf8[] = {0xff};
    const uint8_t *unreadable_turn_id = (const uint8_t *)(uintptr_t)1;
    const uint8_t mime[] = "image/png";
    const uint8_t bytes[] = {0x89, 0x50, 0x4e, 0x47};
    const ha_image_attachment image = {
        .mime = mime,
        .mime_length = sizeof(mime) - 1,
        .bytes = bytes,
        .bytes_length = sizeof(bytes),
    };
    const uint8_t malformed_start[] =
        "{\"id\":\"request-id\",\"method\":\"turn.start\","
        "\"params\":{\"turnId\":\"rejected\"}}";
    if (ha_runtime_init() != 0) {
        return 20;
    }
    void *engine = ha_engine_create(image_stage_callback, NULL);
    if (engine == NULL) {
        ha_runtime_exit();
        return 21;
    }
    int32_t status = ha_engine_stage_turn_options(
        engine, options_only, sizeof(options_only) - 1,
        HA_INTERACTION_MODE_ASK, HA_SHELL_MODE_BASH);
    if (status == 0) {
        status = ha_engine_discard_turn_staging(
            engine, options_only, sizeof(options_only) - 1);
    }
    if (status == 0) {
        status = ha_engine_stage_turn_images(
            engine, images_only, sizeof(images_only) - 1, &image, 1);
    }
    if (status == 0) {
        status = ha_engine_discard_turn_staging(
            engine, images_only, sizeof(images_only) - 1);
    }
    if (status == 0) {
        status = ha_engine_stage_turn_images(
            engine, both, sizeof(both) - 1, &image, 1);
    }
    if (status == 0) {
        status = ha_engine_stage_turn_options(
            engine, both, sizeof(both) - 1,
            HA_INTERACTION_MODE_PLAN, HA_SHELL_MODE_BOTH);
    }
    if (status == 0) {
        status = ha_engine_discard_turn_staging(
            engine, both, sizeof(both) - 1);
    }
    if (status == 0) {
        status = ha_engine_discard_turn_staging(
            engine, both, sizeof(both) - 1);
    }
    if (status == 0
            && ha_engine_discard_turn_staging(engine, NULL, 1) != 2) {
        status = 22;
    }
    if (status == 0
            && ha_engine_discard_turn_staging(
                engine, invalid_utf8, sizeof(invalid_utf8)) != 2) {
        status = 23;
    }
    if (status == 0
            && ha_engine_discard_turn_staging(engine, both, SIZE_MAX) != 2) {
        status = 24;
    }
    if (status == 0
            && ha_engine_stage_turn_images(
                engine, both, SIZE_MAX, &image, 1) != 2) {
        status = 25;
    }
    if (status == 0
            && ha_engine_stage_turn_options(
                engine, both, SIZE_MAX,
                HA_INTERACTION_MODE_ASK, HA_SHELL_MODE_NONE) != 2) {
        status = 26;
    }
    if (status == 0
            && ha_engine_discard_turn_staging(
                engine, unreadable_turn_id, 1025) != 2) {
        status = 27;
    }
    if (status == 0
            && ha_engine_stage_turn_images(
                engine, unreadable_turn_id, 1025, &image, 1) != 2) {
        status = 28;
    }
    if (status == 0
            && ha_engine_stage_turn_options(
                engine, unreadable_turn_id, 1025,
                HA_INTERACTION_MODE_ASK, HA_SHELL_MODE_NONE) != 2) {
        status = 29;
    }
    if (status == 0) {
        status = ha_engine_stage_turn_images(
            engine, rejected, sizeof(rejected) - 1, &image, 1);
    }
    if (status == 0) {
        status = ha_engine_stage_turn_options(
            engine, rejected, sizeof(rejected) - 1,
            HA_INTERACTION_MODE_YOLO, HA_SHELL_MODE_NONE);
    }
    if (status == 0) {
        status = ha_engine_send_json(
            engine, malformed_start, sizeof(malformed_start) - 1);
    }
    ha_engine_destroy(engine);
    ha_runtime_exit();
    return status;
}

int ha_native_turn_options_stage_smoke(void) {
    const uint8_t turn_id[] = "native-options-turn";
    const uint8_t interaction_id[] = "not-active";
    const int32_t interaction_modes[] = {
        HA_INTERACTION_MODE_ASK,
        HA_INTERACTION_MODE_PLAN,
        HA_INTERACTION_MODE_YOLO
    };
    const int32_t shell_modes[] = {
        HA_SHELL_MODE_NONE,
        HA_SHELL_MODE_BASH,
        HA_SHELL_MODE_GHCI,
        HA_SHELL_MODE_BOTH
    };
    if (ha_runtime_init() != 0) {
        return 10;
    }
    void *engine = ha_engine_create(image_stage_callback, NULL);
    if (engine == NULL) {
        ha_runtime_exit();
        return 11;
    }
    int32_t status = ha_engine_set_interaction_callback(
        engine, interaction_callback, NULL);
    for (size_t mode_index = 0;
            status == 0
                && mode_index
                    < sizeof(interaction_modes) / sizeof(interaction_modes[0]);
            ++mode_index) {
        for (size_t shell_index = 0;
                status == 0
                    && shell_index
                        < sizeof(shell_modes) / sizeof(shell_modes[0]);
                ++shell_index) {
            status = ha_engine_stage_turn_options(
                engine,
                turn_id,
                sizeof(turn_id) - 1,
                interaction_modes[mode_index],
                shell_modes[shell_index]);
        }
    }
    if (status == 0
            && ha_engine_stage_turn_options(
                engine,
                turn_id,
                sizeof(turn_id) - 1,
                99,
                HA_SHELL_MODE_BOTH) != 4) {
        status = 12;
    }
    if (status == 0
            && ha_engine_stage_turn_options(
                engine,
                turn_id,
                sizeof(turn_id) - 1,
                HA_INTERACTION_MODE_ASK,
                99) != 4) {
        status = 14;
    }
    if (status == 0
            && ha_engine_resolve_interaction(
                engine,
                turn_id,
                sizeof(turn_id) - 1,
                interaction_id,
                sizeof(interaction_id) - 1,
                0,
                NULL,
                0) != 4) {
        status = 13;
    }
    if (status == 0) {
        status = ha_engine_set_interaction_callback(engine, NULL, NULL);
    }
    ha_engine_destroy(engine);
    ha_runtime_exit();
    return status;
}
