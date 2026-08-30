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
