#include "HaskellAgentBridge.h"

#include <string.h>

static void event_callback(
    void *context,
    const uint8_t *bytes,
    size_t length
) {
    (void)context;
    (void)bytes;
    (void)length;
}

struct completion_capture {
    int called;
    int32_t status;
    uint8_t text[64];
    size_t text_length;
};

static void capture_completion(
    void *context,
    const ha_browser_result *result
) {
    struct completion_capture *capture =
        (struct completion_capture *)context;
    capture->called += 1;
    if (result == NULL
            || result->struct_size != HA_BROWSER_RESULT_STRUCT_SIZE
            || result->text_length > sizeof(capture->text)) {
        capture->status = HA_BROWSER_STATUS_INVALID_ARGUMENT;
        return;
    }
    capture->status = result->status;
    capture->text_length = result->text_length;
    if (result->text_length > 0) {
        memcpy(capture->text, result->text, result->text_length);
    }
}

static int32_t browser_callback(
    void *context,
    const ha_browser_request *request,
    ha_browser_completion completion,
    void *completion_context
) {
    const char *text = "native browser \xE2\x9C\x93";
    (void)context;
    if (request == NULL || completion == NULL
            || request->struct_size != HA_BROWSER_REQUEST_STRUCT_SIZE
            || request->scope_id_length != 5
            || memcmp(request->scope_id, "scope", 5) != 0
            || request->call_id_length != 4
            || memcmp(request->call_id, "call", 4) != 0) {
        return HA_BROWSER_STATUS_INVALID_ARGUMENT;
    }
    switch (request->command) {
        case HA_BROWSER_TYPE:
            if (request->argument1_length != 9
                    || memcmp(request->argument1, "ref-query", 9) != 0
                    || request->argument2_length != 7
                    || memcmp(request->argument2, "haskell", 7) != 0
                    || request->scroll_delta_x != 0
                    || request->scroll_delta_y != 0
                    || request->flags != HA_BROWSER_TYPE_SUBMIT) {
                return HA_BROWSER_STATUS_INVALID_ARGUMENT;
            }
            break;
        case HA_BROWSER_SCROLL:
            if (request->argument1_length != 0
                    || request->argument2_length != 0
                    || request->scroll_delta_x != 12.5
                    || request->scroll_delta_y != -800
                    || request->flags != 0) {
                return HA_BROWSER_STATUS_INVALID_ARGUMENT;
            }
            break;
        case HA_BROWSER_SCREENSHOT:
            break;
        default:
            return HA_BROWSER_STATUS_INVALID_ARGUMENT;
    }
    ha_browser_result result = {
        .struct_size = HA_BROWSER_RESULT_STRUCT_SIZE,
        .status = HA_BROWSER_STATUS_SUCCESS,
        .image_format = HA_BROWSER_IMAGE_NONE,
        .image_width = 0,
        .image_height = 0,
        .reserved = 0,
        .text = (const uint8_t *)text,
        .text_length = strlen(text),
        .image = NULL,
        .image_length = 0
    };
    completion(completion_context, &result);
    return HA_BROWSER_STATUS_SUCCESS;
}

static void browser_cancel_callback(
    void *context,
    const uint8_t *scope_id,
    size_t scope_id_length,
    const uint8_t *call_id,
    size_t call_id_length
) {
    (void)context;
    (void)scope_id;
    (void)scope_id_length;
    (void)call_id;
    (void)call_id_length;
}

static int exercise_callback(int32_t command) {
    struct completion_capture capture = {0};
    ha_browser_request request = {
        .struct_size = HA_BROWSER_REQUEST_STRUCT_SIZE,
        .command = command,
        .flags = command == HA_BROWSER_TYPE ? HA_BROWSER_TYPE_SUBMIT : 0,
        .reserved = 0,
        .scroll_delta_x = command == HA_BROWSER_SCROLL ? 12.5 : 0,
        .scroll_delta_y = command == HA_BROWSER_SCROLL ? -800 : 0,
        .scope_id = (const uint8_t *)"scope",
        .scope_id_length = 5,
        .call_id = (const uint8_t *)"call",
        .call_id_length = 4,
        .argument1 = command == HA_BROWSER_TYPE
            ? (const uint8_t *)"ref-query" : NULL,
        .argument1_length = command == HA_BROWSER_TYPE ? 9 : 0,
        .argument2 = command == HA_BROWSER_TYPE
            ? (const uint8_t *)"haskell" : NULL,
        .argument2_length = command == HA_BROWSER_TYPE ? 7 : 0
    };
    int32_t status = browser_callback(
        NULL, &request, capture_completion, &capture);
    return status == HA_BROWSER_STATUS_SUCCESS
        && capture.called == 1
        && capture.status == HA_BROWSER_STATUS_SUCCESS
        && capture.text_length == 18
        && memcmp(capture.text, "native browser \xE2\x9C\x93", 18) == 0;
}

int ha_browser_callback_abi_smoke(void) {
    if (sizeof(ha_browser_request) != HA_BROWSER_REQUEST_STRUCT_SIZE
            || sizeof(ha_browser_result) != HA_BROWSER_RESULT_STRUCT_SIZE
            || HA_BROWSER_NAVIGATE != 1 || HA_BROWSER_SNAPSHOT != 2
            || HA_BROWSER_CLICK != 3 || HA_BROWSER_TYPE != 4
            || HA_BROWSER_BACK != 5 || HA_BROWSER_FORWARD != 6
            || HA_BROWSER_RELOAD != 7 || HA_BROWSER_KEY != 8
            || HA_BROWSER_SCROLL != 9 || HA_BROWSER_SCREENSHOT != 10
            || HA_BROWSER_LIST_TABS != 11 || HA_BROWSER_SWITCH_TAB != 12
            || HA_BROWSER_LIST_DOWNLOADS != 13
            || HA_BROWSER_TYPE_SUBMIT != 1
            || HA_BROWSER_STATUS_CANCELLED != 8
            || HA_BROWSER_REF_MAX_BYTES != 4096
            || HA_BROWSER_TAB_ID_MAX_BYTES != 512
            || HA_BROWSER_SCOPE_ID_MAX_BYTES != 1024
            || HA_BROWSER_CALL_ID_MAX_BYTES != 1024
            || HA_BROWSER_TEXT_RESULT_MAX_BYTES != 262144
            || HA_BROWSER_IMAGE_RESULT_MAX_BYTES != 16777216) {
        return 1;
    }
    if (!exercise_callback(HA_BROWSER_TYPE)
            || !exercise_callback(HA_BROWSER_SCROLL)
            || !exercise_callback(HA_BROWSER_SCREENSHOT)) {
        return 2;
    }
    if (ha_runtime_init() != 0) {
        return 3;
    }
    void *engine = ha_engine_create(event_callback, NULL);
    if (engine == NULL) {
        ha_runtime_exit();
        return 4;
    }
    int32_t status = ha_engine_set_browser_callback(
        engine, browser_callback, NULL, NULL);
    if (status != HA_BROWSER_STATUS_UNAVAILABLE) {
        ha_engine_destroy(engine);
        ha_runtime_exit();
        return 5;
    }
    status = ha_engine_set_browser_callback(
        engine, NULL, browser_cancel_callback, NULL);
    if (status != HA_BROWSER_STATUS_UNAVAILABLE) {
        ha_engine_destroy(engine);
        ha_runtime_exit();
        return 6;
    }
    status = ha_engine_set_browser_callback(
        engine, browser_callback, browser_cancel_callback, NULL);
    if (status == 0) {
        status = ha_engine_set_browser_callback(
            engine, NULL, NULL, NULL);
    }
    ha_engine_destroy(engine);
    ha_runtime_exit();
    return status;
}
