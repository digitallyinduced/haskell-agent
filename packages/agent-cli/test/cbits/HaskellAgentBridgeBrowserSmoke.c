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

static int32_t browser_callback(
    void *context,
    int32_t command,
    const uint8_t *argument1,
    size_t argument1_length,
    const uint8_t *argument2,
    size_t argument2_length,
    double scroll_delta_x,
    double scroll_delta_y,
    int32_t flags,
    uint8_t *output,
    size_t output_capacity,
    size_t *output_length
) {
    const char *result = "native browser \xE2\x9C\x93";
    size_t result_length = strlen(result);
    (void)context;
    if (output == NULL || output_length == NULL
            || output_capacity < result_length) {
        return HA_BROWSER_STATUS_INVALID_ARGUMENT;
    }
    switch (command) {
        case HA_BROWSER_TYPE:
            if (argument1_length != 6
                    || memcmp(argument1, "#query", 6) != 0
                    || argument2_length != 7
                    || memcmp(argument2, "haskell", 7) != 0
                    || scroll_delta_x != 0 || scroll_delta_y != 0
                    || flags != HA_BROWSER_TYPE_SUBMIT) {
                return HA_BROWSER_STATUS_INVALID_ARGUMENT;
            }
            break;
        case HA_BROWSER_KEY:
            if (argument1_length != 5
                    || memcmp(argument1, "Enter", 5) != 0
                    || argument2_length != 0
                    || scroll_delta_x != 0 || scroll_delta_y != 0
                    || flags != 0) {
                return HA_BROWSER_STATUS_INVALID_ARGUMENT;
            }
            break;
        case HA_BROWSER_SCROLL:
            if (argument1_length != 0 || argument2_length != 0
                    || scroll_delta_x != 12.5 || scroll_delta_y != -800
                    || flags != 0) {
                return HA_BROWSER_STATUS_INVALID_ARGUMENT;
            }
            break;
        default:
            return HA_BROWSER_STATUS_INVALID_ARGUMENT;
    }
    memcpy(output, result, result_length);
    *output_length = result_length;
    return HA_BROWSER_STATUS_SUCCESS;
}

/*
 * Compile and exercise the exported setter exactly as a native UI host does.
 * Stack-backed callback context is safe because disabling synchronizes with
 * any callback before returning.
 */
int ha_browser_callback_abi_smoke(void) {
    uint8_t output[64];
    size_t output_length = 0;
    if (HA_BROWSER_NAVIGATE != 1 || HA_BROWSER_SNAPSHOT != 2
            || HA_BROWSER_CLICK != 3 || HA_BROWSER_TYPE != 4
            || HA_BROWSER_BACK != 5 || HA_BROWSER_FORWARD != 6
            || HA_BROWSER_RELOAD != 7 || HA_BROWSER_KEY != 8
            || HA_BROWSER_SCROLL != 9 || HA_BROWSER_TYPE_SUBMIT != 1
            || HA_BROWSER_STATUS_SUCCESS != 0
            || HA_BROWSER_STATUS_INVALID_ARGUMENT != 1
            || HA_BROWSER_STATUS_UNAVAILABLE != 2
            || HA_BROWSER_STATUS_TIMEOUT != 3
            || HA_BROWSER_STATUS_PERMISSION_DENIED != 4
            || HA_BROWSER_STATUS_UNSUPPORTED != 5
            || HA_BROWSER_STATUS_FAILED != 6
            || HA_BROWSER_STATUS_OUTPUT_TOO_LARGE != 7
            || HA_BROWSER_URL_MAX_BYTES != 8192
            || HA_BROWSER_SELECTOR_MAX_BYTES != 4096
            || HA_BROWSER_TEXT_MAX_BYTES != 65536
            || HA_BROWSER_KEY_MAX_BYTES != 128
            || HA_BROWSER_SCROLL_MAX_ABS_DELTA != 10000
            || HA_BROWSER_OUTPUT_CAPACITY != 262144) {
        return 1;
    }
    if (browser_callback(
            NULL, HA_BROWSER_TYPE,
            (const uint8_t *)"#query", 6,
            (const uint8_t *)"haskell", 7,
            0, 0,
            HA_BROWSER_TYPE_SUBMIT,
            output, sizeof(output), &output_length) != 0
            || output_length != 18
            || memcmp(output, "native browser \xE2\x9C\x93", 18) != 0) {
        return 2;
    }
    output_length = 0;
    if (browser_callback(
            NULL, HA_BROWSER_KEY,
            (const uint8_t *)"Enter", 5,
            (const uint8_t *)"", 0,
            0, 0, 0,
            output, sizeof(output), &output_length) != 0
            || output_length != 18) {
        return 3;
    }
    output_length = 0;
    if (browser_callback(
            NULL, HA_BROWSER_SCROLL,
            (const uint8_t *)"", 0,
            (const uint8_t *)"", 0,
            12.5, -800, 0,
            output, sizeof(output), &output_length) != 0
            || output_length != 18) {
        return 4;
    }
    if (ha_runtime_init() != 0) {
        return 5;
    }
    void *engine = ha_engine_create(event_callback, NULL);
    if (engine == NULL) {
        ha_runtime_exit();
        return 6;
    }
    int32_t status =
        ha_engine_set_browser_callback(engine, browser_callback, NULL);
    if (status == 0) {
        status = ha_engine_set_browser_callback(engine, NULL, NULL);
    }
    ha_engine_destroy(engine);
    ha_runtime_exit();
    return status;
}
