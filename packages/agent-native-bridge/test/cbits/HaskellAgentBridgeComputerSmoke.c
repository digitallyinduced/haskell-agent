#include "HaskellAgentBridge.h"

#include <stddef.h>
#include <string.h>

static int32_t computer_callback(
    void *context,
    uint32_t abi_version,
    int32_t operation,
    uint64_t session_token,
    const uint8_t *request,
    size_t request_length,
    uint8_t *result,
    size_t result_capacity,
    size_t *result_length,
    uint8_t *accessibility,
    size_t accessibility_capacity,
    size_t *accessibility_length,
    uint8_t *image,
    size_t image_capacity,
    size_t *image_length,
    uint8_t *error,
    size_t error_capacity,
    size_t *error_length,
    uint64_t *output_session_token,
    int32_t *output_image_format
) {
    (void)result;
    (void)accessibility;
    (void)image;
    (void)error;
    if (context == NULL
            || strcmp((const char *)context, "computer-context") != 0
            || abi_version != HA_COMPUTER_ABI_VERSION
            || operation != HA_COMPUTER_OPEN
            || session_token != 0
            || request != NULL
            || request_length != 0
            || result_capacity != HA_COMPUTER_RESULT_CAPACITY
            || accessibility_capacity != HA_COMPUTER_ACCESSIBILITY_CAPACITY
            || image_capacity != HA_COMPUTER_IMAGE_CAPACITY
            || error_capacity != HA_COMPUTER_ERROR_CAPACITY
            || result_length == NULL
            || accessibility_length == NULL
            || image_length == NULL
            || error_length == NULL
            || output_session_token == NULL
            || output_image_format == NULL) {
        return HA_COMPUTER_STATUS_INVALID_ARGUMENT;
    }
    *result_length = 0;
    *accessibility_length = 0;
    *image_length = 0;
    *error_length = 0;
    *output_session_token = 42;
    *output_image_format = HA_COMPUTER_IMAGE_NONE;
    return HA_COMPUTER_STATUS_SUCCESS;
}

int ha_computer_callback_abi_smoke(void) {
    static uint8_t result[HA_COMPUTER_RESULT_CAPACITY];
    static uint8_t accessibility[HA_COMPUTER_ACCESSIBILITY_CAPACITY];
    static uint8_t image[HA_COMPUTER_IMAGE_CAPACITY];
    static uint8_t error[HA_COMPUTER_ERROR_CAPACITY];
    size_t result_length = 1;
    size_t accessibility_length = 1;
    size_t image_length = 1;
    size_t error_length = 1;
    uint64_t output_session_token = 0;
    int32_t output_image_format = -1;
    ha_computer_callback callback = computer_callback;

    if (HA_COMPUTER_ABI_VERSION != 3
            || HA_COMPUTER_OPEN != 1
            || HA_COMPUTER_LIST != 2
            || HA_COMPUTER_BIND != 3
            || HA_COMPUTER_OBSERVE_OR_ACT != 4
            || HA_COMPUTER_CLOSE != 5
            || HA_COMPUTER_REQUEST_MAX_BYTES != 1048576
            || HA_COMPUTER_RESULT_CAPACITY != 1048576
            || HA_COMPUTER_IMAGE_CAPACITY != 16777216
            || HA_COMPUTER_ACCESSIBILITY_CAPACITY != 524288) {
        return 1;
    }

    int32_t status = callback(
        (void *)"computer-context",
        HA_COMPUTER_ABI_VERSION,
        HA_COMPUTER_OPEN,
        0,
        NULL,
        0,
        result,
        sizeof(result),
        &result_length,
        accessibility,
        sizeof(accessibility),
        &accessibility_length,
        image,
        sizeof(image),
        &image_length,
        error,
        sizeof(error),
        &error_length,
        &output_session_token,
        &output_image_format
    );
    if (status != HA_COMPUTER_STATUS_SUCCESS
            || result_length != 0
            || accessibility_length != 0
            || image_length != 0
            || error_length != 0
            || output_session_token != 42
            || output_image_format != HA_COMPUTER_IMAGE_NONE) {
        return 2;
    }
    return 0;
}
