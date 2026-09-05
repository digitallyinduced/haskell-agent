#include "HaskellAgentBridge.h"

#include <stddef.h>
#include <string.h>

_Static_assert(sizeof(ha_computer_point_v1) == 8,
    "ha_computer_point_v1 must remain 8 bytes");
_Static_assert(sizeof(ha_computer_action_v1) == 64,
    "ha_computer_action_v1 must remain 64 bytes");
_Static_assert(offsetof(ha_computer_action_v1, struct_size) == 0,
    "unexpected struct_size offset");
_Static_assert(offsetof(ha_computer_action_v1, action) == 4,
    "unexpected action offset");
_Static_assert(offsetof(ha_computer_action_v1, x) == 8,
    "unexpected x offset");
_Static_assert(offsetof(ha_computer_action_v1, y) == 12,
    "unexpected y offset");
_Static_assert(offsetof(ha_computer_action_v1, delta_x) == 16,
    "unexpected delta_x offset");
_Static_assert(offsetof(ha_computer_action_v1, delta_y) == 20,
    "unexpected delta_y offset");
_Static_assert(offsetof(ha_computer_action_v1, button) == 24,
    "unexpected button offset");
_Static_assert(offsetof(ha_computer_action_v1, modifiers) == 28,
    "unexpected modifiers offset");
_Static_assert(offsetof(ha_computer_action_v1, text_offset) == 32,
    "unexpected text_offset offset");
_Static_assert(offsetof(ha_computer_action_v1, text_length) == 40,
    "unexpected text_length offset");
_Static_assert(offsetof(ha_computer_action_v1, point_offset) == 48,
    "unexpected point_offset offset");
_Static_assert(offsetof(ha_computer_action_v1, point_count) == 56,
    "unexpected point_count offset");

static int32_t computer_callback(
    void *context,
    uint32_t abi_version,
    int32_t operation,
    uint64_t expected_display_token,
    int32_t expected_width,
    int32_t expected_height,
    const ha_computer_action_v1 *actions,
    size_t action_count,
    const ha_computer_point_v1 *points,
    size_t point_count,
    const uint8_t *text,
    size_t text_length,
    int32_t requested_image_format,
    uint8_t *output,
    size_t output_capacity,
    size_t *output_length,
    uint8_t *accessibility_output,
    size_t accessibility_output_capacity,
    size_t *accessibility_output_length,
    uint64_t *output_display_token,
    int32_t *output_width,
    int32_t *output_height,
    int32_t *output_image_format
) {
    const char *expected_context = "computer-context";
    if (context == NULL
            || strcmp((const char *)context, expected_context) != 0
            || abi_version != HA_COMPUTER_ABI_VERSION
            || operation != HA_COMPUTER_QUERY_DISPLAY
            || expected_display_token != 0
            || expected_width != 0 || expected_height != 0
            || actions != NULL || action_count != 0
            || points != NULL || point_count != 0
            || text != NULL || text_length != 0
            || requested_image_format != HA_COMPUTER_IMAGE_NONE
            || output == NULL || output_capacity != HA_COMPUTER_ERROR_CAPACITY
            || output_length == NULL
            || accessibility_output != NULL
            || accessibility_output_capacity != 0
            || accessibility_output_length == NULL
            || output_display_token == NULL
            || output_width == NULL
            || output_height == NULL || output_image_format == NULL) {
        return HA_COMPUTER_STATUS_INVALID_ARGUMENT;
    }
    *output_length = 0;
    *accessibility_output_length = 0;
    *output_display_token = 42;
    *output_width = 1440;
    *output_height = 900;
    *output_image_format = HA_COMPUTER_IMAGE_NONE;
    return HA_COMPUTER_STATUS_SUCCESS;
}

int ha_computer_callback_abi_smoke(void) {
    uint8_t output[HA_COMPUTER_ERROR_CAPACITY];
    size_t output_length = 17;
    size_t accessibility_output_length = 17;
    uint64_t output_display_token = 0;
    int32_t output_width = 0;
    int32_t output_height = 0;
    int32_t output_image_format = -1;
    ha_computer_callback callback = computer_callback;

    if (HA_COMPUTER_ABI_VERSION != 2
            || HA_COMPUTER_ACTION_STRUCT_SIZE_V1 != 64
            || HA_COMPUTER_MAX_ACTIONS != 10
            || HA_COMPUTER_MAX_POINTS_PER_ACTION != 1024
            || HA_COMPUTER_MAX_TOTAL_POINTS != 10240
            || HA_COMPUTER_MAX_TEXT_BYTES != 327680
            || HA_COMPUTER_OUTPUT_CAPACITY != 16777216
            || HA_COMPUTER_ACCESSIBILITY_CAPACITY != 524288
            || HA_COMPUTER_STATUS_DISPLAY_CHANGED != 9) {
        return 1;
    }

    int32_t status = callback(
        (void *)"computer-context",
        HA_COMPUTER_ABI_VERSION,
        HA_COMPUTER_QUERY_DISPLAY,
        0,
        0,
        0,
        NULL,
        0,
        NULL,
        0,
        NULL,
        0,
        HA_COMPUTER_IMAGE_NONE,
        output,
        sizeof(output),
        &output_length,
        NULL,
        0,
        &accessibility_output_length,
        &output_display_token,
        &output_width,
        &output_height,
        &output_image_format
    );
    if (status != HA_COMPUTER_STATUS_SUCCESS
            || output_length != 0
            || accessibility_output_length != 0
            || output_display_token != 42
            || output_width != 1440
            || output_height != 900
            || output_image_format != HA_COMPUTER_IMAGE_NONE) {
        return 2;
    }
    return 0;
}
