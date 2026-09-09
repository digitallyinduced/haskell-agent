#include "HaskellAgentBridge.h"
#include <stdint.h>

static void connection_callback(
    void *context, int32_t status, uint64_t revision,
    const uint8_t *identifier, size_t identifier_length,
    const uint8_t *display_name, size_t display_name_length,
    const uint8_t *endpoint, size_t endpoint_length,
    int32_t enabled, int32_t state,
    const uint8_t *error, size_t error_length
) {
    (void)status; (void)revision; (void)identifier; (void)identifier_length;
    (void)display_name; (void)display_name_length; (void)endpoint;
    (void)endpoint_length; (void)enabled; (void)state; (void)error;
    (void)error_length;
    if (context) (*(int *)context)++;
}

static void authorization_callback(void *context, const uint8_t *url, size_t length) {
    (void)url; (void)length;
    if (context) (*(int *)context)++;
}

/* Invalid operations must not touch disk, start network activity, or callback. */
int ha_mcp_connection_validation_smoke(void) {
    int callbacks = 0;
    void *operation = (void *)(uintptr_t)1;
    const uint8_t identifier[] = "connection";
    const uint8_t display_name[] = "Account";
    const uint8_t endpoint[] = "https://example.invalid/mcp";
    const uint8_t invalid_utf8[] = {0xff};
    const uint8_t embedded_nul[] = {'a', 0, 'b'};

    if (ha_mcp_connections_list(NULL, &callbacks, &operation) != 1 || operation) return 1;
    if (ha_mcp_connections_list(connection_callback, &callbacks, NULL) != 1) return 2;
    if (ha_mcp_connection_create(0, display_name, sizeof(display_name)-1,
            endpoint, sizeof(endpoint)-1, NULL, &callbacks, &operation) != 1 || operation) return 3;
    if (ha_mcp_connection_create(0, invalid_utf8, 1, endpoint, sizeof(endpoint)-1,
            connection_callback, &callbacks, &operation) != 2 || operation) return 4;
    if (ha_mcp_connection_create(0, embedded_nul, 3, endpoint, sizeof(endpoint)-1,
            connection_callback, &callbacks, &operation) != 2 || operation) return 5;
    if (ha_mcp_connection_rename(0, identifier, sizeof(identifier)-1, NULL, 0,
            connection_callback, &callbacks, &operation) != 2 || operation) return 6;
    if (ha_mcp_connection_set_enabled(0, identifier, sizeof(identifier)-1, 2,
            connection_callback, &callbacks, &operation) != 2 || operation) return 7;
    if (ha_mcp_connection_remove(0, NULL, 1,
            connection_callback, &callbacks, &operation) != 2 || operation) return 8;
    if (ha_mcp_connection_authorize(0, identifier, sizeof(identifier)-1, NULL,
            connection_callback, &callbacks, &operation) != 1 || operation) return 9;
    if (ha_mcp_connection_authorize(0, invalid_utf8, 1, authorization_callback,
            connection_callback, &callbacks, &operation) != 2 || operation) return 10;
    ha_mcp_connection_operation_cancel(NULL);
    ha_mcp_connection_operation_destroy(NULL);
    return callbacks ? 11 : 0;
}
