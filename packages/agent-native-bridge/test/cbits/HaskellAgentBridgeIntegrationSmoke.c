#include "HaskellAgentBridge.h"

#include <stddef.h>
#include <string.h>

int ha_connection_abi_check_snapshot(const ha_connection_snapshot *snapshot) {
    if (!snapshot || snapshot->phase != 3 || snapshot->field_count != 1 ||
            snapshot->item_count != 1 || snapshot->poll_after_milliseconds != 2500) return 1;
    if (snapshot->session_id.length != 7 ||
            memcmp(snapshot->session_id.bytes, "session", 7) != 0) return 2;
    if (snapshot->fields[0].kind != 1 || snapshot->fields[0].required != 1 ||
            snapshot->fields[0].identifier.length != 3 ||
            memcmp(snapshot->fields[0].identifier.bytes, "tan", 3) != 0) return 3;
    if (snapshot->items[0].selected != 1 || snapshot->items[0].title.length != 4 ||
            memcmp(snapshot->items[0].title.bytes, "Bank", 4) != 0) return 4;
    return 0;
}

static void integration_result_callback(
        void *context, int32_t status,
        const uint8_t *json, size_t json_length,
        const uint8_t *error, size_t error_length) {
    (void)context; (void)status; (void)json; (void)json_length;
    (void)error; (void)error_length;
}

/* Compile each generic integration function and callback arity as a native
 * consumer. Haskell tests exercise synchronous rejection directly; invoking a
 * foreign export from an interpreted GHCi test process is not portable. */
int ha_integration_abi_smoke(void) {
    ha_integration_result_callback callback = integration_result_callback;
    int32_t (*list_operation)(
        void *, ha_integration_result_callback, void *) =
        ha_engine_integration_admin_list;
    int32_t (*call_operation)(
        void *, const uint8_t *, size_t, const uint8_t *, size_t,
        ha_integration_result_callback, void *) =
        ha_engine_integration_admin_call;

    if (list_operation == NULL) {
        return 1;
    }
    if (call_operation == NULL) {
        return 2;
    }
    if (callback == NULL) {
        return 3;
    }
    int32_t (*connections_list)(
        void *, ha_connection_secret_callback, void *, ha_connection_callback, void *) =
        ha_engine_connections_list;
    int32_t (*connections_search)(
        void *, const uint8_t *, size_t, const uint8_t *, size_t,
        ha_connection_secret_callback, void *, ha_connection_callback, void *) =
        ha_engine_connections_search;
    int32_t (*connection_begin)(
        void *, const uint8_t *, size_t, const uint8_t *, size_t,
        ha_connection_secret_callback, void *, ha_connection_callback, void *) =
        ha_engine_connection_begin;
    int32_t (*connection_submit)(
        void *, const uint8_t *, size_t, const ha_connection_answer *, size_t,
        ha_connection_secret_callback, void *, ha_connection_callback, void *) =
        ha_engine_connection_submit;
    int32_t (*connection_poll)(
        void *, const uint8_t *, size_t,
        ha_connection_secret_callback, void *, ha_connection_callback, void *) =
        ha_engine_connection_poll;
    int32_t (*connection_cancel)(
        void *, const uint8_t *, size_t,
        ha_connection_secret_callback, void *, ha_connection_callback, void *) =
        ha_engine_connection_cancel;
    int32_t (*connection_disconnect)(
        void *, const uint8_t *, size_t,
        ha_connection_secret_callback, void *, ha_connection_callback, void *) =
        ha_engine_connection_disconnect;
    if (!connections_list || !connections_search || !connection_begin ||
            !connection_submit || !connection_poll || !connection_cancel ||
            !connection_disconnect) return 4;
    return 0;
}
