#include "HaskellAgentBridge.h"

#include <stddef.h>

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
    return 0;
}
