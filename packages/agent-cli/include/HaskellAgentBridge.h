#ifndef HASKELL_AGENT_BRIDGE_H
#define HASKELL_AGENT_BRIDGE_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef void (*ha_event_callback)(
    void *context,
    /*
     * Events are normally UTF-8 JSON. Native loop events use a versioned
     * binary HAEV frame:
     *   magic "HAEV" (4), version (u8), kind (u8), flags (u16 BE),
     *   turn-id and kind-specific fields as u32-BE length-prefixed UTF-8.
     * Kind 1 is reasoning text, 2 text, 3 status, 4 tool-start, and 5
     * tool-finish. Tool-start flags use bit 0 for encrypted arguments and
     * bit 1 for truncation; tool-finish uses bit 1 for truncation.
     * The bytes are valid only for the duration of this callback; copy before
     * returning.
     */
    const uint8_t *bytes,
    size_t length
);

typedef void (*ha_account_list_callback)(
    void *context,
    int32_t status,
    const uint8_t *provider, size_t provider_length,
    const uint8_t *billing, size_t billing_length,
    const uint8_t *selection_id, size_t selection_id_length,
    const uint8_t *account_id, size_t account_id_length,
    const uint8_t *label, size_t label_length,
    const uint8_t *detail, size_t detail_length,
    const uint8_t *managed_id, size_t managed_id_length,
    const uint8_t *source, size_t source_length,
    int32_t enabled,
    int32_t can_manage,
    const uint8_t *error, size_t error_length
);

typedef void (*ha_account_result_callback)(
    void *context,
    int32_t status,
    const uint8_t *account_id,
    size_t account_id_length,
    const uint8_t *error,
    size_t error_length
);

typedef void (*ha_account_oauth_start_callback)(
    void *context,
    int32_t status,
    const uint8_t *verification_url,
    size_t verification_url_length,
    const uint8_t *user_code,
    size_t user_code_length,
    const uint8_t *device_auth_id,
    size_t device_auth_id_length,
    const uint8_t *device_code,
    size_t device_code_length,
    int32_t poll_interval_seconds,
    int32_t expires_in_seconds,
    const uint8_t *error,
    size_t error_length
);

/* Runtime calls are process-global and reference counted. */
int32_t ha_runtime_init(void);
void ha_runtime_exit(void);

/*
 * Engine calls must be serialized with ha_engine_destroy. Requests are copied
 * before ha_engine_send_json returns. Send status is 0 when accepted, 1 for a
 * null engine, 2 for null non-empty bytes, 3 for an internal failure, and 4
 * for an invalid request envelope.
 */
void *ha_engine_create(ha_event_callback callback, void *context);
int32_t ha_engine_send_json(
    void *engine,
    const uint8_t *bytes,
    size_t length
);
void ha_engine_destroy(void *engine);

/*
 * Account operations use typed callbacks. Callback buffers are valid only
 * during the callback and must be copied by the caller. All functions return
 * 0 when accepted, or a nonzero error before starting the worker.
 */
int32_t ha_accounts_list(ha_account_list_callback callback, void *context);
int32_t ha_account_oauth_start(
    const uint8_t *provider,
    size_t provider_length,
    ha_account_oauth_start_callback callback,
    void *context
);
int32_t ha_account_oauth_poll(
    const uint8_t *provider,
    size_t provider_length,
    const uint8_t *verification_url,
    size_t verification_url_length,
    const uint8_t *user_code,
    size_t user_code_length,
    const uint8_t *device_auth_id,
    size_t device_auth_id_length,
    const uint8_t *device_code,
    size_t device_code_length,
    int32_t poll_interval_seconds,
    int32_t expires_in_seconds,
    ha_account_result_callback callback,
    void *context
);
int32_t ha_account_api_key_connect(
    const uint8_t *provider,
    size_t provider_length,
    const uint8_t *api_key,
    size_t api_key_length,
    ha_account_result_callback callback,
    void *context
);
int32_t ha_account_set_enabled(
    const uint8_t *managed_id,
    size_t managed_id_length,
    int32_t enabled,
    ha_account_result_callback callback,
    void *context
);
int32_t ha_account_delete(
    const uint8_t *managed_id,
    size_t managed_id_length,
    ha_account_result_callback callback,
    void *context
);

#ifdef __cplusplus
}
#endif

#endif
