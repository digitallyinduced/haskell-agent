#include "HaskellAgentBridge.h"

#include <stddef.h>

static void mail_account_callback(
        void *context, int32_t status,
        const uint8_t *account_id, size_t account_id_length,
        const uint8_t *provider, size_t provider_length,
        const uint8_t *email, size_t email_length,
        const uint8_t *label, size_t label_length,
        int32_t enabled,
        const uint8_t *state, size_t state_length,
        const uint8_t *error, size_t error_length) {
    (void)context; (void)status; (void)account_id;
    (void)account_id_length; (void)provider; (void)provider_length;
    (void)email; (void)email_length; (void)label; (void)label_length;
    (void)enabled; (void)state; (void)state_length;
    (void)error; (void)error_length;
}

static void mail_oauth_start_callback(
        void *context, int32_t status,
        const uint8_t *authorization_url, size_t authorization_url_length,
        const uint8_t *flow_id, size_t flow_id_length,
        int32_t expires_in_seconds,
        const uint8_t *error, size_t error_length) {
    (void)context; (void)status; (void)authorization_url;
    (void)authorization_url_length; (void)flow_id; (void)flow_id_length;
    (void)expires_in_seconds; (void)error; (void)error_length;
}

static void mail_result_callback(
        void *context, int32_t status,
        const uint8_t *account_id, size_t account_id_length,
        const uint8_t *error, size_t error_length) {
    (void)context; (void)status; (void)account_id;
    (void)account_id_length; (void)error; (void)error_length;
}

static void mail_discovery_callback(
        void *context, int32_t status,
        const uint8_t *provider, size_t provider_length,
        const uint8_t *imap_host, size_t imap_host_length,
        int32_t imap_port,
        const uint8_t *tls_mode, size_t tls_mode_length,
        const uint8_t *username, size_t username_length,
        const uint8_t *error, size_t error_length) {
    (void)context; (void)status; (void)provider; (void)provider_length;
    (void)imap_host; (void)imap_host_length; (void)imap_port;
    (void)tls_mode; (void)tls_mode_length; (void)username;
    (void)username_length; (void)error; (void)error_length;
}

/*
 * Compile every function/callback arity as a native consumer and exercise only
 * synchronous rejection paths. No callback, credential, listener, or network
 * operation is started by this smoke test.
 */
int ha_mail_abi_smoke(void) {
    static const uint8_t gmail[] = "gmail";
    static const uint8_t client_id[] = "public-client";
    static const uint8_t email[] = "person@example.com";
    static const uint8_t label[] = "Example";
    static const uint8_t host[] = "imap.example.com";
    static const uint8_t tls[] = "tls";
    static const uint8_t username[] = "person@example.com";
    static const uint8_t password[] = "app-password";
    static const uint8_t account_id[] = "imap-1";
    static const uint8_t invalid_utf8[] = {0xff};
    ha_mail_account_list_callback list_cb = mail_account_callback;
    ha_mail_oauth_start_callback start_cb = mail_oauth_start_callback;
    ha_mail_result_callback result_cb = mail_result_callback;
    ha_mail_discovery_callback discovery_cb = mail_discovery_callback;

    if (ha_mail_accounts_list(NULL, NULL) != -1) {
        return 1;
    }
    if (ha_mail_oauth_start(
            gmail, sizeof(gmail) - 1,
            NULL, sizeof(client_id) - 1,
            start_cb, NULL) != -1) {
        return 2;
    }
    if (ha_mail_oauth_poll(NULL, 1, result_cb, NULL) != -1
            || ha_mail_oauth_cancel(NULL, 1, result_cb, NULL) != -1) {
        return 3;
    }
    if (ha_mail_discover(
            invalid_utf8, sizeof(invalid_utf8),
            discovery_cb, NULL) != -1) {
        return 4;
    }
    if (ha_mail_imap_connect(
            email, sizeof(email) - 1,
            label, sizeof(label) - 1,
            host, sizeof(host) - 1,
            993,
            tls, sizeof(tls) - 1,
            username, sizeof(username) - 1,
            NULL, sizeof(password) - 1,
            result_cb, NULL) != -1) {
        return 5;
    }
    if (ha_mail_account_set_enabled(
            account_id, sizeof(account_id) - 1,
            2, result_cb, NULL) != -1) {
        return 6;
    }
    if (ha_mail_account_delete(NULL, 1, result_cb, NULL) != -1) {
        return 7;
    }

    /* Keep all callback typedef assignments live under -Werror. */
    if (list_cb == NULL || start_cb == NULL || result_cb == NULL
            || discovery_cb == NULL) {
        return 8;
    }
    return 0;
}
