#include "HaskellAgentBridge.h"

/* No credentials or network: exercise every exported entry point. */
int ha_mobile_validation_smoke(void) {
    if (ha_mobile_session_open(NULL, NULL) != 1) return 1;
    if (ha_mobile_runner_register(0, NULL, 0, NULL, 0, NULL, NULL) != 1) return 2;
    if (ha_mobile_pairings_list(0, NULL, NULL) != 1) return 3;
    if (ha_mobile_pairing_revoke(0, NULL, 0, NULL, NULL) != 1) return 4;
    if (ha_mobile_pairing_wake(0, NULL, 0, NULL, NULL) != 1) return 5;
    if (ha_mobile_relay_open(0, NULL, 0, NULL, NULL) != 1) return 6;
    if (ha_mobile_relay_send(0, NULL, 0, NULL, NULL) != 1) return 7;
    if (ha_mobile_relay_receive(0, NULL, NULL) != 1) return 8;
    if (ha_mobile_handle_close(0) != 0) return 9;
    if (ha_mobile_handle_close(0) != 0) return 10;
    return 0;
}
