#include "HaskellAgentBridge.h"

#include <pthread.h>
#include <string.h>

struct observation_validation_state {
    pthread_mutex_t mutex;
    int callbacks;
    int terminal_callbacks;
};

static void observation_validation_callback(
    void *context, int32_t kind,
    const uint8_t *owner_id, size_t owner_id_length,
    const uint8_t *turn_id, size_t turn_id_length,
    uint64_t sequence, int64_t generation_start, int64_t durable_turn_count,
    const uint8_t *text, size_t text_length,
    const uint8_t *call_id, size_t call_id_length,
    const uint8_t *tool_name, size_t tool_name_length,
    const uint8_t *tool_summary, size_t tool_summary_length,
    uint32_t flags
) {
    struct observation_validation_state *state = context;
    (void)owner_id; (void)owner_id_length;
    (void)turn_id; (void)turn_id_length;
    (void)sequence; (void)generation_start; (void)durable_turn_count;
    (void)text; (void)text_length;
    (void)call_id; (void)call_id_length;
    (void)tool_name; (void)tool_name_length;
    (void)tool_summary; (void)tool_summary_length; (void)flags;
    pthread_mutex_lock(&state->mutex);
    state->callbacks += 1;
    if (kind == HA_SESSION_OBSERVATION_CANCELLED
        || kind == HA_SESSION_OBSERVATION_FAILURE) {
        state->terminal_callbacks += 1;
    }
    pthread_mutex_unlock(&state->mutex);
}

int ha_session_observation_validation_smoke(void) {
    struct observation_validation_state state = {
        .mutex = PTHREAD_MUTEX_INITIALIZER,
        .callbacks = 0,
        .terminal_callbacks = 0
    };
    void *handle = (void *)1;
    const uint8_t invalid_utf8[] = {0xff};
    const uint8_t embedded_null[] = {'s', 0, 's'};
    const uint8_t valid_id[] = "native-observation-validation-missing-session";
    int result = 0;
    if (ha_session_observation_start(NULL, 1,
            observation_validation_callback, &state, &handle) != 1
        || handle != NULL) {
        result = 1;
        goto finish;
    }
    if (ha_session_observation_start(invalid_utf8, sizeof(invalid_utf8),
            observation_validation_callback, &state, &handle) != 1
        || handle != NULL) {
        result = 2;
        goto finish;
    }
    if (ha_session_observation_start(embedded_null, sizeof(embedded_null),
            observation_validation_callback, &state, &handle) != 1
        || handle != NULL) {
        result = 3;
        goto finish;
    }
    if (ha_session_observation_start(valid_id, sizeof(valid_id) - 1,
            NULL, &state, &handle) != 1 || handle != NULL) {
        result = 4;
        goto finish;
    }
    if (state.callbacks != 0) {
        result = 5;
        goto finish;
    }
    if (ha_session_observation_start(valid_id, sizeof(valid_id) - 1,
            observation_validation_callback, &state, &handle) != 0
        || handle == NULL) {
        result = 6;
        goto finish;
    }
    ha_session_observation_cancel(handle);
    ha_session_observation_cancel(handle);
    ha_session_observation_destroy(handle);
    handle = NULL;
    if (state.terminal_callbacks != 1) {
        result = 7;
    }
finish:
    ha_session_observation_cancel(NULL);
    ha_session_observation_destroy(NULL);
    pthread_mutex_destroy(&state.mutex);
    return result;
}
