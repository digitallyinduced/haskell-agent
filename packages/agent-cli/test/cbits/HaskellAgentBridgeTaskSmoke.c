#include "HaskellAgentBridge.h"

#include <stddef.h>

typedef struct task_snapshot_state {
    int terminal_count;
    int invalid_item;
} task_snapshot_state;

static void task_event_callback(void *context, const uint8_t *bytes,
                                size_t length) {
    (void)context;
    (void)bytes;
    (void)length;
}

static void task_snapshot_callback(void *raw_context, int32_t status,
                                   const uint8_t *task_id,
                                   size_t task_id_length,
                                   const uint8_t *session_id,
                                   size_t session_id_length,
                                   int32_t state,
                                   const uint8_t *error,
                                   size_t error_length) {
    task_snapshot_state *context = (task_snapshot_state *)raw_context;
    (void)session_id;
    (void)session_id_length;

    if (status == 0) {
        if (task_id == NULL || task_id_length == 0
                || (state != 0 && state != 1)
                || error != NULL || error_length != 0) {
            context->invalid_item = 1;
        }
    } else if (status == 1) {
        if (task_id != NULL || task_id_length != 0
                || error != NULL || error_length != 0) {
            context->invalid_item = 1;
        }
        context->terminal_count += 1;
    } else {
        context->invalid_item = 1;
    }
}

/*
 * Exercise the task-control ABI from a native caller. FIFO command handling
 * guarantees the accepted snapshot finishes before destroy returns.
 */
int ha_task_supervisor_abi_smoke(void) {
    const uint8_t missing_task[] = "missing-task";
    task_snapshot_state state = {0, 0};

    if (ha_runtime_init() != 0) {
        return 10;
    }
    void *engine = ha_engine_create(task_event_callback, NULL);
    if (engine == NULL) {
        ha_runtime_exit();
        return 11;
    }
    if (ha_engine_set_task_limit(engine, 0) != 2
            || ha_engine_set_task_limit(engine, 33) != 2
            || ha_engine_set_task_limit(engine, 2) != 0) {
        ha_engine_destroy(engine);
        ha_runtime_exit();
        return 12;
    }
    if (ha_engine_cancel_task(
            engine, missing_task, sizeof(missing_task) - 1) != 0) {
        ha_engine_destroy(engine);
        ha_runtime_exit();
        return 13;
    }
    if (ha_engine_list_tasks(engine, task_snapshot_callback, &state) != 0) {
        ha_engine_destroy(engine);
        ha_runtime_exit();
        return 14;
    }
    ha_engine_destroy(engine);
    ha_runtime_exit();

    if (state.invalid_item) {
        return 15;
    }
    if (state.terminal_count != 1) {
        return 16;
    }
    return 0;
}
