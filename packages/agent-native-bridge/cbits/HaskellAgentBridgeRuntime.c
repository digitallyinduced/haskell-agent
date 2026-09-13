#include "HaskellAgentBridge.h"

#include <Rts.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

#ifndef AGENT_BUILD_COMMIT
#define AGENT_BUILD_COMMIT development
#endif

#define HA_STRINGIFY_INNER(value) #value
#define HA_STRINGIFY(value) HA_STRINGIFY_INNER(value)

static pthread_mutex_t runtime_lock = PTHREAD_MUTEX_INITIALIZER;
static unsigned int runtime_references = 0;
static int runtime_initialized = 0;
static int runtime_cli_started = 0;

/* Private foreign export. The returned stable pointer belongs to this file. */
extern HsStablePtr haskell_agent_cli_main_action(void);

int32_t ha_cli_main(int argc, char **argv) {
    if (argc < 1 || argv == NULL || argv[argc] != NULL) {
        return 64;
    }
    for (int index = 0; index < argc; index++) {
        if (argv[index] == NULL) {
            return 64;
        }
    }

    pthread_mutex_lock(&runtime_lock);
    if (runtime_initialized) {
        pthread_mutex_unlock(&runtime_lock);
        return 70;
    }
    runtime_initialized = 1;
    runtime_cli_started = 1;
    pthread_mutex_unlock(&runtime_lock);

    RtsConfig configuration = defaultRtsConfig;
    configuration.rts_opts_enabled = RtsOptsAll;
    configuration.rts_opts = "-N4 -M8G";
    configuration.rts_hs_main = true;
    hs_init_ghc(&argc, &argv, configuration);
    setenv("AGENT_BUILD_COMMIT", HA_STRINGIFY(AGENT_BUILD_COMMIT), 1);

    HsStablePtr action = haskell_agent_cli_main_action();
    Capability *capability = rts_lock();
    rts_evalStableIOMain(&capability, action, NULL);
    SchedulerStatus status = rts_getSchedStatus(capability);
    rts_unlock(capability);
    hs_free_stable_ptr(action);

    /* Match hs_main's scheduler outcomes and shutdown path. runMainIO,
     * installed by rts_evalStableIOMain, handles ordinary ExitCode exceptions
     * and SIGINT itself; do not turn them into a generic bridge error. */
    int exit_status;
    switch (status) {
        case Success:
            exit_status = EXIT_SUCCESS;
            break;
        case Killed:
            fprintf(stderr, "main thread exited (uncaught exception)\n");
            exit_status = EXIT_KILLED;
            break;
        case Interrupted:
            fprintf(stderr, "interrupted\n");
            exit_status = EXIT_INTERRUPTED;
            break;
        case HeapExhausted:
            exit_status = EXIT_HEAPOVERFLOW;
            break;
        default:
            fprintf(stderr, "main thread completed with invalid status\n");
            exit_status = EXIT_FAILURE;
            break;
    }
    shutdownHaskellAndExit(exit_status, 0);
}

int32_t ha_runtime_init(void) {
    pthread_mutex_lock(&runtime_lock);
    if (runtime_cli_started) {
        pthread_mutex_unlock(&runtime_lock);
        return -1;
    }
    if (!runtime_initialized) {
        int argc = 1;
        /* Agent.ClientIdentity uses this explicit RTS product name to
         * distinguish desktop traffic from the CLI (including CLI on macOS). */
        char *argv[] = {"haskell-agent-macos", NULL};
        char **argv_pointer = argv;
        hs_init(&argc, &argv_pointer);
        setenv("AGENT_BUILD_COMMIT", HA_STRINGIFY(AGENT_BUILD_COMMIT), 1);
        runtime_initialized = 1;
    }
    runtime_references += 1;
    pthread_mutex_unlock(&runtime_lock);
    return 0;
}

void ha_runtime_exit(void) {
    pthread_mutex_lock(&runtime_lock);
    if (runtime_references > 0) {
        runtime_references -= 1;
    }
    pthread_mutex_unlock(&runtime_lock);
}
