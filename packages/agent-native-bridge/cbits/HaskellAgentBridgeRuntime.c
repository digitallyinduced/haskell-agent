#include "HaskellAgentBridge.h"

#include <HsFFI.h>
#include <pthread.h>
#include <stdlib.h>

#ifndef AGENT_BUILD_COMMIT
#define AGENT_BUILD_COMMIT development
#endif

#define HA_STRINGIFY_INNER(value) #value
#define HA_STRINGIFY(value) HA_STRINGIFY_INNER(value)

static pthread_mutex_t runtime_lock = PTHREAD_MUTEX_INITIALIZER;
static unsigned int runtime_references = 0;
static int runtime_initialized = 0;

int32_t ha_runtime_init(void) {
    pthread_mutex_lock(&runtime_lock);
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
