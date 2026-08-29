#include "HaskellAgentBridge.h"

#include <HsFFI.h>
#include <pthread.h>

static pthread_mutex_t runtime_lock = PTHREAD_MUTEX_INITIALIZER;
static unsigned int runtime_references = 0;

int32_t ha_runtime_init(void) {
    pthread_mutex_lock(&runtime_lock);
    if (runtime_references == 0) {
        int argc = 1;
        char *argv[] = {"haskell-agent-macos", NULL};
        char **argv_pointer = argv;
        hs_init(&argc, &argv_pointer);
    }
    runtime_references += 1;
    pthread_mutex_unlock(&runtime_lock);
    return 0;
}

void ha_runtime_exit(void) {
    pthread_mutex_lock(&runtime_lock);
    if (runtime_references > 0) {
        runtime_references -= 1;
        if (runtime_references == 0) {
            hs_exit();
        }
    }
    pthread_mutex_unlock(&runtime_lock);
}
