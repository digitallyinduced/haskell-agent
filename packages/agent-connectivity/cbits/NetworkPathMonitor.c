#include <Network/Network.h>
#include <dispatch/dispatch.h>
#include <stdlib.h>

typedef void (*haskell_agent_network_path_callback)(int satisfied);

typedef struct {
    nw_path_monitor_t monitor;
    dispatch_queue_t queue;
    dispatch_semaphore_t cancelled;
    haskell_agent_network_path_callback callback;
} haskell_agent_network_path_monitor;

haskell_agent_network_path_monitor *
haskell_agent_network_path_monitor_create(
    haskell_agent_network_path_callback callback)
{
    if (callback == NULL) {
        return NULL;
    }

    haskell_agent_network_path_monitor *wrapper =
        calloc(1, sizeof(haskell_agent_network_path_monitor));
    if (wrapper == NULL) {
        return NULL;
    }

    wrapper->monitor = nw_path_monitor_create();
    wrapper->queue =
        dispatch_queue_create("dev.haskell-agent.network-path", DISPATCH_QUEUE_SERIAL);
    wrapper->cancelled = dispatch_semaphore_create(0);
    wrapper->callback = callback;

    if (wrapper->monitor == NULL || wrapper->queue == NULL ||
        wrapper->cancelled == NULL) {
        if (wrapper->monitor != NULL) {
            nw_release(wrapper->monitor);
        }
#if !OS_OBJECT_USE_OBJC
        if (wrapper->queue != NULL) {
            dispatch_release(wrapper->queue);
        }
        if (wrapper->cancelled != NULL) {
            dispatch_release(wrapper->cancelled);
        }
#endif
        free(wrapper);
        return NULL;
    }

    nw_path_monitor_set_queue(wrapper->monitor, wrapper->queue);
    nw_path_monitor_set_update_handler(
        wrapper->monitor,
        ^(nw_path_t path) {
            wrapper->callback(
                nw_path_get_status(path) == nw_path_status_satisfied);
        });
    nw_path_monitor_set_cancel_handler(
        wrapper->monitor,
        ^{
            dispatch_semaphore_signal(wrapper->cancelled);
        });
    nw_path_monitor_start(wrapper->monitor);

    return wrapper;
}

void
haskell_agent_network_path_monitor_destroy(
    haskell_agent_network_path_monitor *wrapper)
{
    if (wrapper == NULL) {
        return;
    }

    nw_path_monitor_cancel(wrapper->monitor);
    dispatch_semaphore_wait(wrapper->cancelled, DISPATCH_TIME_FOREVER);

    /*
     * The semaphore is signalled at the end of the monitor's serial callback
     * queue. Enqueueing one final synchronous block guarantees that the
     * cancellation handler itself has returned before its captured wrapper is
     * released.
     */
    dispatch_sync(wrapper->queue, ^{});

    nw_release(wrapper->monitor);
#if !OS_OBJECT_USE_OBJC
    dispatch_release(wrapper->queue);
    dispatch_release(wrapper->cancelled);
#endif
    free(wrapper);
}
