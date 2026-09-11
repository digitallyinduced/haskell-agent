#define _POSIX_C_SOURCE 200809L
#include "peer.h"
#include <assert.h>
#include <stdio.h>
#include <time.h>
#include <sys/wait.h>
#include <errno.h>

static void pause_frame(void) {
    struct timespec interval = { 0, 10000000 };
    nanosleep(&interval, NULL);
}
static void operation(AgentPeer *peer, char *sdp) {
    int result = 0;
    for (int i = 0; i < 1000 && !result; ++i) {
        result = agent_peer_operation(peer, sdp, 65536);
        if (!result) pause_frame();
    }
    assert(result > 0 && result <= 65536);
    sdp[result] = 0;
}
static void local(AgentPeer *peer, int offer, char *sdp) {
    assert(agent_peer_create_description(peer, offer) == 1);
    operation(peer, sdp);
    assert(agent_peer_set_description(peer, sdp, offer, 1) == 1);
    operation(peer, sdp);
    int result = 0;
    for (int i = 0; i < 1000 && !result; ++i) {
        result = agent_peer_local_description(peer, sdp, 65536);
        if (!result) pause_frame();
    }
    assert(result > 0 && result <= 65536);
    sdp[result] = 0;
}
int main(int argc, char **argv) {
    (void)argv;
    AgentPeer *a = agent_peer_new(), *b = agent_peer_new();
    assert(a && b);
    char sdp[65537], pcm[24000], frame[480] = { 0 };
    /* Nix build sandboxes expose only loopback, which libnice excludes.
     * Always exercise IPC/SDP/bounds/cleanup; pass an argument on a networked
     * host to additionally verify complete ICE and bidirectional PCM. */
    if (argc == 1) {
        assert(agent_peer_create_description(a, 1) == 1);
        operation(a, sdp);
        assert(!agent_peer_push(a, frame, 1));
        assert(!agent_peer_push(a, frame, 24002));
        agent_peer_free(a); agent_peer_free(b);
        assert(waitpid(-1, NULL, WNOHANG) == -1 && errno == ECHILD);
        puts("Static client/helper SDP, bounds and joined cleanup passed");
        return 0;
    }
    local(a, 1, sdp);
    assert(agent_peer_set_description(b, sdp, 1, 0) == 1); operation(b, sdp);
    local(b, 0, sdp);
    assert(agent_peer_set_description(a, sdp, 0, 0) == 1); operation(a, sdp);
    for (int i = 0; i < 1000 && (agent_peer_state(a) != 1 || agent_peer_state(b) != 1); ++i) pause_frame();
    assert(agent_peer_state(a) == 1 && agent_peer_state(b) == 1);
    int received_a = 0, received_b = 0;
    for (int i = 0; i < 1000 && (received_a < 10 || received_b < 10); ++i) {
        assert(agent_peer_push(a, frame, sizeof frame) == 1);
        assert(agent_peer_push(b, frame, sizeof frame) == 1);
        int count = agent_peer_pull(a, pcm, sizeof pcm); assert(count >= 0); received_a += count > 0;
        count = agent_peer_pull(b, pcm, sizeof pcm); assert(count >= 0); received_b += count > 0;
        pause_frame();
    }
    assert(!agent_peer_push(a, frame, 1));
    assert(!agent_peer_push(a, frame, 24002));
    agent_peer_free(a); agent_peer_free(b);
    assert(received_a >= 10 && received_b >= 10);
    assert(waitpid(-1, NULL, WNOHANG) == -1 && errno == ECHILD);
    printf("Static client/helper PCM frames %d/%d; all children joined\n", received_a, received_b);
    return 0;
}
