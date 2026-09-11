#include "peer.h"
#include <glib.h>
#include <string.h>
#include <stdio.h>

static void operation(AgentPeer *peer, char *sdp) {
    gint64 deadline = g_get_monotonic_time() + 10000000;
    int result;
    while ((result = agent_peer_operation(peer, sdp, 65536)) == 0 && g_get_monotonic_time() < deadline) g_usleep(10000);
    g_assert_cmpint(result, >, 0);
    sdp[result] = 0;
}
static void local(AgentPeer *peer, int offer, char *sdp) {
    g_assert_true(agent_peer_create_description(peer, offer));
    operation(peer, sdp);
    g_assert_true(agent_peer_set_description(peer, sdp, offer, 1));
    operation(peer, sdp);
    gint64 deadline = g_get_monotonic_time() + 10000000;
    int result;
    while ((result = agent_peer_local_description(peer, sdp, 65536)) == 0 && g_get_monotonic_time() < deadline) g_usleep(10000);
    g_assert_cmpint(result, >, 0);
    sdp[result] = 0;
}
int main(void) {
    AgentPeer *a = agent_peer_new(), *b = agent_peer_new();
    g_assert_nonnull(a); g_assert_nonnull(b);
    char sdp[65537], pcm[24000], frame[960] = {0};
    local(a, 1, sdp);
    g_assert_true(agent_peer_set_description(b, sdp, 1, 0)); operation(b, sdp);
    local(b, 0, sdp);
    g_assert_true(agent_peer_set_description(a, sdp, 0, 0)); operation(a, sdp);
    /* Match the host: do not enqueue microphone data before ICE connects. */
    gint64 connected_deadline = g_get_monotonic_time() + 10000000;
    while ((agent_peer_state(a) != 1 || agent_peer_state(b) != 1) &&
           g_get_monotonic_time() < connected_deadline) {
        g_assert_cmpint(agent_peer_state(a), >=, 0);
        g_assert_cmpint(agent_peer_state(b), >=, 0);
        g_usleep(10000);
    }
    g_assert_cmpint(agent_peer_state(a), ==, 1);
    g_assert_cmpint(agent_peer_state(b), ==, 1);
    int received_a = 0, received_b = 0;
    gint64 deadline = g_get_monotonic_time() + 10000000;
    while (g_get_monotonic_time() < deadline && (received_a < 10 || received_b < 10)) {
        g_assert_cmpint(agent_peer_state(a), >=, 0); g_assert_cmpint(agent_peer_state(b), >=, 0);
        g_assert_true(agent_peer_push(a, frame, sizeof frame));
        g_assert_true(agent_peer_push(b, frame, sizeof frame));
        int count = agent_peer_pull(a, pcm, sizeof pcm); g_assert_cmpint(count, >=, 0); received_a += count > 0;
        count = agent_peer_pull(b, pcm, sizeof pcm); g_assert_cmpint(count, >=, 0); received_b += count > 0;
        g_usleep(20000);
    }
    g_assert_false(agent_peer_push(a, frame, 1));
    g_assert_false(agent_peer_push(a, frame, 24002));
    gint64 start = g_get_monotonic_time();
    agent_peer_free(a); agent_peer_free(b);
    gint64 teardown = g_get_monotonic_time() - start;
    printf("PCM frames: %d/%d; teardown: %lld us\n", received_a, received_b, (long long)teardown);
    return received_a >= 10 && received_b >= 10 && teardown < 2000000 ? 0 : 1;
}
