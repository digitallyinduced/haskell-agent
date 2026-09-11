/* White-box benchmark: vary receiver jitter buffering without changing the
 * production interface. Measures PCM submission to decoded PCM, not devices,
 * Internet transit, model inference, or end-of-turn detection. */
#include "../cbits/peer.c"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

static void complete(AgentPeer *peer, char *description) {
    gint64 deadline = g_get_monotonic_time() + 10000000;
    int count;
    while ((count = agent_peer_operation(peer, description, 65536)) == 0 &&
           g_get_monotonic_time() < deadline) g_usleep(1000);
    g_assert_cmpint(count, >, 0);
    description[count] = 0;
}

static void describe(AgentPeer *peer, int offer, char *description) {
    g_assert_true(agent_peer_create_description(peer, offer));
    complete(peer, description);
    g_assert_true(agent_peer_set_description(peer, description, offer, 1));
    complete(peer, description);
    gint64 deadline = g_get_monotonic_time() + 10000000;
    int count;
    while ((count = agent_peer_local_description(peer, description, 65536)) == 0 &&
           g_get_monotonic_time() < deadline) g_usleep(1000);
    g_assert_cmpint(count, >, 0);
    description[count] = 0;
}

static double measure(guint latency, int warmup) {
    AgentPeer *sender = agent_peer_new(), *receiver = agent_peer_new();
    g_assert_nonnull(sender); g_assert_nonnull(receiver);
    g_object_set(receiver->rtc, "latency", latency, NULL);
    char description[65537];
    describe(sender, 1, description);
    g_assert_true(agent_peer_set_description(receiver, description, 1, 0));
    complete(receiver, description);
    describe(receiver, 0, description);
    g_assert_true(agent_peer_set_description(sender, description, 0, 0));
    complete(sender, description);
    gint64 deadline = g_get_monotonic_time() + 10000000;
    while ((agent_peer_state(sender) != 1 || agent_peer_state(receiver) != 1) &&
           g_get_monotonic_time() < deadline) g_usleep(1000);
    g_assert_cmpint(agent_peer_state(sender), ==, 1);
    g_assert_cmpint(agent_peer_state(receiver), ==, 1);
    gint16 frame[480], decoded[12000];
    gint64 next = g_get_monotonic_time(), submitted = 0, detected = 0;
    deadline = next + 5000000;
    int sequence = 0;
    while (!detected && g_get_monotonic_time() < deadline) {
        gint64 now = g_get_monotonic_time();
        if (now >= next) {
            for (int sample = 0; sample < 480; sample++)
                frame[sample] = sequence >= warmup ? (gint16)(12000 * sin(sample * 2 * G_PI / 24)) : 0;
            if (sequence == warmup) submitted = now;
            g_assert_true(agent_peer_push(sender, (char *)frame, sizeof frame));
            sequence++;
            next += 20000;
        }
        int count = agent_peer_pull(receiver, (char *)decoded, sizeof decoded);
        g_assert_cmpint(count, >=, 0);
        for (int sample = 0; sample < count / 2; sample++)
            if (abs(decoded[sample]) > 3000 && !detected) detected = g_get_monotonic_time();
        g_usleep(1000);
    }
    g_assert_cmpint(submitted, >, 0);
    g_assert_cmpint(detected, >, submitted);
    agent_peer_free(sender); agent_peer_free(receiver);
    return (detected - submitted) / 1000.0;
}

int main(void) {
    setvbuf(stdout, NULL, _IOLBF, 0);
    AgentPeer *peer = agent_peer_new();
    g_assert_nonnull(peer);
    guint baseline = 0;
    g_object_get(peer->rtc, "latency", &baseline, NULL);
    agent_peer_free(peer);
    printf("production_jitter_buffer_ms=%u\n", baseline);
    for (int warmup = 0; warmup <= 50; warmup += 50)
        for (int repetition = 0; repetition < 5; repetition++) {
            printf("warmup_frames=%d jitter_ms=%u sample=%d latency_ms=%.3f\n",
                   warmup, baseline, repetition, measure(baseline, warmup));
            printf("warmup_frames=%d jitter_ms=50 sample=%d latency_ms=%.3f\n",
                   warmup, repetition, measure(50, warmup));
        }
    return 0;
}
