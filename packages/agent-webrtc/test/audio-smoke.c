/* Synthetic devices: no microphone permission or audio hardware required. */
#include "../cbits/audio.c"

int main(void) {
    AgentAudio *capture = audio_new(1, "audiotestsrc is-live=true samplesperbuffer=240 wave=silence");
    AgentAudio *playback = audio_new(0, "fakesink sync=true async=false");
    g_assert_nonnull(capture);
    g_assert_nonnull(playback);
    char bytes[24000];
    int received = 0;
    gint64 deadline = g_get_monotonic_time() + 2000000;
    while (received < 10 && g_get_monotonic_time() < deadline) {
        int size = agent_audio_read(capture, bytes, sizeof bytes);
        g_assert_cmpint(size, >=, 0);
        if (size) {
            for (int i = 0; i < size; ++i) g_assert_cmpint(bytes[i], ==, 0);
            g_assert_cmpint(agent_audio_write(playback, bytes, size), ==, 1);
            ++received;
        }
        g_usleep(1000);
    }
    g_assert_cmpint(received, ==, 10);
    g_assert_cmpint(agent_audio_write(playback, bytes, 1), ==, -1);
    g_assert_cmpint(agent_audio_write(playback, bytes, 24002), ==, -1);
    gint64 start = g_get_monotonic_time();
    agent_audio_free(playback);
    g_assert_cmpint(g_get_monotonic_time() - start, <, 500000);
    g_assert_cmpint(agent_audio_state(capture), ==, 1);
    playback = audio_new(0, "fakesink sync=true async=false");
    g_assert_nonnull(playback);
    agent_audio_free(playback);
    /* A stalled consumer fails closed rather than accumulating audio. */
    deadline = g_get_monotonic_time() + 2000000;
    while (agent_audio_state(capture) > 0 && g_get_monotonic_time() < deadline)
        g_usleep(1000);
    g_assert_cmpint(agent_audio_state(capture), ==, -1);
    agent_audio_free(capture);
    g_assert_null(audio_new(0, "missing-voice-device"));
    g_print("Audio device PCM, interruption, bounds and cleanup passed.\n");
    return 0;
}
