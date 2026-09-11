#include "audio.h"
#include <gst/gst.h>
#include <gst/app/gstappsink.h>
#include <gst/app/gstappsrc.h>

/* Device I/O stays in GStreamer; no subprocesses or Haskell callbacks.
 * The caller serializes public operations and joins readers before free. */
struct AgentAudio {
    GstElement *pipeline, *endpoint;
    GMutex mutex;
    GQueue captured;
    gsize captured_bytes;
    gint failed;
    gboolean capture;
};

static GstFlowReturn capture_sample(GstAppSink *sink, gpointer context) {
    AgentAudio *audio = context;
    GstSample *sample = gst_app_sink_pull_sample(sink);
    if (!sample) return GST_FLOW_EOS;
    GstBuffer *buffer = gst_sample_get_buffer(sample);
    gsize size = buffer ? gst_buffer_get_size(buffer) : 0;
    g_mutex_lock(&audio->mutex);
    if (!size || size % 2 || size > 24000 || audio->captured_bytes + size > 24000 ||
        audio->captured.length >= 100) {
        g_atomic_int_set(&audio->failed, 1);
    } else {
        g_queue_push_tail(&audio->captured, gst_buffer_ref(buffer));
        audio->captured_bytes += size;
    }
    g_mutex_unlock(&audio->mutex);
    gst_sample_unref(sample);
    return g_atomic_int_get(&audio->failed) ? GST_FLOW_ERROR : GST_FLOW_OK;
}

void agent_audio_free(AgentAudio *audio) {
    if (!audio) return;
    if (audio->pipeline) {
        gst_element_send_event(audio->pipeline, gst_event_new_flush_start());
        /* NULL stops and joins streaming callbacks before freeing userdata. */
        gst_element_set_state(audio->pipeline, GST_STATE_NULL);
    }
    g_queue_clear_full(&audio->captured, (GDestroyNotify)gst_buffer_unref);
    if (audio->endpoint) gst_object_unref(audio->endpoint);
    if (audio->pipeline) gst_object_unref(audio->pipeline);
    g_mutex_clear(&audio->mutex);
    g_free(audio);
}

static AgentAudio *audio_new(int capture, const char *device) {
    if (!gst_init_check(NULL, NULL, NULL)) return NULL;
    AgentAudio *audio = g_new0(AgentAudio, 1);
    g_mutex_init(&audio->mutex);
    g_queue_init(&audio->captured);
    audio->capture = capture != 0;
    gchar *description = capture ? g_strdup_printf(
        "%s ! audioconvert ! audioresample ! "
        "audio/x-raw,format=S16LE,rate=24000,channels=1,layout=interleaved ! "
        "appsink name=endpoint sync=false async=false enable-last-sample=false", device)
        : g_strdup_printf(
        "appsrc name=endpoint is-live=true format=time do-timestamp=true block=false "
        "max-bytes=24000 caps=audio/x-raw,format=S16LE,rate=24000,channels=1,layout=interleaved "
        "! audioconvert ! audioresample ! %s", device);
    GError *error = NULL;
    audio->pipeline = gst_parse_launch(description, &error);
    g_free(description);
    if (error || !audio->pipeline) {
        g_clear_error(&error);
        agent_audio_free(audio);
        return NULL;
    }
    audio->endpoint = gst_bin_get_by_name(GST_BIN(audio->pipeline), "endpoint");
    if (capture) {
        GstAppSinkCallbacks callbacks = {0};
        callbacks.new_sample = capture_sample;
        gst_app_sink_set_callbacks(GST_APP_SINK(audio->endpoint), &callbacks, audio, NULL);
    }
    if (gst_element_set_state(audio->pipeline, GST_STATE_PLAYING) == GST_STATE_CHANGE_FAILURE) {
        agent_audio_free(audio);
        return NULL;
    }
    return audio;
}

AgentAudio *agent_audio_new(int capture) {
#if defined(__APPLE__)
    return audio_new(capture, capture ? "osxaudiosrc buffer-time=40000 latency-time=10000"
                                     : "osxaudiosink buffer-time=40000 latency-time=10000");
#elif defined(__linux__)
    return audio_new(capture, capture ? "pulsesrc buffer-time=40000 latency-time=10000"
                                     : "pulsesink buffer-time=40000 latency-time=10000");
#else
    (void)capture;
    return NULL;
#endif
}

int agent_audio_state(AgentAudio *audio) {
    GstBus *bus = gst_element_get_bus(audio->pipeline);
    GstMessage *message;
    while ((message = gst_bus_pop(bus))) {
        if (GST_MESSAGE_TYPE(message) == GST_MESSAGE_ERROR || GST_MESSAGE_TYPE(message) == GST_MESSAGE_EOS)
            g_atomic_int_set(&audio->failed, 1);
        gst_message_unref(message);
    }
    gst_object_unref(bus);
    return g_atomic_int_get(&audio->failed) ? -1 : 1;
}

int agent_audio_read(AgentAudio *audio, char *bytes, size_t capacity) {
    if (!audio->capture || agent_audio_state(audio) < 0) return -1;
    g_mutex_lock(&audio->mutex);
    GstBuffer *buffer = g_queue_pop_head(&audio->captured);
    gsize size = buffer ? gst_buffer_get_size(buffer) : 0;
    audio->captured_bytes -= size;
    g_mutex_unlock(&audio->mutex);
    if (!buffer) return 0;
    int result = -1;
    if (size <= capacity && gst_buffer_extract(buffer, 0, bytes, size) == size) result = (int)size;
    gst_buffer_unref(buffer);
    return result;
}

int agent_audio_write(AgentAudio *audio, const char *bytes, size_t count) {
    if (audio->capture || !count || count % 2 || count > 24000 || agent_audio_state(audio) < 0) return -1;
    if (gst_app_src_get_current_level_bytes(GST_APP_SRC(audio->endpoint)) + count > 24000) return -1;
    GstBuffer *buffer = gst_buffer_new_allocate(NULL, count, NULL);
    if (!buffer) return -1;
    gst_buffer_fill(buffer, 0, bytes, count);
    GST_BUFFER_DURATION(buffer) = gst_util_uint64_scale(count / 2, GST_SECOND, 24000);
    return gst_app_src_push_buffer(GST_APP_SRC(audio->endpoint), buffer) == GST_FLOW_OK ? 1 : -1;
}
