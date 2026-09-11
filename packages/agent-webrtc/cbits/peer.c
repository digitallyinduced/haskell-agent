#define GST_USE_UNSTABLE_API
#include "peer.h"
#include <gst/gst.h>
#include <gst/app/gstappsrc.h>
#include <gst/app/gstappsink.h>
#include <gst/webrtc/webrtc.h>
#include <gst/sdp/sdp.h>
#include <string.h>

/* No Haskell callbacks, device access, credentials or application signaling.
 * All cryptography, ICE, RTP and codec work belongs to GStreamer. */
struct AgentPeer {
    GstElement *pipeline, *rtc, *source, *sink, *receiver;
    GstPromise *operation;
    gint *operation_done;
    const char *description_field;
    gint failed;
};

static void incoming(GstElement *rtc, GstPad *pad, gpointer data) {
    (void)rtc;
    AgentPeer *p = data;
    if (GST_PAD_DIRECTION(pad) != GST_PAD_SRC) return;
    GstPad *target = gst_element_get_static_pad(p->receiver, "sink");
    if (gst_pad_is_linked(target) || gst_pad_link(pad, target) != GST_PAD_LINK_OK)
        g_atomic_int_set(&p->failed, 1);
    gst_object_unref(target);
}

AgentPeer *agent_peer_new(void) {
    if (!gst_init_check(NULL, NULL, NULL)) return NULL;
    AgentPeer *p = g_new0(AgentPeer, 1);
    GError *error = NULL;
    p->pipeline = gst_parse_launch(
        "webrtcbin name=rtc bundle-policy=max-bundle "
        "appsrc name=source is-live=true format=time do-timestamp=true block=false max-bytes=48000 "
        "caps=audio/x-raw,format=S16LE,rate=24000,channels=1,layout=interleaved "
        "! audioconvert ! audioresample ! opusenc frame-size=20 "
        "! rtpopuspay pt=111 ! application/x-rtp,media=audio,encoding-name=OPUS,payload=111,clock-rate=48000 ! rtc.", &error);
    if (error || !p->pipeline) goto failed;
    p->rtc = gst_bin_get_by_name(GST_BIN(p->pipeline), "rtc");
    /* Use ICE connectivity checks, not router port mappings. Optional UPnP
     * discovery otherwise starts local HTTP listeners on every interface and
     * emits warnings where listening is unavailable. Configure only this
     * peer, before gathering; do not suppress GLib or transport diagnostics. */
    GObject *ice = NULL, *agent = NULL;
    g_object_get(p->rtc, "ice-agent", &ice, NULL);
    if (ice && g_object_class_find_property(G_OBJECT_GET_CLASS(ice), "agent"))
        g_object_get(ice, "agent", &agent, NULL);
    if (agent && g_object_class_find_property(G_OBJECT_GET_CLASS(agent), "upnp"))
        g_object_set(agent, "upnp", FALSE, NULL);
    g_clear_object(&agent);
    g_clear_object(&ice);
    p->source = gst_bin_get_by_name(GST_BIN(p->pipeline), "source");
    p->receiver = gst_parse_bin_from_description(
        "queue max-size-buffers=50 max-size-bytes=96000 max-size-time=2000000000 "
        "! rtpopusdepay ! opusdec ! audioconvert ! audioresample "
        "! audio/x-raw,format=S16LE,rate=24000,channels=1,layout=interleaved "
        "! appsink name=received sync=false async=false max-buffers=100 max-bytes=96000 drop=true", TRUE, &error);
    if (error || !p->receiver) {
        if (p->receiver) gst_object_unref(p->receiver);
        p->receiver = NULL;
        goto failed;
    }
    p->sink = gst_bin_get_by_name(GST_BIN(p->receiver), "received");
    if (!gst_bin_add(GST_BIN(p->pipeline), p->receiver)) {
        gst_object_unref(p->receiver); p->receiver = NULL; goto failed;
    }
    g_signal_connect(p->rtc, "pad-added", G_CALLBACK(incoming), p);
    if (gst_element_set_state(p->pipeline, GST_STATE_PLAYING) == GST_STATE_CHANGE_FAILURE) goto failed;
    return p;
failed:
    g_clear_error(&error);
    agent_peer_free(p);
    return NULL;
}

void agent_peer_free(AgentPeer *p) {
    if (!p) return;
    if (p->operation) {
        gst_promise_interrupt(p->operation);
        gst_promise_unref(p->operation);
    }
    /* NULL joins streaming threads before the signal userdata is released. */
    if (p->pipeline) {
        gst_element_send_event(p->pipeline, gst_event_new_flush_start());
        gst_element_set_state(p->pipeline, GST_STATE_NULL);
    }
    if (p->rtc) gst_object_unref(p->rtc);
    if (p->source) gst_object_unref(p->source);
    if (p->sink) gst_object_unref(p->sink);
    if (p->pipeline) gst_object_unref(p->pipeline);
    g_free(p);
}

static void operation_done(GstPromise *promise, gpointer done) {
    (void)promise;
    g_atomic_int_set((gint *)done, 1);
}

static int begin_operation(AgentPeer *p, const char *field) {
    if (p->operation) return 0;
    p->description_field = field;
    p->operation_done = g_new0(gint, 1);
    p->operation = gst_promise_new_with_change_func(operation_done, p->operation_done, g_free);
    return 1;
}

int agent_peer_create_description(AgentPeer *p, int offer) {
    if (!begin_operation(p, offer ? "offer" : "answer")) return 0;
    g_signal_emit_by_name(p->rtc, offer ? "create-offer" : "create-answer", NULL, p->operation);
    return 1;
}

int agent_peer_set_description(AgentPeer *p, const char *sdp, int offer, int local) {
    size_t length = strlen(sdp);
    if (length > 65536 || !length || p->operation) return 0;
    GstSDPMessage *message = NULL;
    if (gst_sdp_message_new(&message) != GST_SDP_OK) return 0;
    if (gst_sdp_message_parse_buffer((const guint8 *)sdp, length, message) != GST_SDP_OK) {
        gst_sdp_message_free(message); return 0;
    }
    GstWebRTCSessionDescription *description = gst_webrtc_session_description_new(
        offer ? GST_WEBRTC_SDP_TYPE_OFFER : GST_WEBRTC_SDP_TYPE_ANSWER, message);
    begin_operation(p, NULL);
    g_signal_emit_by_name(p->rtc, local ? "set-local-description" : "set-remote-description", description, p->operation);
    gst_webrtc_session_description_free(description);
    return 1;
}

static int copy_description(GstWebRTCSessionDescription *description, char *output, size_t capacity) {
    if (!description) return -1;
    gchar *text = gst_sdp_message_as_text(description->sdp);
    size_t length = text ? strlen(text) : 0;
    int result = -1;
    if (length && length <= 65536 && length <= capacity) {
        memcpy(output, text, length); result = (int)length;
    }
    g_free(text);
    gst_webrtc_session_description_free(description);
    return result;
}

int agent_peer_operation(AgentPeer *p, char *output, size_t capacity) {
    if (!p->operation) return -1;
    if (!g_atomic_int_get(p->operation_done)) return 0;
    int result = -1;
    if (gst_promise_wait(p->operation) == GST_PROMISE_RESULT_REPLIED) {
        const GstStructure *reply = gst_promise_get_reply(p->operation);
        if (!reply || !gst_structure_has_field(reply, "error")) {
            if (p->description_field && reply) {
                GstWebRTCSessionDescription *description = NULL;
                gst_structure_get(reply, p->description_field, GST_TYPE_WEBRTC_SESSION_DESCRIPTION, &description, NULL);
                result = copy_description(description, output, capacity);
            } else if (capacity) { output[0] = 0; result = 1; }
        }
    }
    gst_promise_unref(p->operation); p->operation = NULL; p->operation_done = NULL;
    return result;
}

int agent_peer_local_description(AgentPeer *p, char *output, size_t capacity) {
    GstWebRTCICEGatheringState state;
    g_object_get(p->rtc, "ice-gathering-state", &state, NULL);
    if (state != GST_WEBRTC_ICE_GATHERING_STATE_COMPLETE) return 0;
    GstWebRTCSessionDescription *description = NULL;
    g_object_get(p->rtc, "local-description", &description, NULL);
    return copy_description(description, output, capacity);
}

int agent_peer_state(AgentPeer *p) {
    GstBus *bus = gst_element_get_bus(p->pipeline);
    GstMessage *error = gst_bus_pop_filtered(bus, GST_MESSAGE_ERROR | GST_MESSAGE_EOS);
    if (error) { g_atomic_int_set(&p->failed, 1); gst_message_unref(error); }
    gst_object_unref(bus);
    if (g_atomic_int_get(&p->failed)) return -1;
    GstWebRTCPeerConnectionState state;
    g_object_get(p->rtc, "connection-state", &state, NULL);
    if (state == GST_WEBRTC_PEER_CONNECTION_STATE_FAILED || state == GST_WEBRTC_PEER_CONNECTION_STATE_CLOSED ||
        state == GST_WEBRTC_PEER_CONNECTION_STATE_DISCONNECTED) return -1;
    return state == GST_WEBRTC_PEER_CONNECTION_STATE_CONNECTED ? 1 : 0;
}

int agent_peer_push(AgentPeer *p, const char *pcm, size_t length) {
    if (!length || length > 24000 || length % 2 ||
        gst_app_src_get_current_level_bytes(GST_APP_SRC(p->source)) + length > 48000) return 0;
    GstBuffer *buffer = gst_buffer_new_allocate(NULL, length, NULL);
    if (!buffer) return 0;
    gst_buffer_fill(buffer, 0, pcm, length);
    GST_BUFFER_DURATION(buffer) = gst_util_uint64_scale(length / 2, GST_SECOND, 24000);
    return gst_app_src_push_buffer(GST_APP_SRC(p->source), buffer) == GST_FLOW_OK;
}

int agent_peer_pull(AgentPeer *p, char *pcm, size_t capacity) {
    GstSample *sample = gst_app_sink_try_pull_sample(GST_APP_SINK(p->sink), 0);
    if (!sample) return 0;
    GstBuffer *buffer = gst_sample_get_buffer(sample);
    size_t length = buffer ? gst_buffer_get_size(buffer) : 0;
    int result = -1;
    if (length && length <= capacity && length % 2 == 0) {
        gst_buffer_extract(buffer, 0, pcm, length); result = (int)length;
    }
    gst_sample_unref(sample);
    return result;
}
