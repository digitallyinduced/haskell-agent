#ifndef AGENT_WEBRTC_PEER_H
#define AGENT_WEBRTC_PEER_H
#include <stddef.h>
typedef struct AgentPeer AgentPeer;
AgentPeer *agent_peer_new(void);
void agent_peer_free(AgentPeer *peer);
int agent_peer_create_description(AgentPeer *peer, int offer);
int agent_peer_set_description(AgentPeer *peer, const char *sdp, int offer, int local);
/* 0 pending, 1 complete, -1 failed; SDP functions return byte count. */
int agent_peer_operation(AgentPeer *peer, char *output, size_t capacity);
int agent_peer_local_description(AgentPeer *peer, char *output, size_t capacity);
int agent_peer_state(AgentPeer *peer);
int agent_peer_push(AgentPeer *peer, const char *pcm, size_t length);
int agent_peer_pull(AgentPeer *peer, char *pcm, size_t capacity);
#endif
