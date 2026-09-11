#include "peer.h"
#include "audio.h"
#include "helper-protocol.h"
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>

/* One resource per child. EOF releases the resource, including on parent death. */
int main(void) {
    const int fd = 3;
    if (fcntl(fd, F_SETFD, FD_CLOEXEC) < 0) return 1;
    AgentPeer *peer = NULL;
    AgentAudio *audio = NULL;
    char *bytes = malloc(AGENT_MEDIA_LIMIT + 1u);
    if (!bytes) return 1;
    AgentMediaMessage request;
    int initialized = 0;
    while (media_transfer(fd, &request, sizeof request, 0)) {
        if (request.magic != AGENT_MEDIA_MAGIC || request.length > AGENT_MEDIA_LIMIT)
            break;
        if (!media_transfer(fd, bytes, request.length, 0)) break;
        bytes[request.length] = 0;
        int result = -1;
        uint32_t length = 0;
        if (!initialized) {
            if (request.length != 0) break;
            switch (request.operation) {
            case MEDIA_OPEN_PEER: peer = agent_peer_new(); result = peer ? 0 : -1; break;
            case MEDIA_OPEN_CAPTURE: audio = agent_audio_new(1); result = audio ? 0 : -1; break;
            case MEDIA_OPEN_PLAYBACK: audio = agent_audio_new(0); result = audio ? 0 : -1; break;
            default: goto done;
            }
            initialized = 1;
        } else if (peer) {
            switch (request.operation) {
            case MEDIA_CREATE: result = agent_peer_create_description(peer, request.argument != 0); break;
            case MEDIA_SET:
                if (memchr(bytes, 0, request.length)) break;
                result = agent_peer_set_description(peer, bytes, request.argument & 1, (request.argument >> 1) & 1);
                break;
            case MEDIA_OPERATION: result = agent_peer_operation(peer, bytes, AGENT_MEDIA_LIMIT); if (result > 0) length = (uint32_t)result; break;
            case MEDIA_LOCAL: result = agent_peer_local_description(peer, bytes, AGENT_MEDIA_LIMIT); if (result > 0) length = (uint32_t)result; break;
            case MEDIA_STATE: result = agent_peer_state(peer); break;
            case MEDIA_PUSH: result = agent_peer_push(peer, bytes, request.length); break;
            case MEDIA_PULL: result = agent_peer_pull(peer, bytes, request.argument <= AGENT_MEDIA_LIMIT ? request.argument : 0); if (result > 0) length = (uint32_t)result; break;
            default: goto done;
            }
        } else if (audio) {
            switch (request.operation) {
            case MEDIA_AUDIO_READ: result = agent_audio_read(audio, bytes, request.argument <= AGENT_MEDIA_LIMIT ? request.argument : 0); if (result > 0) length = (uint32_t)result; break;
            case MEDIA_AUDIO_WRITE: result = agent_audio_write(audio, bytes, request.length); break;
            case MEDIA_AUDIO_STATE: result = agent_audio_state(audio); break;
            default: goto done;
            }
        }
        AgentMediaMessage response = { AGENT_MEDIA_MAGIC, request.operation, (uint32_t)(int32_t)result, length };
        if (length > AGENT_MEDIA_LIMIT || !media_transfer(fd, &response, sizeof response, 1)
            || !media_transfer(fd, bytes, length, 1)) break;
        if (!peer && !audio) break;
    }
done:
    if (peer) agent_peer_free(peer);
    if (audio) agent_audio_free(audio);
    free(bytes);
    close(fd);
    return 0;
}
