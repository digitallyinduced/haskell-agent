#define _POSIX_C_SOURCE 200809L
#include "peer.h"
#include "audio.h"
#include "helper-protocol.h"
#include <spawn.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <time.h>
#include <fcntl.h>

extern char **environ;
typedef struct { int fd; pid_t pid; } AgentMediaChild;
struct AgentPeer { AgentMediaChild child; };
struct AgentAudio { AgentMediaChild child; };

static int media_request(AgentMediaChild *child, uint32_t operation, uint32_t argument,
    const char *input, size_t size, char *output, size_t capacity) {
    if (child->fd < 0 || size > AGENT_MEDIA_LIMIT || capacity > AGENT_MEDIA_LIMIT) return -1;
    AgentMediaMessage message = { AGENT_MEDIA_MAGIC, operation, argument, (uint32_t)size };
    int returns_bytes = operation == MEDIA_OPERATION || operation == MEDIA_LOCAL
        || operation == MEDIA_PULL || operation == MEDIA_AUDIO_READ;
    if (!media_transfer(child->fd, &message, sizeof message, 1)
        || !media_transfer(child->fd, (void *)input, size, 1)
        || !media_transfer(child->fd, &message, sizeof message, 0)
        || message.magic != AGENT_MEDIA_MAGIC || message.operation != operation
        || message.length > capacity
        || (returns_bytes && (int32_t)message.argument > 0 && message.argument != message.length)
        || (returns_bytes && (int32_t)message.argument <= 0 && message.length != 0)
        || !media_transfer(child->fd, output, message.length, 0)) {
        close(child->fd);
        child->fd = -1;
        return -1;
    }
    return (int32_t)message.argument;
}

static void media_close(AgentMediaChild *child) {
    if (child->fd >= 0) close(child->fd);
    /* Resource cleanup is joined. A wedged media driver cannot hold hangup forever. */
    for (int attempt = 0; attempt < 100; ++attempt) {
        pid_t result = waitpid(child->pid, NULL, WNOHANG);
        if (result == child->pid || (result < 0 && errno == ECHILD)) return;
        struct timespec delay = { 0, 10000000 };
        nanosleep(&delay, NULL);
    }
    kill(child->pid, SIGKILL);
    while (waitpid(child->pid, NULL, 0) < 0 && errno == EINTR) {}
}

static int media_open(AgentMediaChild *child, uint32_t operation) {
    const char *program = getenv("AGENT_MEDIA_HELPER");
    if (!program || program[0] != '/') return 0;
    int sockets[2];
    if (socketpair(AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC, 0, sockets) != 0) return 0;
    /* Keep both endpoints above the child's fixed protocol descriptor. */
    int remote = fcntl(sockets[1], F_DUPFD_CLOEXEC, 4);
    close(sockets[1]);
    if (remote < 0) { close(sockets[0]); return 0; }
    posix_spawn_file_actions_t actions;
    if (posix_spawn_file_actions_init(&actions) != 0) { close(remote); close(sockets[0]); return 0; }
    int error = posix_spawn_file_actions_addclose(&actions, sockets[0]);
    if (!error) error = posix_spawn_file_actions_adddup2(&actions, remote, 3);
    if (!error) error = posix_spawn_file_actions_addclose(&actions, remote);
    char *const argv[] = { (char *)program, NULL };
    if (!error) error = posix_spawn(&child->pid, program, &actions, NULL, argv, environ);
    posix_spawn_file_actions_destroy(&actions);
    close(remote);
    if (error) { close(sockets[0]); return 0; }
    child->fd = sockets[0];
    struct timeval timeout = { 5, 0 };
    setsockopt(child->fd, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof timeout);
    setsockopt(child->fd, SOL_SOCKET, SO_SNDTIMEO, &timeout, sizeof timeout);
    if (media_request(child, operation, 0, NULL, 0, NULL, 0) != 0) { media_close(child); return 0; }
    timeout.tv_sec = 1;
    setsockopt(child->fd, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof timeout);
    setsockopt(child->fd, SOL_SOCKET, SO_SNDTIMEO, &timeout, sizeof timeout);
    return 1;
}

AgentPeer *agent_peer_new(void) {
    AgentPeer *peer = calloc(1, sizeof *peer);
    if (peer && !media_open(&peer->child, MEDIA_OPEN_PEER)) { free(peer); return NULL; }
    return peer;
}
void agent_peer_free(AgentPeer *peer) { if (peer) { media_close(&peer->child); free(peer); } }
int agent_peer_create_description(AgentPeer *p, int offer) { return media_request(&p->child, MEDIA_CREATE, offer != 0, NULL, 0, NULL, 0); }
int agent_peer_set_description(AgentPeer *p, const char *sdp, int offer, int local) { return media_request(&p->child, MEDIA_SET, (offer != 0) | ((local != 0) << 1), sdp, strlen(sdp), NULL, 0); }
int agent_peer_operation(AgentPeer *p, char *out, size_t size) { return media_request(&p->child, MEDIA_OPERATION, 0, NULL, 0, out, size); }
int agent_peer_local_description(AgentPeer *p, char *out, size_t size) { return media_request(&p->child, MEDIA_LOCAL, 0, NULL, 0, out, size); }
int agent_peer_state(AgentPeer *p) { return media_request(&p->child, MEDIA_STATE, 0, NULL, 0, NULL, 0); }
int agent_peer_push(AgentPeer *p, const char *pcm, size_t size) {
    if (!pcm || !size || size % 2 || size > 24000) return 0;
    int result = media_request(&p->child, MEDIA_PUSH, 0, pcm, size, NULL, 0);
    return result == 1;
}
int agent_peer_pull(AgentPeer *p, char *pcm, size_t size) { return media_request(&p->child, MEDIA_PULL, (uint32_t)size, NULL, 0, pcm, size); }
AgentAudio *agent_audio_new(int capture) {
    AgentAudio *audio = calloc(1, sizeof *audio);
    if (audio && !media_open(&audio->child, capture ? MEDIA_OPEN_CAPTURE : MEDIA_OPEN_PLAYBACK)) { free(audio); return NULL; }
    return audio;
}
void agent_audio_free(AgentAudio *audio) { if (audio) { media_close(&audio->child); free(audio); } }
int agent_audio_read(AgentAudio *a, char *out, size_t size) { return media_request(&a->child, MEDIA_AUDIO_READ, (uint32_t)size, NULL, 0, out, size); }
int agent_audio_write(AgentAudio *a, const char *pcm, size_t size) {
    if (!pcm || !size || size % 2 || size > 24000) return -1;
    return media_request(&a->child, MEDIA_AUDIO_WRITE, 0, pcm, size, NULL, 0);
}
int agent_audio_state(AgentAudio *a) { return media_request(&a->child, MEDIA_AUDIO_STATE, 0, NULL, 0, NULL, 0); }
