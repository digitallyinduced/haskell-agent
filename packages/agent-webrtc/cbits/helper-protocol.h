#pragma once
#include <stdint.h>
#include <stddef.h>
#include <errno.h>
#include <sys/socket.h>
#include <unistd.h>

/* Private, same-machine protocol over an inherited socket, never a listener.
 * All allocations are bounded. No credentials or shell commands cross it. */
#define AGENT_MEDIA_LIMIT (256u * 1024u)
#define AGENT_MEDIA_MAGIC 0x414d4431u
enum AgentMediaOperation {
    MEDIA_OPEN_PEER = 1, MEDIA_OPEN_CAPTURE, MEDIA_OPEN_PLAYBACK,
    MEDIA_CREATE, MEDIA_SET, MEDIA_OPERATION, MEDIA_LOCAL, MEDIA_STATE,
    MEDIA_PUSH, MEDIA_PULL, MEDIA_AUDIO_READ, MEDIA_AUDIO_WRITE, MEDIA_AUDIO_STATE
};
typedef struct {
    uint32_t magic, operation, argument, length;
} AgentMediaMessage;

static int media_transfer(int fd, void *buffer, size_t length, int writing) {
    char *p = buffer;
    while (length) {
        ssize_t n = writing ? send(fd, p, length, MSG_NOSIGNAL) : recv(fd, p, length, 0);
        if (n < 0 && errno == EINTR) continue;
        if (n <= 0) return 0;
        p += n;
        length -= (size_t)n;
    }
    return 1;
}
