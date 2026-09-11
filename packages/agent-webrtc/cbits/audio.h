#pragma once
#include <stddef.h>
typedef struct AgentAudio AgentAudio;
AgentAudio *agent_audio_new(int capture);
void agent_audio_free(AgentAudio *audio);
int agent_audio_read(AgentAudio *audio, char *bytes, size_t capacity);
int agent_audio_write(AgentAudio *audio, const char *bytes, size_t count);
int agent_audio_state(AgentAudio *audio);
