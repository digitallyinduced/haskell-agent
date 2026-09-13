#include "HaskellAgentBridge.h"
#include <string.h>

struct syntax_observation {
    const uint8_t *source;
    size_t source_length;
    size_t end;
    unsigned int classes;
    int invalid;
};

static void observe_syntax(void *context, size_t offset, size_t length, int32_t token_class) {
    struct syntax_observation *observation = context;
    if (!length || offset < observation->end || offset > observation->source_length
        || length > observation->source_length - offset
        || token_class < HA_SYNTAX_NORMAL || token_class > HA_SYNTAX_ERROR) {
        observation->invalid = 1;
        return;
    }
    /* Neither boundary may point into a UTF-8 continuation byte. */
    if ((observation->source[offset] & 0xc0) == 0x80
        || (offset + length < observation->source_length
            && (observation->source[offset + length] & 0xc0) == 0x80))
        observation->invalid = 1;
    observation->end = offset + length;
    observation->classes |= 1u << token_class;
}

int ha_syntax_abi_smoke(void) {
    const uint8_t language[] = "haskell";
    const uint8_t source[] =
        "module Main where\n\n-- Unicode: \xf0\x9f\x8c\x8d\nmain :: IO ()\n"
        "main = putStrLn \"Hello, \xc3\xa4!\"\n";
    struct syntax_observation observation = {source, sizeof(source) - 1, 0, 0, 0};
    if (ha_syntax_highlight(language, sizeof(language) - 1,
            source, sizeof(source) - 1, observe_syntax, &observation) != 0)
        return 1;
    unsigned int required = (1u << HA_SYNTAX_KEYWORD) | (1u << HA_SYNTAX_TYPE)
        | (1u << HA_SYNTAX_STRING) | (1u << HA_SYNTAX_COMMENT);
    if (observation.invalid || (observation.classes & required) != required)
        return 2;
    if (observation.end != sizeof(source) - 2) return 3;
    if (ha_syntax_highlight(NULL, 1, source, sizeof(source) - 1,
            observe_syntax, &observation) != 2) return 4;
    if (ha_syntax_highlight(language, sizeof(language) - 1, NULL, 1,
            observe_syntax, &observation) != 2) return 5;
    if (ha_syntax_highlight(language, sizeof(language) - 1, source, sizeof(source) - 1,
            NULL, &observation) != 2) return 6;
    const uint8_t malformed[] = {0xff};
    if (ha_syntax_highlight(language, sizeof(language) - 1, malformed, 1,
            observe_syntax, &observation) != 2) return 7;
    if (ha_syntax_highlight(language, sizeof(language) - 1, source, 256 * 1024 + 1,
            observe_syntax, &observation) != 1) return 8;
    const uint8_t unknown[] = "unknown-syntax-language";
    observation.end = 0;
    observation.classes = 0;
    if (ha_syntax_highlight(unknown, sizeof(unknown) - 1, source, sizeof(source) - 1,
            observe_syntax, &observation) != 1 || observation.classes) return 9;
    if (ha_syntax_highlight(language, sizeof(language) - 1, NULL, 0,
            observe_syntax, &observation) != 0 || observation.classes) return 10;
    uint8_t many_lines[5000];
    memset(many_lines, '\n', sizeof(many_lines));
    if (ha_syntax_highlight(language, sizeof(language) - 1, many_lines, sizeof(many_lines),
            observe_syntax, &observation) != 1 || observation.classes) return 11;
    /* An alias and a missing final newline retain exactly the supplied range. */
    const uint8_t alias[] = "hs";
    const uint8_t short_source[] = "main = pure 42";
    observation = (struct syntax_observation){short_source, sizeof(short_source) - 1, 0, 0, 0};
    if (ha_syntax_highlight(alias, sizeof(alias) - 1, short_source, sizeof(short_source) - 1,
            observe_syntax, &observation) != 0 || observation.invalid
            || observation.end != sizeof(short_source) - 1
            || !(observation.classes & (1u << HA_SYNTAX_NUMBER))) return 12;
    return 0;
}
