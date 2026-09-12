#include "HaskellAgentBridge.h"
#include <stdatomic.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

typedef struct {
    _Atomic int terminal;
    int items;
    int warnings;
    int source;
    int invalid;
    int terminal_count;
    char *identity;
    char *instructions;
} skill_result;

static char *copy_text(const char *text, size_t count) {
    char *copy = malloc(count + 1);
    if (count) memcpy(copy, text, count);
    copy[count] = '\0';
    return copy;
}

static void receive_skill(void *context, int32_t status, int32_t source,
    const char *identity, size_t identity_length,
    const char *name, size_t name_length,
    const char *description, size_t description_length,
    const char *instructions, size_t instructions_length,
    const char *error, size_t error_length) {
    skill_result *result = context;
    if (status == 0) {
        if (!identity || !identity_length || !name || !name_length
                || !description || !description_length || source < 0 || source > 2)
            result->invalid = 1;
        if (name_length == strlen("catalog-fixture")
                && memcmp(name, "catalog-fixture", name_length) == 0) {
            result->items++;
            result->source = source;
            free(result->identity);
            free(result->instructions);
            result->identity = copy_text(identity, identity_length);
            result->instructions = copy_text(instructions, instructions_length);
        }
    } else if (status == 2) {
        result->warnings++;
        if (!error || !error_length) result->invalid = 1;
    } else {
        result->terminal_count++;
        atomic_store(&result->terminal, status);
    }
}

static int await_result(skill_result *result) {
    for (int attempt = 0; attempt < 3000; attempt++) {
        if (atomic_load(&result->terminal)) return 1;
        usleep(10000);
    }
    return 0;
}

/* Heap contexts deliberately survive a timeout, so late callbacks remain safe. */
int ha_installed_skills_abi_smoke(const char *cwd) {
    skill_result *listing = calloc(1, sizeof(*listing));
    if (ha_installed_skill_list((const uint8_t *)cwd, strlen(cwd), NULL, NULL) != 1) return 1;
    if (ha_installed_skill_list(NULL, 0, receive_skill, listing) != 2) return 2;
    if (ha_installed_skill_read((const uint8_t *)cwd, strlen(cwd),
            NULL, 0, receive_skill, listing) != 2) return 3;
    static const uint8_t invalid_utf8[] = {'/', 0xff};
    static const uint8_t embedded_nul[] = {'/', 0, 'a'};
    if (ha_installed_skill_list(invalid_utf8, sizeof(invalid_utf8),
            receive_skill, listing) != 2
            || ha_installed_skill_list(embedded_nul, sizeof(embedded_nul),
                receive_skill, listing) != 2
            || ha_installed_skill_list((const uint8_t *)"relative", 8,
                receive_skill, listing) != 2
            || ha_installed_skill_list((const uint8_t *)cwd, 32769,
                receive_skill, listing) != 2
            || atomic_load(&listing->terminal) != 0) return 10;
    if (ha_installed_skill_list((const uint8_t *)cwd, strlen(cwd),
            receive_skill, listing) != 0 || !await_result(listing)) return 4;
    if (atomic_load(&listing->terminal) != 1 || listing->terminal_count != 1
            || listing->items != 1 || listing->source != 2 || listing->invalid
            || !listing->identity || listing->instructions[0] != '\0'
            || listing->warnings < 1) return 5;

    skill_result *reading = calloc(1, sizeof(*reading));
    if (ha_installed_skill_read((const uint8_t *)cwd, strlen(cwd),
            (const uint8_t *)listing->identity, strlen(listing->identity),
            receive_skill, reading) != 0 || !await_result(reading)) return 6;
    if (atomic_load(&reading->terminal) != 1 || reading->terminal_count != 1
            || reading->items != 1 || reading->invalid
            || !strstr(reading->instructions, "Fixture instructions")
            || strcmp(reading->identity, listing->identity) != 0) return 7;

    skill_result *missing = calloc(1, sizeof(*missing));
    if (ha_installed_skill_read((const uint8_t *)cwd, strlen(cwd),
            (const uint8_t *)cwd, strlen(cwd), receive_skill, missing) != 0
            || !await_result(missing)) return 8;
    if (atomic_load(&missing->terminal) != -2 || missing->terminal_count != 1
            || missing->items != 0) return 9;
    free(listing->identity);
    free(listing->instructions);
    free(reading->identity);
    free(reading->instructions);
    free(listing);
    free(reading);
    free(missing);
    return 0;
}
