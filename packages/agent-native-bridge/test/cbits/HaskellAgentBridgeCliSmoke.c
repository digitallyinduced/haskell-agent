#include "HaskellAgentBridge.h"

#include <string.h>

int main(int argc, char **argv) {
    if (argc == 2 && strcmp(argv[1], "--check-entrypoint-arguments") == 0) {
        char *missing_name[] = {NULL, NULL};
        char *missing_terminator[] = {"agent-cli", "unexpected"};
        if (ha_cli_main(0, NULL) != 64) return 1;
        if (ha_cli_main(1, NULL) != 64) return 2;
        if (ha_cli_main(1, missing_name) != 64) return 3;
        if (ha_cli_main(1, missing_terminator) != 64) return 4;
        return 0;
    }
    if (argc == 2 && strcmp(argv[1], "--check-runtime-conflict") == 0) {
        char *arguments[] = {"agent-cli", "--version", NULL};
        if (ha_runtime_init() != 0) return 1;
        if (ha_cli_main(2, arguments) != 70) return 2;
        ha_runtime_exit();
        return 0;
    }
    return ha_cli_main(argc, argv);
}
