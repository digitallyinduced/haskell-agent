#ifndef _WIN32
#include <sys/ioctl.h>
#include <unistd.h>
#endif

/* Read terminal dimensions without sending a cursor query or consuming input. */
int agent_cli_terminal_size(int *rows, int *columns) {
#ifndef _WIN32
    struct winsize size;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &size) == 0
            && size.ws_row > 0 && size.ws_col > 0) {
        *rows = size.ws_row;
        *columns = size.ws_col;
        return 1;
    }
#endif
    return 0;
}
