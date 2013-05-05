#include <stdio.h>

/**
 * Helper functions for stdout and stderr, because I can't evaluate them
 * at compile/link time.
 */

FILE *PosixRedirect_stdout() {
    return stdout;
}

FILE *PosixRedirect_stderr() {
    return stderr;
}
