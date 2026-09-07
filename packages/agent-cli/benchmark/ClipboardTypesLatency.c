/* Conservative subprocess-overhead model for text-only bracketed pastes.
 * Baseline runs three real osascript processes, each returning false without
 * clipboard coercion. Thus this excludes (rather than invents) the additional
 * failed-coercion latency of the old implementation. Both paths return the
 * same "no image" result. Native inspection uses a private named pasteboard.
 */
#include <ApplicationServices/../Frameworks/HIServices.framework/Headers/Pasteboard.h>
#include <assert.h>
#include <spawn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

extern char **environ;
int agent_cli_pasteboard_may_contain_images(CFStringRef name);

static double milliseconds(clockid_t clock)
{
    struct timespec value;
    assert(clock_gettime(clock, &value) == 0);
    return value.tv_sec * 1000.0 + value.tv_nsec / 1000000.0;
}

static int baseline(void)
{
    for (int index = 0; index < 3; ++index) {
        char *arguments[] = {"osascript", "-e", "return", NULL};
        pid_t process;
        int status;
        assert(posix_spawn(&process, "/usr/bin/osascript", NULL, NULL,
                           arguments, environ) == 0);
        assert(waitpid(process, &status, 0) == process);
        assert(WIFEXITED(status) && WEXITSTATUS(status) == 0);
    }
    return 0;
}

static int compare(const void *left, const void *right)
{
    double first = *(const double *)left;
    double second = *(const double *)right;
    return (first > second) - (first < second);
}

int main(int count, char **arguments)
{
    unsigned int samples = count > 1 ? (unsigned int)atoi(arguments[1]) : 7;
    assert(samples >= 3 && samples <= 100);
    char name[128];
    snprintf(name, sizeof(name), "org.haskell-agent.clipboard-benchmark.%ld",
             (long)getpid());
    CFStringRef clipboard_name =
        CFStringCreateWithCString(NULL, name, kCFStringEncodingUTF8);
    PasteboardRef clipboard = NULL;
    assert(PasteboardCreate(clipboard_name, &clipboard) == noErr);
    const size_t sizes[] = {32, 1024, 65536, 32};
    volatile int checksum = 0;
    for (size_t index = 0; index < sizeof(sizes) / sizeof(sizes[0]); ++index) {
        size_t size = sizes[index];
        UInt8 *bytes = malloc(size);
        assert(bytes != NULL);
        memset(bytes, 'a', size);
        memcpy(bytes, "https://example.org/", 20);
        CFDataRef data = CFDataCreate(NULL, bytes, (CFIndex)size);
        free(bytes);
        assert(PasteboardClear(clipboard) == noErr);
        assert(PasteboardPutItemFlavor(clipboard, (PasteboardItemID)1,
                                      CFSTR("public.utf8-plain-text"),
                                      data, 0) == noErr);
        CFRelease(data);
        assert(agent_cli_pasteboard_may_contain_images(clipboard_name) == 0);
        double elapsed[2][100], cpu[2][100];
        for (unsigned int sample = 0; sample < samples; ++sample) {
            for (unsigned int order = 0; order < 2; ++order) {
                unsigned int method = (sample + order) % 2;
                double before_cpu = milliseconds(CLOCK_PROCESS_CPUTIME_ID);
                double before_elapsed = milliseconds(CLOCK_MONOTONIC);
                int result = method
                    ? agent_cli_pasteboard_may_contain_images(clipboard_name)
                    : baseline();
                elapsed[method][sample] =
                    milliseconds(CLOCK_MONOTONIC) - before_elapsed;
                cpu[method][sample] =
                    milliseconds(CLOCK_PROCESS_CPUTIME_ID) - before_cpu;
                assert(result == 0);
                checksum += result + 1;
            }
        }
        for (unsigned int method = 0; method < 2; ++method) {
            qsort(elapsed[method], samples, sizeof(double), compare);
            qsort(cpu[method], samples, sizeof(double), compare);
            printf("%s bytes=%zu samples=%u elapsed-ms=%.6f cpu-ms=%.6f\n",
                   method ? "native-metadata" : "three-process-baseline",
                   size, samples, elapsed[method][samples / 2],
                   cpu[method][samples / 2]);
        }
    }
    assert(PasteboardClear(clipboard) == noErr);
    CFRelease(clipboard);
    CFRelease(clipboard_name);
    printf("checksum=%d\n", checksum);
    return 0;
}
