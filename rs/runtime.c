#include <assert.h>
#include <inttypes.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <unistd.h>
#include "inc.h"

// Explicitly link to the assembly entry point
extern int64_t init(int64_t*) __attribute__((noinline));

void set_handler(void (*handler)(int, siginfo_t *, void *)) {
    struct sigaction action;
    action.sa_flags = SA_SIGINFO;
    action.sa_sigaction = handler;

    if (sigaction(SIGSEGV, &action, NULL) == -1) {
        perror("sigsegv: sigaction");
        _exit(1);
    }
}

void handler(int signo, siginfo_t *info, __attribute__((unused)) void *extra) {
    printf("Program crashed due to unexpected error \n");
    printf("Signal number        : %d \n", signo);
    printf("SIGSEGV at address   : 0x%lx \n", (long)info->si_addr);
    abort();
}

int main() {
    FILE *debug = getenv("DEBUG") ? stderr : fopen("/dev/null", "w");
    fprintf(debug, "%s\n\n", "The glorious incremental compiler");

    set_handler(handler);

    int64_t r12, rsp;
    int64_t *heap = calloc(1024, 8);

    // Read current stack pointer into local variable for diagnostics
    asm("nop; movq %%rsp, %0" : "=r"(rsp));

    // Execute all of the generated ASM; this could return a value or segfault
    int64_t val = init(heap);

    // Copy the value of R12 into a local variable. The nop instruction makes it
    // easier to spot this in the generated asm
    asm("nop; movq %%r12, %0" : "=r"(r12));

    ptrdiff_t size = (uintptr_t)r12 - (uintptr_t)heap;

    fprintf(debug, "Stack base addr : %p\n", (void *)rsp);
    fprintf(debug, "Heap segment    : %p" "-> %p \n", (void *)heap, (void *)r12);
    fprintf(debug, "Heap size       : %td bytes \n", size);
    fprintf(debug, "Value in rax    : %" PRId64 " (0x%" PRIx64 ")", val, val);
    fprintf(debug, "\n\n");
    fflush(stdout);

    print(val, false);
    printf("\n");
    fflush(stdout);

    free(heap);
}
