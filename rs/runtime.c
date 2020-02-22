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

// Turns out writing a signal handler that can handle a segfault due to stack
// overflow isn't that simple. See the rethinkdb blog for details.
//
// https://rethinkdb.com/blog/handling-stack-overflow-on-custom-stacks/
#ifdef __linux__
void set_handler(void (*handler)(int, siginfo_t *, void *)) {
    struct sigaction action;
    action.sa_flags = SA_SIGINFO|SA_STACK;
    action.sa_sigaction = handler;

    // By default, when a signal is delivered, its handler is called on the same
    // stack where the program was running. But if the signal is due to stack
    // overflow, then attempting to execute the handler will cause a second
    // segfault. Linux is smart enough not to send this segfault back to the
    // same signal handler, which would prevent an infinite cascade of
    // segfaults. Instead, in effect, the signal handler does not work.

    // To make it work, we have to provide an alternate stack to execute the
    // signal handler on. The system call to install this stack is called
    // sigaltstack. As a parameter, it takes a stack_t, which consists of a
    // pointer to the base of the stack, the size of the stack, and some flags
    // that aren’t relevant for our purposes.
    stack_t segv_stack;
    int SEGV_STACK_SIZE = 4096;
    segv_stack.ss_sp = valloc(SEGV_STACK_SIZE);
    segv_stack.ss_flags = 0;
    segv_stack.ss_size = SEGV_STACK_SIZE;
    sigaltstack(&segv_stack, NULL);

    if (sigaction(SIGSEGV, &action, NULL) == -1) {
        perror("sigsegv: sigaction");
        _exit(1);
    }
}

void handler(int signo, siginfo_t *info, __attribute__((unused)) void *extra) {
    printf("Segmentation fault due to invalid memory access \n");
    printf("Signal number        : %d \n", signo);
    printf("SIGSEGV at address   : 0x%lx \n", (long)info->si_addr);
    abort();
}
#endif

int main() {
    FILE *debug = getenv("DEBUG") ? stderr : fopen("/dev/null", "w");
    fprintf(debug, "%s\n\n", "The glorious incremental compiler");


    #ifdef __linux__
    set_handler(handler);
    #endif

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
