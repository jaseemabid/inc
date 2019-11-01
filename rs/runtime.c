#include <assert.h>
#include <fcntl.h>
#include <inttypes.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

// The default calling convention used by GCC on x86-64 seems to be System V
// AMD64 ABI, in which arguments are passed in the registers RDI, RSI, RDX, RCX,
// R8, R9 and the return value is passed back in RAX. See x86 module docs for
// more details.
extern int64_t init(int64_t*) __attribute__((noinline));

const int64_t numtag  = 0;
const int64_t booltag = 1;
const int64_t chartag = 2;
const int64_t pairtag = 3;
const int64_t niltag  = 4;
const int64_t strtag  = 5;
const int64_t symtag  = 6;
const int64_t vectag  = 7;

const int64_t shift   = 3;
const int64_t mask    = 7;

const int64_t bool_f  = (0 << shift) | booltag;
const int64_t bool_t  = (1 << shift) | booltag;

/*
  Stdlib

  Arguments and return values are immediate encoded
*/

int64_t string_length(int64_t val);
int64_t symbol_eq(int64_t a, int64_t b);

/*
  Internal definitions

  A value of type `int64_t` is usually an immediate encoded value, and a pointer
  type usually refers to raw data.
*/

char *get_str(int64_t val);
char *get_sym_name(int64_t val);
char get_char(int64_t val);
int64_t get_num(int64_t val);
int64_t get_pair_car(int64_t val);
int64_t get_pair_cdr(int64_t val);
int64_t get_strlen(int64_t val);
int64_t get_sym_id(int64_t val);
int64_t get_sym_len(int64_t val);
int64_t get_vec_len(int64_t val);
int64_t get_vec_nth(int64_t val, int n);

/* IO helpers */

int64_t rt_open_write(int64_t fname);
int64_t writeln(int64_t str, int64_t port);

/* --- */

void print(int64_t val, bool nested) {

    if ((val & mask) == numtag) {
        printf("%" PRId64, get_num(val));

    } else if ((val & mask) == chartag) {
        char c = get_char(val);

        if (c == '\t')
            printf("#\\tab");
        else if (c == '\n')
            printf("#\\newline");
        else if (c == '\r')
            printf("#\\return");
        else if (c == ' ')
            printf("#\\space");
        else
            printf("#\\%c", c);

    } else if (val == bool_t) {
        printf("#t");

    } else if (val == bool_f) {
        printf("#f");

    } else if (val == niltag) {
        printf("()");

    } else if ((val & mask) == pairtag) {
        int64_t car = get_pair_car(val);
        int64_t cdr = get_pair_cdr(val);

        if (!nested) printf("(");

        print(car, false);

        if (cdr != niltag) {
            if ((cdr & mask) != pairtag) {
                printf(" . ");
                print(cdr, false);
            } else {
                printf(" ");
                print(cdr, true);
            }
        }
        if (!nested) printf(")");

    } else if ((val & mask) == strtag) {
        // A string in memory is a pair of length and a pointer to a blob of
        // bytes - ideally guaranteed by the compiler to be valid UTF-8. See
        // compiler module for documentation on the layout.
        printf("\"");
        fwrite(get_str(val), 1, get_strlen(val), stdout);
        printf("\"");

    } else if ((val & mask) == symtag) {
        printf("'");
        fwrite(get_sym_name(val), 1, get_sym_len(val), stdout);

    } else if ((val & mask) == vectag) {
        printf("[");
        for (int i = 0; i < get_vec_len(val); i++) {
            print(get_vec_nth(val, i), false);
            if (i != get_vec_len(val) - 1) {
                printf(" ");
            }
        }
        printf("]");
    }

    else {
        printf("Runtime Error: unknown value returned: `%" PRId64 " `\n", val);
    }
}

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

    free(heap);
}

/*
  Stdlib

  Arguments and return values are immediate encoded
*/

int64_t string_length(int64_t val) {
    int64_t *p = (int64_t *)(val - strtag);
    int64_t len = *(p + 0);

    return len * 8;
}

int64_t symbol_eq(int64_t a, int64_t b) {
    return ((a == b) && (a & mask) == symtag) ? bool_t : bool_f;
}

/*
  Internal definitions

  A value of type `int64_t` is usually an immediate encoded value, and a pointer
  type usually refers to raw data.
*/

/* Get raw values from immediate encoded values */
int64_t get_num(int64_t val) {
    assert((val & mask) == numtag);

    return val >> shift;
}

char get_char(int64_t val) {
    assert((val & mask) == chartag);

    return val >> shift;
}

int64_t get_pair_car(int64_t val) {
    assert((val & mask) == pairtag);

    int64_t *p = (int64_t *)(val - pairtag);
    return *p;
}

int64_t get_pair_cdr(int64_t val) {
    assert((val & mask) == pairtag);

    int64_t *p = (int64_t *)(val - pairtag);
    return *(p + 1);
}

int64_t get_sym_id(int64_t val) {
    assert((val & mask) == symtag);

    int64_t *p = (int64_t *)(val - symtag);
    return *p;
}

int64_t get_sym_len(int64_t val) {
    assert((val & mask) == symtag);

    int64_t *p = (int64_t *)(val - symtag);
    return *(p + 1);
}

char *get_sym_name(int64_t val) {
    assert((val & mask) == symtag);

    int64_t *p = (int64_t *)(val - symtag);
    return (char *)(p + 2);
}

char *get_str(int64_t val) {
    assert((val & mask) == strtag);

    int64_t *p = (int64_t *)(val - strtag);
    int64_t *str = p + 1;

    return (char *)str;
}

int64_t get_strlen(int64_t val) {
    assert((val & mask) == strtag);

    int64_t *p = (int64_t *)(val - strtag);
    return *p;
}

int64_t get_vec_len(int64_t val) {
    assert((val & mask) == vectag);

    int64_t *p = (int64_t *)(val - vectag);
    return *p;
}

int64_t get_vec_nth(int64_t val, int n) {
    int64_t *p = (int64_t *)(val - vectag);
    return *(p + n + 1);
}

/* IO helpers */

int64_t rt_open_write(int64_t fname) {
    char *name = get_str(fname);

    int fd = open(name, O_WRONLY | O_CREAT | O_TRUNC, 0640);
    return fd * 8;
}

int64_t writeln(int64_t str, int64_t port) {
    int64_t fd = get_num(get_vec_nth(port, 2));
    char *data = get_str(str);
    int len = get_strlen(str);

    write(fd, data, len);
    write(fd, "\n", 1);
    return niltag;
}
