#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#define BOOL 1

#define CHAR 2

#define FALSE ((0 << SHIFT) | BOOL)

#define MASK 7

#define NIL 4

#define NUM 0

#define PAIR 3

#define SHIFT 3

#define STR 5

#define SYM 6

#define TRUE ((1 << SHIFT) | BOOL)

#define VEC 7

#define WORDSIZE 8

int64_t car(int64_t val);

int64_t cdr(int64_t val);

void print(int64_t val, bool nested);

int64_t rt_current_error_port(void);

int64_t rt_current_input_port(void);

int64_t rt_current_output_port(void);

/**
 * Open a file for reading return the immediate encoded file descriptor
 * Fails if file doesn't exist already
 */
int64_t rt_open_read(int64_t fname);

/**
 * Open a file for writing and return the immediate encoded file descriptor
 * Creates file if it doesn't exist already
 */
int64_t rt_open_write(int64_t fname);

/**
 * Read string from a port object
 */
int64_t rt_read(int64_t port);

/**
 * Write a string object to a port
 */
int64_t rt_write(int64_t data, int64_t port);

uintptr_t string_length(int64_t val);

int64_t symbol_eq(int64_t a, int64_t b);
