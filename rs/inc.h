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

/**
 * A scheme object
 */
typedef struct {
  int64_t _0;
} Object;

Object car(Object val);

Object cdr(Object val);

void print(Object val, bool nested);

Object rt_current_error_port(void);

Object rt_current_input_port(void);

Object rt_current_output_port(void);

/**
 * Open a file for reading return the immediate encoded file descriptor
 * Fails if file doesn't exist already
 */
Object rt_open_read(Object fname);

/**
 * Open a file for writing and return the immediate encoded file descriptor
 * Creates file if it doesn't exist already
 */
Object rt_open_write(Object fname);

/**
 * Read string from a port object
 */
Object rt_read(Object port);

/**
 * Write a string object to a port
 */
Object rt_write(Object data, Object port);

Object string_length(int64_t val);

int64_t symbol_eq(int64_t a, int64_t b);
