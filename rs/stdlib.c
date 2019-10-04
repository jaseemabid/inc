#include <inttypes.h>

int64_t string_length(int64_t val) {

    int64_t *p = (int64_t *)(val - 5);
    int64_t len = *(p + 0);

    return len * 8;
}
