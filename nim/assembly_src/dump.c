#include <stdint.h>
#include <stdio.h>

void dump(uint64_t x) {
  char buf[32];
  size_t buf_sz = 1;
  buf[sizeof(buf) - buf_sz] = '\n';
  do {
    buf[sizeof(buf) - buf_sz - 1] = x % 10 + '0';
    buf_sz++;
    x /= 10;
  } while (x);
  fwrite(&buf[sizeof(buf) - buf_sz], 1, buf_sz, stdout);
}
