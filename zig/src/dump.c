#include <stddef.h>
#include <stdint.h>

long write(int fd, const void *buf, size_t count) {
  // x86_64 assembly Linux write syscall
  asm volatile("syscall\n"
               "ret\n"
               :
               : "a"(1), "D"(fd), "S"(buf), "d"(count));
  // unreachable
  return 0;
}

void dump(uint64_t value) {
  char buf[32];
  size_t bufSize = 1;
  buf[sizeof(buf) - bufSize] = '\n';

  do {
    buf[sizeof(buf) - bufSize - 1] = '0' + (value % 10);
    value /= 10;
    ++bufSize;
  } while (value > 0);
  write(1, buf + sizeof(buf) - bufSize, bufSize);
}
