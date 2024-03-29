#include <stddef.h>
#include <stdint.h>

static inline void sys_write(int file, char* buffer, size_t size) {
    __asm__("movq $1, %%rax\n\t"
            "movl %[File], %%edi\n\t"
            "movq %[Buffer], %%rsi\n\t"
            "movq %[Size], %%rdx\n\t"
            "syscall"
            :
            : [File] "r"(file), [Buffer] "q"(buffer), [Size] "q"(size)
            : "rax", "edi", "rsi", "rdx");
}

void dump(uint64_t x) {
    char buffer[32];
    size_t cursor = sizeof buffer - 1;
    buffer[cursor] = '\n';
    cursor -= 1;
    size_t buffer_size = 1;

    do {
        buffer[cursor] = x % 10 + '0';
        cursor -= 1;
        buffer_size += 1;
        x /= 10;
    } while (x != 0);

    sys_write(1, buffer + cursor + 1, buffer_size);
}
