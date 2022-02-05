#include <stdint.h>
#include <unistd.h>

void dump(uint64_t x)
{
    char buffer[32];
    size_t cursor = sizeof buffer - 1;
    buffer[cursor] = '\n';
    cursor -= 1;
    size_t buffer_size = 1;

    do
    {
        buffer[cursor] = x % 10 + '0';
        cursor -= 1;
        buffer_size += 1;
        x /= 10;
    } while (x != 0);

    write(STDOUT_FILENO, buffer + cursor + 1, buffer_size);
}
