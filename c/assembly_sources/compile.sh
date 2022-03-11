#!/bin/sh

clang -O2 -march=native -mtune=native -fno-asynchronous-unwind-tables -fno-stack-protector dump.c -c -o dump.o
objconv -fnasm dump.o
