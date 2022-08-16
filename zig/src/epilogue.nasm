    mov rax, SYS_EXIT
    mov rdi, 0
    syscall
segment .bss
mem: resb {d}
