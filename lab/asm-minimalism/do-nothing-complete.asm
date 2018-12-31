SECTION .text
        GLOBAL start            ; start is the default name of the entry point
start:
        mov     rdi, 42         ; first parameter "exit code" = 42
        mov     rax, 0x0200_0001 ; exit syscall number in rax = 0x02000000 + 1
        syscall
