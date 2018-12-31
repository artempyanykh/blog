SECTION .text
        GLOBAL start
start:
        mov     rax, 0x0200_0001
        syscall
