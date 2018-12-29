SECTION .text
        GLOBAL _main

_main:
        mov     rdi, 42         ; return code
        mov     rax, 0x02000001 ; exit syscall on 64bit Macho
exit:
        syscall
