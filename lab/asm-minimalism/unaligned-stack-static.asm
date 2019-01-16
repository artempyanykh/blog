GLOBAL _start
        EXTERN _printf

        %include "printmacro.asm"

        SECTION .data
        hellostr        db      'Hello, there!', `\n`, 0

        SECTION .text
_start:
        print0  hellostr, 0

        mov     rdi, 0
        mov     rax, 0x0200_0001
        syscall
