GLOBAL _main
        EXTERN _printf

        %include "printmacro.asm"

        SECTION .data
        hellostr        db      'Hello, there!', `\n`, 0

        SECTION .text
_main:
        print0  hellostr, 0

        mov     rax, 0
        ret
