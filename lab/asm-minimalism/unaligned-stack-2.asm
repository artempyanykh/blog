GLOBAL _main
        EXTERN _printf

        %include "printmacro.asm"

        SECTION .data
        hellostr        db      'Hello, there!', `\n`, 0

        SECTION .text
_main:
        print0  hellostr, 8     ; 1st call to printf with properly aligned stack
        print0  hellostr, 0     ; (+) unaligned call to print0

        mov     rax, 0
        ret

GLOBAL _main
        EXTERN _printf

        %include "printmacro.asm"

        SECTION .data
        hellostr        db      'Hello, there!', `\n`, 0

        SECTION .text
_main:
        print0  hellostr, 8     ; 1st call to printf with properly aligned stack
        print0  hellostr, 0     ; (+) unaligned invocation of print0

        mov     rax, 0
        ret
