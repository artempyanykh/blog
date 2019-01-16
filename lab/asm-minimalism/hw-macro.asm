GLOBAL _main
        EXTERN _printf

        %include "printmacro.asm"

        SECTION .data
        hellostr        db      'Hello, there!', `\n`, 0
        todaynumstr     db      "Today's number is %d", `\n`, 0

        SECTION .text
_main:
        print0  hellostr, 8
        print1  todaynumstr, 42, 8

        mov     rax, 0
        ret
