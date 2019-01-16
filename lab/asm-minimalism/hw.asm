GLOBAL _main
        EXTERN _printf

        SECTION .data
        helloworldstr   db      'Hello World!', 0xA, 0

        SECTION .text
_main:
        mov     rdi, helloworldstr
        sub     rsp, 8
        xor     al, al
        call    _printf
        add     rsp, 8

        mov     rax, 0
        ret
