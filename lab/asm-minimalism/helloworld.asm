GLOBAL _main
        EXTERN _printf          ; (1)

        SECTION .data
        helloworldstr   db      'Hello World!', `\n`, 0 ; (2)

        SECTION .text
_main:
        sub     rsp, 8                  ; (3.1)
        mov     rdi, helloworldstr      ; (3.2)
        xor     al, al                  ; (3.3)
        call    _printf
        add     rsp, 8                  ; (3.4)

        mov     rax, 0
        ret

GLOBAL _main
        EXTERN _printf

        SECTION .data
        helloworldstr   db      'Hello World!', `\n`, 0

        SECTION .text
_main:
        sub     rsp, 8
        mov     rdi, helloworldstr
        xor     al, al
        call    _printf
        add     rsp, 8

        mov     rax, 0
        ret
