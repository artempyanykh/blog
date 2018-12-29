SECTION .text
        GLOBAL _main
_main:
        extern _printf
        sub     rsp, 8          ; 16 bytes align the stack

        mov     rdi, single_number_fmt
        mov     rsi, 42
        xor     rax, rax
        call    _printf

        add     rsp, 8          ; get back to the old esp before returning
        mov     rax, 11
        ret

        SECTION .data
        particular_number db 42
        single_number_fmt db '%d', 0xA, 0
        single_number_len equ $ - single_number_fmt
