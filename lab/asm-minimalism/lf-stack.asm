SECTION .data
        msg     db      'Hello', 0
        ln      equ     $ - msg

        SECTION .text
        global _main

_main:
        mov     rdi, 1
        mov     rsi, msg
        mov     rdx, ln
        mov     rax, 0x02000004
        sub     rsp, 8
        syscall
        add     rsp, 8

        mov     ax, 0xA
        push    ax
        mov     rdi, 1
        mov     rsi, rsp
        mov     rdx, 1
        mov     rax, 0x02000004
        sub     rsp, 6          ; align stack to 16 bytes
        syscall
        add     rsp, 6          ; undo stack changes
        pop     ax              ;

        mov     rax, 0
        ret
