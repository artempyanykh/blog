section .data
        msg     db      'Hello stack!', 0x0, 0xA
        len     equ     $ - msg

        section .text
        global _start

_start:
        ;; push rbp
        ;; mov rbp, rsp

        call _print

        ;; pop rbp
        ret

_print:
        mov rdi, 1
        mov rsi, msg
        mov rdx, len
        mov rax, 0x02000004
        syscall

        ret
