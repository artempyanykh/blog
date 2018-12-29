GLOBAL _main

        SECTION .data
        msg db 'Hello, brave new world! Repent and rejoice!', 0xA

        SECTION .text
_main:
;;; rdi, rsi, rdx, r10, r8, r9
;;; sycall number in rax register
        lea     rbx, [rel msg]
        mov     rdi, rbx
        call    strlen
        mov     rdi, 1          ; stdout
        mov     rsi, rbx        ; pointer to msg
        mov     rdx, rax        ; msg len
        mov     rax, 0x02000004 ;
        syscall

        ret

strlen: ; a pointer to a string is in rdi, return len is in rax
        mov     rax, rdi
strlen_loop:
        cmp     byte [rax], 0   ; zero byte?
        jz      strlen_fin
        inc     rax
        jmp     strlen_loop
strlen_fin:
        sub     rax, rdi
        ret
