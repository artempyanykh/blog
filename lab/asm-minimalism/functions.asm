;;; int strlen(char* msg)
;;; msg is in rdi, resulting len is in rax
strlen:
        mov     rax, rdi
strlen_loop:
        cmp     byte [rax], 0   ; zero byte?
        jz      strlen_fin
        inc     rax
        jmp     strlen_loop
strlen_fin:
        sub     rax, rdi
        ret

;;; void print(char* msg)
print:
        push    rbx
        mov     rbx, rdi
        call    strlen

        mov     rdi, 1          ; 1 arg = fd (1 stdout)
        mov     rsi, rbx        ; 2 arg = msg pointer
        mov     rdx, rax        ; 3 arg = msg len
        mov     rax, 0x02000004
        syscall

        pop     rbx
        ret
