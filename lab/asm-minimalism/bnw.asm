GLOBAL _main

        SECTION .data
        msg db 'Hello, brave new world! Repent and rejoice!', 0xA

        SECTION .text
_main:
;;; arguments are passed on the stack
;;; stack is 16 bytes aligned
;;; syscall number is in eax
        mov     ebx, msg
        mov     eax, msg
        jmp     calclen
printmsg:
        sub     eax, ebx        ; get the len into eax
        push    eax             ; msg len
        push    msg             ; pointer to msg
        push    1               ; stdout
        mov     eax, 4
        sub     esp, 4
        int     80h
        add     esp, 16

        ret

calclen:
        cmp     byte [eax], 0   ; zero byte?
        jz      printmsg
        inc     eax
        jmp     calclen
