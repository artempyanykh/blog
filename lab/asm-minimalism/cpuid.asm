section .data
        cpumanfmt       db      '%s', 0xA, 0
        cpufeatfmt      db      '%d %d', 0xA, 0

        section .text
        global _main
        extern _printf

_main:
        push    rbp
        mov     eax, 0
        cpuid

        mov     rbp, rsp
        sub     rsp, 16

        mov     [rsp], ebx
        mov     [rsp + 4], edx
        mov     [rsp + 8], ecx
        mov     [rsp + 12], byte 0



        mov     rdi, cpumanfmt
        mov     rsi, rsp

        xor     al, al
        call    _printf

        add     rsp, 16

        mov     eax, 1
        cpuid
        mov     [rbp - 4], ecx
        mov     [rbp - 8], ebx
        sub     rsp, 16
        mov     rdi, cpufeatfmt
        mov     rsi, [rbp - 4]
        mov     rdx, [rbp - 8]
        xor     al, al
        call    _printf
        add     rsp, 16

        pop     rbp

        ret
