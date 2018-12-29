section .data
        num_fmt db      '%d', 0xA, 0

        SECTION .text
        global  _main
        extern  _printf
        extern  _strtol

_main:
;;; prologue
        push    rbp
        mov     rbp, rsp

        push    r12
        push    r13
        push    r14
        push    rbx

;;; body
        mov     r12, rdi        ; get a number of passed args in r12
        mov     r14, rsi        ; get an array pointer in r14

        mov     rbx, 0          ; init sum
        mov     r13, 1          ; init counter

.do_sum:
        cmp     r13, r12
        jge     .done_sum        ;

        mov     rbp, rsp     ; save rsp
        and     rsp, 0xff_ff_ff_ff_ff_ff_ff_f0 ; 16-byte align stack before calling atoi

        mov     rdi, [r14 + 8*r13] ; pointer to string
        call    str_to_base10
        add     rbx, rax        ; add a converted from string int value to accumulator

        mov     rsp, rbp        ; restore rsp
        add     r13, 1
        jmp     .do_sum

.done_sum:
        mov     rdi, num_fmt
        mov     rsi, rbx
        xor     rax, rax
        ;; align stack
        mov     rbp, rsp
        and     rsp,    0xff_ff_ff_ff_ff_ff_ff_f0 ; 16-byte align stack before calling printf
        call    _printf
        ;; restore stack
        mov     rsp, rbp

;;; epilogue
        pop     rbx
        pop     r14
        pop     r13
        pop     r12

        pop     rbp
        ret

str_to_base10:
        mov     rsi, 0
        mov     rdx, 10
        sub     rsp, 8
        call    _strtol
        add     rsp, 8
        ret
