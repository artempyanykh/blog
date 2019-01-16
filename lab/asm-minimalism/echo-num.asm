GLOBAL _main
        EXTERN _printf
        EXTERN _strtol

        %include "printmacro.asm"

        SECTION .data
        usestr  db      'Usage: echo-num <number>', `\n`, 0
        nanstr  db      'Not a number', `\n`, 0
        numstr  db      'Your number is %d', `\n`, 0

        SECTION .text
_main:
        cmp     rdi, 1
        jle     .noargs

        push    rbp             ; (1)
        mov     rbp, rsp        ;

        lea     rax, [rsi + 8]  ; (2)
        mov     rdi, [rax]      ; rdi points to the first char of argv[1]
        call    parsenum

        cmp     rdx, 0
        jne     .invalid

        print1  numstr, rax, 0
        jmp     .fin

.invalid:
        print0  nanstr, 0

.fin:
        pop     rbp
        mov     rax, 0
        ret

.noargs:
        print0  usestr, 8
        mov     rax, 1
        ret

;;; Parameter 1 - address of a string to parse
;;; Returns:
;;;     rax - parsed number
;;;     rdx - 0 when OK, 1 when the string is not a number
parsenum:
        cmp     byte [rdi], 0
        je      .emptystr
        sub     rsp, 8          ; align stack

        mov     rsi, rsp        ; endptr on top of stack
        mov     rdx, 10         ; base 10 number
        call    _strtol

        mov     rdx, [rsp]      ; following 2 instructions dereference **endptr
        cmp     byte [rdx], 0   ; and check that it points to 0 (end of string)
        jne     .invalidstr     ; otherwise input had some non-digit character

        mov     rdx, 0          ; rdx = 0 since parsing finished successfully
        add     rsp, 8          ; restore rsp
        ret

.invalidstr:                    ; invalidstr does the same as emptystr, but needs
        add     rsp, 8          ; to restore the stack
.emptystr:
        mov     rax, 0
        mov     rdx, 1
        ret
