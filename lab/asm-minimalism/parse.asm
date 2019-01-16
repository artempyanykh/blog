;;; Parameter 1 - address of a string to parse
;;; Returns:
;;;     rax - parsed number
;;;     rdx - 0 when OK, 1 when the string is not a number
parsenum:
        cmp     byte [rdi], 0
        je      .emptystr
        sub     rsp, 8          ; align stack

        mov     rsi, rsp        ; **endptr on top of stack
        mov     rdx, 10         ; base 10 number
        call    _strtol

        mov     rdx, [rsp]      ; following 2 instructions dereference **endptr
        cmp     byte [rdx], 0   ; and check that it points to 0 (end of string)
        jne     .invalidstr     ; otherwise there is some non-digit character

        mov     rdx, 0          ; rdx = 0 since parsing finished successfully
        add     rsp, 8          ; restore rsp
        ret

.invalidstr:                    ; invalidstr does the same as emptystr, but needs
        add     rsp, 8          ; to restore the stack
.emptystr:
        mov     rax, 0
        mov     rdx, 1
        ret
