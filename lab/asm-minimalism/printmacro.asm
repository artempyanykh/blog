;;; Prints a string to stdout.
;;; Parameter 1 -- a pointer to the string to print.
;;; Parameter 2 -- a constant sub to align the stack.
        %macro  print0 2
        sub     rsp, %2
        mov     rdi, %1
        xor     al, al
        call    _printf
        add     rsp, %2
        %endmacro

;;; Prints a string with an integer pattern inside.
;;; Parameter 1 -- a pointer to the format string.
;;; Parameter 2 -- a number to print.
;;; Parameter 3 -- a constant sub to align the stack.
        %macro  print1 3
        sub     rsp, %3
        mov     rdi, %1
        mov     rsi, %2
        xor     al, al
        call    _printf
        add     rsp, %3
        %endmacro
