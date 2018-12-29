SECTION .data
        fizz_str        db      'Fizz', 0
        buzz_str        db      'Buzz', 0
        fizzbuzz_str    db      'FizzBuzz', 0
        str_fmt         db      '%s', 0xA, 0
        num_fmt         db      '%d', 0xA, 0

        SECTION .text
        global _main
        extern _printf

_main:
        push    rbp
        mov     rbp, rsp

        push    rbx
        mov     bx, 0

        sub     rsp, 8
.loop:
        add     bx, 1
        cmp     bx, 100
        jg      .done

        mov     ax, bx
        mov     dh, 15
        div     dh
        cmp     ah, 0
        jne     .check3
        call    print_fizzbuzz
        jmp     .loop
.check3:
        mov     ax, bx
        mov     dh, 3
        div     dh
        cmp     ah, 0
        jne     .check5
        call    print_fizz
        jmp     .loop

.check5:
        mov     ax, bx
        mov     dh, 5
        div     dh
        cmp     ah, 0
        jne     .just_num
        call    print_buzz
        jmp     .loop

.just_num:
        movzx   rdi, bx
        call    print_num
        jmp     .loop

.done:
        add     rsp, 8
        pop     rbx
        pop     rbp
        ret

print_fizz:
        mov     rdi, str_fmt
        mov     rsi, fizz_str
        sub     rsp, 8
        call    _printf
        add     rsp, 8
        ret

print_buzz:
        mov     rdi, str_fmt
        mov     rsi, buzz_str
        sub     rsp, 8
        call    _printf
        add     rsp, 8
        ret

print_fizzbuzz:
        mov     rdi, str_fmt
        mov     rsi, fizzbuzz_str
        sub     rsp, 8
        call    _printf
        add     rsp, 8
        ret

print_num:
        mov     rsi, rdi
        mov     rdi, num_fmt
        sub     rsp, 8
        call    _printf
        add     rsp, 8
        ret
