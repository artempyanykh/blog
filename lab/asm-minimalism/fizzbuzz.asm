GLOBAL _main
        EXTERN  _printf
        EXTERN  _strtol

        %include "printmacro.asm"
        %include "parse.asm"

        SECTION .data
        usestr  db      'Usage: fizzbuzz <number>', `\n`, 0
        nanstr  db      'Not a number', `\n`, 0
        numstr  db      '%d', 0
        fizzstr db      'Fizz', 0
        buzzstr db      'Buzz', 0
        lf      db      `\n`, 0

        SECTION .text
_main:
        ;; The beginning is the same as in echo-num.asm
        cmp     rdi, 1
        jle     .noargs

        push    rbp
        mov     rbp, rsp

        lea     rax, [rsi + 8]
        mov     rdi, [rax]
        call    parsenum

        cmp     rdx, 0
        jne     .invalid
        ;; At this point n is parsed an is in rax register

        ;; Here starts the main logic of the routine.
        ;; Since registers r12 - r15 are calee-saved, they
        ;; need to be pushed onto stack before used.
        push    r13
        mov     r13, rax        ; save n in r13

        push    r12             ; r12 will serve as a counter
        mov     r12, 1

        push    r14             ; r14 will store r12 % 3

.checkStart:
        cmp     r12, r13
        jg      .fin

        ;; div instruction does signed division
        ;; when the divisor is in 64-bit register (rcx in our case)
        ;; rdx:rax get divided by it with quotient stored in rax and
        ;; remainder -- in rdx
        mov     rax, r12
        xor     rdx, rdx
        mov     rcx, 3
        div     rcx

        mov     r14, rdx        ; save r13 % 3 in r14 for further check in .checkNum
        cmp     rdx, 0
        jne     .check5
        print0  fizzstr, 8
        ;; after .check3 we *need* to fall-through to .check5
.check5:
        mov     rax, r12
        xor     rdx, rdx
        mov     rcx, 5
        div     rcx
        cmp     rdx, 0
        jne     .checkNum
        print0  buzzstr, 8
        jmp     .checkEnd       ; check5 is successful, we can go to .checkEnd

.checkNum:
        cmp     r14, 0          ; here r12 % 5 != 0, *but* r12 % 3 could be 0
        je      .checkEnd       ; thus we need to check the result of div 3
        print1  numstr, r12, 8

.checkEnd:
        print0  lf, 8
        inc     r12
        jmp     .checkStart

.fin:
        pop     r14
        pop     r12
        pop     r13
        pop     rbp
        mov     rax, 0
        ret

.noargs:
        print0  usestr, 8
        mov     rax, 1
        ret

.invalid:
        print0  nanstr, 0
        pop     rbp
        mov     rax, 1
        ret
