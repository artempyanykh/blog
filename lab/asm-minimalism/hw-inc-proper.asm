%include 'functions.asm'

        GLOBAL _main

        SECTION .data
        msg1 db 'Hello, reusable world!', 0xA, 0x0
        msg2 db 'I was printed using included subroutines!', 0xA, 0x0

        SECTION .text
_main:
        lea     rdi, [rel msg1]
        call    print

        lea     rdi, [rel msg2]
        call    print

        ret
