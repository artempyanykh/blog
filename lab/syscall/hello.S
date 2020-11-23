    .global _start

    .text

_start:
    mov $1, %rax
    mov $1, %rdi
    mov $hello_msg, %rsi
    mov $hello_msg_len, %rdx
    syscall
    jmp exit

exit:
    mov $60, %rax
    mov $42, %rdi
    syscall

hello_msg:
    .string "Hello, world!\n"

hello_msg_len = . - hello_msg
