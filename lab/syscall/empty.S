    .global _start

    .text

_start:
    mov $60, %rax
    mov $42, %rdi
    syscall
