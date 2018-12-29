	GLOBAL main

	SECTION .data
	msg	db	'Hello, brave new world!', 0Ah

	SECTION .text
main:
;;; arguments are passed on the stack
;;; stack is 16 bytes aligned
;;; syscall number is in eax
	push	13
	push	msg
	push	1
	mov	eax, 4
	sub	esp, 4
	int	80h
	add	esp, 16

	ret
