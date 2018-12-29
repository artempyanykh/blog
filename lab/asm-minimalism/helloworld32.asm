	global _start


	SECTION .data
	msg	db	'Hello World!', 0Ah

	SECTION .text

_start:
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
