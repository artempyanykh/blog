	global _main

	SECTION .data
	msg	db	"Hello, world!", 10
	.len	equ	$ - msg

	SECTION .text

_main:
;;; rdi, rsi, rdx, r10, r8, r9
;;; sycall number in rax register
	mov	eax, 0x02000004
	Mov	edi, 1
	;; mov	rsi, msg
	lea	rsi, [rel msg]
	mov	edx, msg.len
	syscall

	;; ret ;; use with dynamic linking
	mov	rax,	0x02000001
	mov	rdi,	0
	syscall
