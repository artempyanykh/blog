	.file	"cmemset.c"
	.text
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"%s"
	.text
	.globl	main
	.type	main, @function
main:
.LFB11:
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	subq	$48, %rsp
	.cfi_def_cfa_offset 64
	movl	$40, %ebx
	movq	%fs:(%rbx), %rax
	movq	%rax, 40(%rsp)
	xorl	%eax, %eax
	movq	%rsp, %rsi
	movabsq	$72340172838076673, %rdx
	movabsq	$72340172838076673, %rcx
	movq	%rdx, 1(%rsp)
	movq	%rcx, 9(%rsp)
	movabsq	$72340172838076673, %rdi
	movq	%rdi, 17(%rsi)
	movl	$16843009, 25(%rsi)
	movw	$257, 29(%rsi)
	movb	$1, 31(%rsi)
	leaq	.LC0(%rip), %rdi
	movl	$0, %eax
	call	printf@PLT
	movq	40(%rsp), %rdx
	xorq	%fs:(%rbx), %rdx
	jne	.L4
	movl	$1, %eax
	addq	$48, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 16
	popq	%rbx
	.cfi_def_cfa_offset 8
	ret
.L4:
	.cfi_restore_state
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE11:
	.size	main, .-main
	.ident	"GCC: (GNU) 8.2.1 20181127"
	.section	.note.GNU-stack,"",@progbits
