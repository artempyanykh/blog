	.file	"cmemcpy.c"
	.text
	.globl	smallcpy
	.type	smallcpy, @function
smallcpy:
.LFB0:
	.cfi_startproc
	movq	(%rsi), %rax
	movq	%rax, (%rdi)
	movq	8(%rsi), %rax
	movq	%rax, 8(%rdi)
	movq	16(%rsi), %rax
	movq	%rax, 16(%rdi)
	movl	24(%rsi), %eax
	movl	%eax, 24(%rdi)
	movzwl	28(%rsi), %eax
	movw	%ax, 28(%rdi)
	movzbl	30(%rsi), %eax
	movb	%al, 30(%rdi)
	ret
	.cfi_endproc
.LFE0:
	.size	smallcpy, .-smallcpy
	.ident	"GCC: (GNU) 8.2.1 20181127"
	.section	.note.GNU-stack,"",@progbits
