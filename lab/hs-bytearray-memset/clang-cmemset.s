	.text
	.file	"cmemset.c"
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	4               # -- Begin function main
.LCPI0_0:
	.zero	16,1
	.text
	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movq	%fs:40, %rax
	movq	%rax, 32(%rsp)
	movaps	.LCPI0_0(%rip), %xmm0   # xmm0 = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
	movups	%xmm0, 16(%rsp)
	movups	%xmm0, 1(%rsp)
	leaq	.L.str(%rip), %rdi
	movq	%rsp, %rsi
	xorl	%eax, %eax
	callq	printf@PLT
	movq	%fs:40, %rax
	cmpq	32(%rsp), %rax
	jne	.LBB0_2
# %bb.1:
	movl	$1, %eax
	addq	$40, %rsp
	.cfi_def_cfa_offset 8
	retq
.LBB0_2:
	.cfi_def_cfa_offset 48
	callq	__stack_chk_fail@PLT
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"%s"
	.size	.L.str, 3


	.ident	"clang version 7.0.1 (tags/RELEASE_701/final)"
	.section	".note.GNU-stack","",@progbits
	.addrsig
	.addrsig_sym __stack_chk_fail
