.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl TestCall_zdtrModule4_bytes
.type TestCall_zdtrModule4_bytes, @object
TestCall_zdtrModule4_bytes:
	.asciz "main"
.section .data
.align 8
.align 1
.globl TestCall_zdtrModule3_closure
.type TestCall_zdtrModule3_closure, @object
TestCall_zdtrModule3_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	TestCall_zdtrModule4_bytes
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl TestCall_zdtrModule2_bytes
.type TestCall_zdtrModule2_bytes, @object
TestCall_zdtrModule2_bytes:
	.asciz "TestCall"
.section .data
.align 8
.align 1
.globl TestCall_zdtrModule1_closure
.type TestCall_zdtrModule1_closure, @object
TestCall_zdtrModule1_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	TestCall_zdtrModule2_bytes
.section .data
.align 8
.align 1
.globl TestCall_zdtrModule_closure
.type TestCall_zdtrModule_closure, @object
TestCall_zdtrModule_closure:
	.quad	ghczmprim_GHCziTypes_Module_con_info
	.quad	TestCall_zdtrModule3_closure+1
	.quad	TestCall_zdtrModule1_closure+1
	.quad	3
.section .text
.align 8
.align 8
	.quad	4294967301
	.quad	0
	.long	14
	.long	0
.globl TestCall_testCall_info
.type TestCall_testCall_info, @object
TestCall_testCall_info:
.Lc1dx:
	leaq -8(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1dE
.Lc1dF:
	movq $c1du_info,-8(%rbp)
	movq %r14,%rbx
	addq $-8,%rbp
	testb $7,%bl
	jne .Lc1du
.Lc1dv:
	jmp *(%rbx)
.Lc1dI:
	movq $16,904(%r13)
	jmp stg_gc_unpt_r1
.align 8
	.quad	0
	.long	30
	.long	0
c1du_info:
.Lc1du:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .Lc1dI
.Lc1dH:
	movq 7(%rbx),%rax
	incq %rax
	movq $ghczmprim_GHCziTypes_Izh_con_info,-8(%r12)
	movq %rax,(%r12)
	leaq -7(%r12),%rbx
	addq $8,%rbp
	jmp *(%rbp)
.Lc1dE:
	movl $TestCall_testCall_closure,%ebx
	jmp *-8(%r13)
	.size TestCall_testCall_info, .-TestCall_testCall_info
.section .data
.align 8
.align 1
.globl TestCall_testCall_closure
.type TestCall_testCall_closure, @object
TestCall_testCall_closure:
	.quad	TestCall_testCall_info
.section .note.GNU-stack,"",@progbits
.ident "GHC 8.6.4"


