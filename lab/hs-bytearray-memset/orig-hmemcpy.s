.section .text
.align 8
.align 8
	.quad	8589934597
	.quad	0
	.long	14
	.long	0
.globl CopyArray_smallCpy1_info
.type CopyArray_smallCpy1_info, @object
CopyArray_smallCpy1_info:
.Lc1jx:
	leaq -8(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc1jG
.Lc1jH:
	movq $c1ju_info,-8(%rbp)
	movq %r14,%rbx
	addq $-8,%rbp
	testb $7,%bl
	jne .Lc1ju
.Lc1jv:
	jmp *(%rbx)
.Lc1jK:
	movq $64,904(%r13)
	jmp stg_gc_unpt_r1
.align 8
	.quad	0
	.long	30
	.long	0
c1ju_info:
.Lc1ju:
	addq $64,%r12
	cmpq 856(%r13),%r12
	ja .Lc1jK
.Lc1jJ:
	movq 7(%rbx),%rax
	movq $stg_ARR_WORDS_info,-56(%r12)
	movq $32,-48(%r12)
	leaq -56(%r12),%rbx
	subq $8,%rsp
	leaq 16(%rbx),%rcx
	leaq 16(%rax),%rsi
	movq %rcx,%rdi
	movl $32,%edx
	xorl %eax,%eax
	call memcpy
	addq $8,%rsp
	movq $CopyArray_ByteArray_con_info,-8(%r12)
	movq %rbx,(%r12)
	leaq -7(%r12),%rbx
	addq $8,%rbp
	jmp *(%rbp)
.Lc1jG:
	movl $CopyArray_smallCpy1_closure,%ebx
	jmp *-8(%r13)
	.size CopyArray_smallCpy1_info, .-CopyArray_smallCpy1_info
.section .data
.align 8
.align 1
.globl CopyArray_smallCpy1_closure
.type CopyArray_smallCpy1_closure, @object
CopyArray_smallCpy1_closure:
	.quad	CopyArray_smallCpy1_info
.section .text
.align 8
.align 8
	.quad	8589934597
	.quad	0
	.long	14
	.long	0
.globl CopyArray_smallCpy_info
.type CopyArray_smallCpy_info, @object
CopyArray_smallCpy_info:
.Lc1jU:
	jmp CopyArray_smallCpy1_info
	.size CopyArray_smallCpy_info, .-CopyArray_smallCpy_info
.section .data
.align 8
.align 1
.globl CopyArray_smallCpy_closure
.type CopyArray_smallCpy_closure, @object
CopyArray_smallCpy_closure:
	.quad	CopyArray_smallCpy_info
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl CopyArray_zdtrModule4_bytes
.type CopyArray_zdtrModule4_bytes, @object
CopyArray_zdtrModule4_bytes:
	.asciz "main"
.section .data
.align 8
.align 1
.globl CopyArray_zdtrModule3_closure
.type CopyArray_zdtrModule3_closure, @object
CopyArray_zdtrModule3_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	CopyArray_zdtrModule4_bytes
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl CopyArray_zdtrModule2_bytes
.type CopyArray_zdtrModule2_bytes, @object
CopyArray_zdtrModule2_bytes:
	.asciz "CopyArray"
.section .data
.align 8
.align 1
.globl CopyArray_zdtrModule1_closure
.type CopyArray_zdtrModule1_closure, @object
CopyArray_zdtrModule1_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	CopyArray_zdtrModule2_bytes
.section .data
.align 8
.align 1
.globl CopyArray_zdtrModule_closure
.type CopyArray_zdtrModule_closure, @object
CopyArray_zdtrModule_closure:
	.quad	ghczmprim_GHCziTypes_Module_con_info
	.quad	CopyArray_zdtrModule3_closure+1
	.quad	CopyArray_zdtrModule1_closure+1
	.quad	3
.section .data
.align 8
.align 1
r14F_closure:
	.quad	ghczmprim_GHCziTypes_KindRepTyConApp_con_info
	.quad	ghczmprim_GHCziTypes_zdtcByteArrayzh_closure
	.quad	ghczmprim_GHCziTypes_ZMZN_closure+1
	.quad	3
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl CopyArray_zdtcByteArray2_bytes
.type CopyArray_zdtcByteArray2_bytes, @object
CopyArray_zdtcByteArray2_bytes:
	.asciz "ByteArray"
.section .data
.align 8
.align 1
.globl CopyArray_zdtcByteArray1_closure
.type CopyArray_zdtcByteArray1_closure, @object
CopyArray_zdtcByteArray1_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	CopyArray_zdtcByteArray2_bytes
.section .data
.align 8
.align 1
.globl CopyArray_zdtcByteArray_closure
.type CopyArray_zdtcByteArray_closure, @object
CopyArray_zdtcByteArray_closure:
	.quad	ghczmprim_GHCziTypes_TyCon_con_info
	.quad	CopyArray_zdtrModule_closure+1
	.quad	CopyArray_zdtcByteArray1_closure+1
	.quad	ghczmprim_GHCziTypes_krepzdzt_closure
	.quad	8298289852389378222
	.quad	4496013244284234214
	.quad	0
	.quad	3
.section .data
.align 8
.align 1
r16H_closure:
	.quad	ghczmprim_GHCziTypes_KindRepTyConApp_con_info
	.quad	CopyArray_zdtcByteArray_closure+1
	.quad	ghczmprim_GHCziTypes_ZMZN_closure+1
	.quad	3
.section .data
.align 8
.align 1
.globl CopyArray_zdtczqByteArray1_closure
.type CopyArray_zdtczqByteArray1_closure, @object
CopyArray_zdtczqByteArray1_closure:
	.quad	ghczmprim_GHCziTypes_KindRepFun_con_info
	.quad	r14F_closure+1
	.quad	r16H_closure+1
	.quad	3
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl CopyArray_zdtczqByteArray3_bytes
.type CopyArray_zdtczqByteArray3_bytes, @object
CopyArray_zdtczqByteArray3_bytes:
	.asciz "'ByteArray"
.section .data
.align 8
.align 1
.globl CopyArray_zdtczqByteArray2_closure
.type CopyArray_zdtczqByteArray2_closure, @object
CopyArray_zdtczqByteArray2_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	CopyArray_zdtczqByteArray3_bytes
.section .data
.align 8
.align 1
.globl CopyArray_zdtczqByteArray_closure
.type CopyArray_zdtczqByteArray_closure, @object
CopyArray_zdtczqByteArray_closure:
	.quad	ghczmprim_GHCziTypes_TyCon_con_info
	.quad	CopyArray_zdtrModule_closure+1
	.quad	CopyArray_zdtczqByteArray2_closure+1
	.quad	CopyArray_zdtczqByteArray1_closure+4
	.quad	5271623811131805094
	.quad	-8257582885654880761
	.quad	0
	.quad	3
.section .text
.align 8
.align 8
	.quad	4294967301
	.quad	0
	.long	14
	.long	0
CopyArray_ByteArray_info:
.Lc1kj:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .Lc1kn
.Lc1km:
	movq $CopyArray_ByteArray_con_info,-8(%r12)
	movq %r14,(%r12)
	leaq -7(%r12),%rbx
	jmp *(%rbp)
.Lc1kn:
	movq $16,904(%r13)
	movl $CopyArray_ByteArray_closure,%ebx
	jmp *-8(%r13)
	.size CopyArray_ByteArray_info, .-CopyArray_ByteArray_info
.section .data
.align 8
.align 1
.globl CopyArray_ByteArray_closure
.type CopyArray_ByteArray_closure, @object
CopyArray_ByteArray_closure:
	.quad	CopyArray_ByteArray_info
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
i1ks_str:
	.asciz "main:CopyArray.ByteArray"
.section .text
.align 8
.align 8
	.long	i1ks_str-(CopyArray_ByteArray_con_info)+0
	.long	0
	.quad	1
	.long	2
	.long	0
.globl CopyArray_ByteArray_con_info
.type CopyArray_ByteArray_con_info, @object
CopyArray_ByteArray_con_info:
.Lc1kr:
	incq %rbx
	jmp *(%rbp)
	.size CopyArray_ByteArray_con_info, .-CopyArray_ByteArray_con_info
.section .note.GNU-stack,"",@progbits
.ident "GHC 8.6.4"


