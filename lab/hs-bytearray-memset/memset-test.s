.section .text
.align 8
.globl callMemset2
.type callMemset2, @function
callMemset2:
.Lc5:
	leaq 3(%rbx),%rax
	movb $0,0(%rax)
	movw $0,1(%rax)
	movl $0,3(%rax)
	movl $0,7(%rax)
	movw $0,11(%rax)
	movb $0,13(%rax)
	jmp *(%rbp)
	.size callMemset2, .-callMemset2
.section .note.GNU-stack,"",@progbits
.ident "GHC 8.9.20190321"


