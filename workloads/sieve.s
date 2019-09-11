	.file	"sieve.c"
	.option nopic
	.text
	.align	1
	.globl	sieve
	.type	sieve, @function
sieve:
	lui	t1,%hi(.LANCHOR0)
	addi	a3,t1,%lo(.LANCHOR0)
	addi	a3,a3,1026
	li	a2,5
	li	a4,2
	addi	t1,t1,%lo(.LANCHOR0)
	li	a1,4
	lui	a0,%hi(.LANCHOR0)
	addi	a0,a0,%lo(.LANCHOR0)
	addi	t5,a0,1024
	addi	a0,a0,1280
	li	a6,1
	li	t3,256
	j	.L4
.L5:
	mv	t1,t4
.L2:
	addw	a1,a2,a1
	addi	a4,a4,1
	addi	a3,a3,1
	addiw	a2,a2,2
	beq	a4,t3,.L8
.L4:
	sext.w	a7,a4
	lbu	a5,0(a3)
	bnez	a5,.L2
	addi	t4,t1,4
	sw	a7,0(t1)
	add	a5,t5,a1
	bgeu	a5,a0,.L5
.L3:
	sb	a6,0(a5)
	add	a5,a5,a4
	bltu	a5,a0,.L3
	mv	t1,t4
	j	.L2
.L8:
	ret
	.size	sieve, .-sieve
	.bss
	.align	3
	.set	.LANCHOR0,. + 0
	.type	prime, @object
	.size	prime, 1024
prime:
	.zero	1024
	.type	cand, @object
	.size	cand, 256
cand:
	.zero	256
	.ident	"GCC: (GNU) 7.2.0"
