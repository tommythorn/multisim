	.file	"vliw-sieve.c"
	.option nopic
	.text
	.align	2
	.globl	_start
	.type	_start, @function
_start:
	lui	a5,%hi(.LANCHOR0)
	addi	a7,a5,%lo(.LANCHOR0)
	addi	a5,a5,%lo(.LANCHOR0)
	addi	a3,a5,1026
	li	a2,5
	li	a1,4
	li	a4,2
	addi	t3,a7,1024
	addi	a0,a7,1280
	li	a6,1
	li	t1,256
	j	.L4
.L2:
	addi	a4,a4,1
	add	a1,a1,a2
	addi	a3,a3,1
	addi	a2,a2,2
	beq	a4,t1,.L9

.L4:
	lbu	a5,0(a3)
	bnez	a5,.L2

	sw	a4,0(a7)
	add	a5,t3,a1
	addi	a7,a7,4
	bgeu	a5,a0,.L2

.L3:
	sb	a6,0(a5)
	add	a5,a5,a4
	bltu	a5,a0,.L3

	addi	a4,a4,1
	add	a1,a1,a2
	addi	a3,a3,1
	addi	a2,a2,2
	bne	a4,t1,.L4
.L9:
	ret
	.size	_start, .-_start
	.bss
	.align	2
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
