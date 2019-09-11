	.file	"vliw-sieve.c"
	.option nopic
	.local	stack
	.comm	stack,1024,4
	.local	prime
	.comm	prime,1024,4
	.local	cand
	.comm	cand,256,4
	.text
	.align	2
	.globl	_start
	.type	_start, @function
_start:
	lui	sp,%hi(prime)
	lui	gp,%hi(stack)
        nop
        nop
        nop
        nop
        nop
        nop

	addi	sp,sp,%lo(prime)
	addi	gp,gp,%lo(stack)
        nop
        nop
        nop
        nop
        nop
        nop

	addi	sp,sp,-32
        nop
        nop
        nop
        nop
        nop
        nop
        nop

        nop
        nop
        nop
        nop
        sw      s0,28(sp)
        nop
	addi    s0,sp,32
        lui     a5,%hi(prime)

        addi    a5,a5,%lo(prime)
        nop
        nop
        nop
        nop
        nop
        nop
        nop

        nop
        nop
        nop
        nop
        sw      a5,-28(s0)
	nop
        li      a5,2
        nop

        nop
        nop
        nop
        nop
        sw      a5,-20(s0)
	nop
        li      a5,4
        nop

        nop
        nop
        nop
        nop
        sw      a5,-24(s0)
        nop
        nop
        j       .L2


        .align 5
.L6:
	lui	a5,%hi(cand)
	addi	a4,a5,%lo(cand)
	lw	a5,-20(s0)
	add	a5,a4,a5
	lbu	a5,0(a5)
	bnez	a5,.L3
	lw	a5,-28(s0)
	addi	a4,a5,4
	sw	a4,-28(s0)
	lw	a4,-20(s0)
	sw	a4,0(a5)
	lw	a4,-24(s0)
	lui	a5,%hi(cand)
	addi	a5,a5,%lo(cand)
	add	a5,a4,a5
	sw	a5,-32(s0)
	j	.L4
.L5:
	lw	a5,-32(s0)
	li	a4,1
	sb	a4,0(a5)
	lw	a5,-20(s0)
	lw	a4,-32(s0)
	add	a5,a4,a5
	sw	a5,-32(s0)
.L4:
	lui	a5,%hi(cand+256)
	addi	a5,a5,%lo(cand+256)
	lw	a4,-32(s0)
	bltu	a4,a5,.L5
.L3:
	lw	a5,-20(s0)
	slli	a5,a5,1
	addi	a5,a5,1
	lw	a4,-24(s0)
	add	a5,a4,a5
	sw	a5,-24(s0)
	lw	a5,-20(s0)
	addi	a5,a5,1
	sw	a5,-20(s0)

        .align 5
.L2:
	li	a5,255
        nop
	lw	a4,-20(s0)
        nop
        nop
        nop
        nop
        nop

        nop
        nop
        nop
        nop
        nop
        nop
	ble	a4,a5,.L6
	nop

        nop
        nop
	lw	s0,28(sp)
	addi	sp,sp,32
	jr	ra
	.size	_start, .-_start
	.ident	"GCC: (GNU) 7.2.0"
