	.file	"vliw-sieve.c"
	.option nopic
	.local	stack
	.comm	stack,1024,4
	.local	prime
	.comm	prime,1024,4
	.local	cand
	.comm	cand,256,4
	.text
	.align	5
	.globl	_start
	.type	_start, @function
_start:
	lui	sp,%hi(prime)
        nop
        nop
        nop
        nop
        nop
        nop
        nop

	addi	gp,sp,%lo(prime)
	addi	sp,sp,%lo(prime)
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

	li	a3,255
        nop
	lw	a4,-20(s0)
        nop
        nop
        sw      a5,-24(s0)
        nop
        nop
        nop

        nop
        nop
        nop
        nop
        nop
        nop
	ble	a4,a3,.L6
	nop

        nop
        nop
	lw	s0,28(sp)
	nop
	nop
        nop
        addi	sp,sp,32
	jr	ra


        .align 5
.L6:
	mv	a4,gp
	nop
	lw	a5,-20(s0)
	nop
        nop
        nop
	nop
        nop

	add	a5,a4,a5
        nop
        nop
        nop
        nop
        nop
	nop
	nop

	nop
        nop
	lbu	a5,0(a5)
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
        nop
	bnez	a5,.L3

	nop
        nop
	lw	a5,-28(s0)
	nop
        nop
	nop
	nop
        nop

	addi	a4,a5,4
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
	lw	a7,-20(s0)
	sw	a4,-28(s0)
        nop
        nop
        nop

	nop
        nop
	lw	a4,-24(s0)
	nop
	sw	a7,0(a5)
        nop
        mv	a5,gp
        nop

	add	a5,a4,a5
	nop
        nop
	nop
	nop
        nop
        nop
        nop

	nop
        nop
	lw	a4,-32(s0)
	nop
	sw	a5,-32(s0)
        nop
	mv	a5,gp
	nop

	nop
	nop
	nop
	nop
	nop
	nop
	nop
	bltu	a4,gp,.L5

	nop
	nop
	nop
	nop
	nop
	nop
	nop
	j	.L3

        .align 5
.L5:
	li	a4,1
        nop
	lw	a5,-32(s0)
	nop
        nop
	nop
	nop
        nop

        nop
        nop
        nop
        nop
	sb	a4,0(a5)
        nop
        nop
        nop


        nop
        nop
	lw	a5,-20(s0)
	lw	a4,-32(s0)
        nop
        nop
        nop
        nop

	add	a5,a4,a5
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
	sw	a5,-32(s0)
        nop
        nop
        nop

        .align 5
.L4:
	mv	a5,gp
	nop
	lw	a4,-32(s0)
	nop
	nop
	nop
	nop
	nop

.L44:
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	bltu	a4,a5,.L5

        .align 5
.L3:
	li	a0,255
	nop
	lw	a5,-20(s0)
	nop
	nop
	nop
	nop
	nop

	addi	a6,a5,1
	slli	a5,a5,1
	nop
	nop
	nop
	nop
	nop
	nop

	addi	a5,a5,1
	nop
	lw	a4,-24(s0)
	nop
	sw	a6,-20(s0)
	nop
	nop
	nop

	add	a5,a4,a5
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
	sw	a5,-24(s0)
	nop
	nop
	ble	a6,a0,.L6

        nop
        nop
	lw	s0,28(sp)
	nop
	nop
        nop
        addi	sp,sp,32
	jr	ra


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
	nop
	nop
        nop
        addi	sp,sp,32
	jr	ra

	.size	_start, .-_start
	.ident	"GCC: (GNU) 7.2.0"
