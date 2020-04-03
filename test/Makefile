RISCV := riscv64-linux-gnu-

test_compressed_disassembly: all_compressed.golden all_compressed.dut
	@echo -n 'SCORE (out of 49152): '
	@diff -uwU999999 all_compressed.golden all_compressed.dut | egrep '^ ' | wc -l
	diff -uw all_compressed.golden all_compressed.dut

all_compressed.dut: test_disasm
	./$^ 2> all_compressed.map > $@

all_compressed.golden: all_compressed.elf
	$(RISCV)objdump -d $^ | egrep '^    800' | sed 's, *<.*,,' > $@

../%.o: ../%.c
	$(CC) -g -I.. -o $@ -c $^

test_disasm: test_disasm.c ../riscv.o ../memory.o ../arch.o ../loadelf.o ../rvc.o
	$(CC) -g -I.. -o $@ $^

all_compressed.elf: all_compressed.s
	$(RISCV)gcc -Ttext=80000000 -nostdlib -o $@ $^

all_compressed.s: test_disasm
	./$^ --gen > $@
