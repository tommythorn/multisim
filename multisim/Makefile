# Multisim: a microprocessor architecture exploration framework
# Copyright (C) 2014 Tommy Thorn
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the
# Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.

WARN=-Wall -Werror -Wno-logical-op-parentheses -Wno-bitwise-op-parentheses \
     -Wno-parentheses
OPT=-O
CFLAGS=-g -std=c99 $(WARN) -MD $(OPT) -g -I/opt/local/include
LDFLAGS=-g
FLAGS=
OBJS=main.o arch.o \
	run_simple.o run_sscalar_io.o run_sscalar_oooe.o \
	memory.o loadelf.o alpha.o lm32.o riscv.o
#VERB=-t
VERB=-d

all: multisim

boot: multisim
	./multisim ~/BTSync/RISCV/vmlinux

boot_linux: multisim
	./multisim $(VERB) ../workloads/vmlinux.lm32

sieve_lm32: multisim
	./multisim $(VERB) ../workloads/sieve.lm32

sieve_alpha: multisim
	./multisim $(VERB) ../workloads/sieve.alpha

sieve_riscv: multisim
	./multisim $(VERB) ../workloads/sieve.riscv

install: multisim
	cp multisim $(PREFIX)/bin

multisim: $(OBJS)
	$(CC) $(LDFLAGS) $^ -o $@

clean:
	-rm *.o *.d multisim

realclean: clean
	-rm *~

TAGS:
	etags *.[hc]

regress: multisim
	./multisim -1 ../workloads/sieve.alpha
	./multisim -2 ../workloads/sieve.alpha
	./multisim -3 ../workloads/sieve.alpha

-include *.d
