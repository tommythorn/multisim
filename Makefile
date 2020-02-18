# Multisim: a microprocessor architecture exploration framework
# Copyright (C) 2014,2019 Tommy Thorn
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

ISA=
#ISA=-m32
WARN=-Wall -Werror -Wno-parentheses
OPT=
CFLAGS=-g -std=c99 $(WARN) -MD $(OPT) $(ISA) -g -I/opt/local/include
LDFLAGS=-g $(ISA)
FLAGS=
#MORE_STUFF=run_sscalar.o run_sscalar_oooe.o
MORE_STUFF=
OBJS=main.o arch.o \
	run_simple.o run_ooo.o $(MORE_STUFF) \
	memory.o loadelf.o riscv.o
#VERB=-t
VERB=-d

regress: test_philo

all: multisim

multisim: $(OBJS)
	$(CC) $(LDFLAGS) $^ -o $@

clean:
	-rm *.o *.d multisim

realclean: clean
	-rm *~

TAGS tags:
	etags *.[hc]

test_hello: multisim
	./multisim -d --ooo workloads/zephyr/hello_world.elf

test_ooo: multisim
	./multisim -d --ooo workloads/riscv-tests/rv32ui-p-lw

test_philo: multisim
	./multisim -d --ooo workloads/zephyr/philosophers.elf

arewethereyet:
	@bash -c 'printf "We are %3.1f%% there\n" $$(echo `./regress-rv32.sh --ooo|grep SUCCESS|wc -l` / 0.39|bc -l)'


-include *.d
