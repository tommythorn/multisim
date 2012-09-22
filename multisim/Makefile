# Multisim: a microprocessor architecture exploration framework
# Copyright (C) 2012 Tommy Thorn
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

WARN=-Wall -Werror -Wno-logical-op-parentheses -Wno-bitwise-op-parentheses
CFLAGS=-g $(WARN) -MD -O0 -g -I/opt/local/include
LDFLAGS=-g
FLAGS=

test_sieve: multisim
	./multisim ../workloads/sieve.alpha

install: multisim
	cp multisim $(PREFIX)/bin

multisim: main.o run_simple.o run_sscalar_io.o run_sscalar_oooe.o memory.o loadelf.o alpha-isa.o
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
