WARN=-Wall -Werror -Wno-logical-op-parentheses -Wno-bitwise-op-parentheses
CFLAGS=-g $(WARN) -MD -O0 -g -I/opt/local/include
LDFLAGS=-g
FLAGS=

test_sieve: multisim
	./multisim ../workloads/sieve.alpha

install: multisim
	cp multisim $(PREFIX)/bin

multisim: main.o run_simple.o memory.o loadelf.o alpha-isa.o
	$(CC) $(LDFLAGS) $^ -o $@

clean:
	-rm *.o *.d multisim

realclean: clean
	-rm *~

TAGS:
	etags *.[hc]

-include *.d
