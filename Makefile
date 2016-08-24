CFLAGS = -g2 -Wall -W

all: elv newselvc.elv

clean:
	rm -f elv *.o *.s newselvc.elv tests/*.elv tests/*.s tests/*.a tests/*.out syms tests/syms

elvc: elvc.o
elv: elv.o

elvc.c: ichbins elvc.scm
	./ichbins <elvc.scm >elvc.c

newselvc.elv: elvas.py newselvc.s
	python elvas.py newselvc.s >newselvc.elv

newselvc.s: elv selvc.elv selvc.scm
	./elv selvc.elv <selvc.scm >newselvc.s
