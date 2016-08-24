CFLAGS = -g2 -Wall -W

all: ichbins ichbins2 elvc elv newselvc.elv

clean:
	rm -f ichbins ichbins2* ichbins3* *.o tests/?.c tests/? tests/?.out

ichbins: ichbins.o

ichbins2: ichbins2.o

ichbins2.c: ichbins ichbins.scm 
	./ichbins <ichbins.scm >ichbins2.c

elvc: elvc.o
elv: elv.o

elvc.c: ichbins elvc.scm
	./ichbins <elvc.scm >elvc.c

newselvc.elv: elvas.py newselvc.s
	python elvas.py newselvc.s >newselvc.elv

newselvc.s: elv selvc.elv selvc.scm
	./elv selvc.elv <selvc.scm >newselvc.s
