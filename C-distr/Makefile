

CFLAGS	=-I.
LIBS	= p2c/p2clib.o
TARFILE	=/usr/ftp/pub/kamin-c.tar

GOALS	= chap1 lisp apl sasl scheme clu smalltalk prolog

goal:	$(GOALS)
	@echo done

clean:
	rm -f $(GOALS) *.o test.*.out p2c/*.o

tar:	clean
	tar crf $(TARFILE) .
	compress $(TARFILE)

p2c/p2clib.o: p2c/p2clib.c p2c/p2c.h
	$(CC) $(CFLAGS) -o p2c/p2clib.o -c p2c/p2clib.c

chap1: chap1.c $(LIBS)
	$(CC) $(CFLAGS) -o chap1 chap1.c $(LIBS)
	chap1 < code.ch1 > test.ch1.out
	diff -c test.ch1.out code.ch1.out
	rm -f test.*.out

lisp: lisp.c $(LIBS)
	$(CC) $(CFLAGS) -o lisp lisp.c $(LIBS)
	lisp < code.lsp > test.lsp.out
	diff -c test.lsp.out code.lsp.out
	rm -f test.*.out

apl: apl.c $(LIBS)
	$(CC) $(CFLAGS) -o apl apl.c $(LIBS)
	apl < code.apl > test.apl.out
	diff -c test.apl.out code.apl.out
	rm -f test.*.out

clu: clu.c $(LIBS)
	$(CC) $(CFLAGS) -o clu clu.c $(LIBS)
	clu < code.clu > test.clu.out
	diff -c test.clu.out code.clu.out
	rm -f test.*.out

scheme: scheme.c $(LIBS)
	$(CC) $(CFLAGS) -o scheme scheme.c $(LIBS)
	scheme < code.sch > test.sch.out
	diff -c test.sch.out code.sch.out
	rm -f test.*.out

smalltalk: smalltalk.c $(LIBS)
	$(CC) $(CFLAGS) -o smalltalk smalltalk.c $(LIBS)
	smalltalk < code.smt > test.smt.out
	diff -c test.smt.out code.smt.out
	rm -f test.*.out

sasl: sasl.c $(LIBS)
	$(CC) $(CFLAGS) -o sasl sasl.c $(LIBS)
	sasl < code.ssl > test.ssl.out
	diff -c test.ssl.out code.ssl.out
	rm -f test.*.out

prolog: prolog.c $(LIBS)
	$(CC) $(CFLAGS) -o prolog prolog.c $(LIBS)
	prolog < code1.pro > test.pro.out
	diff -c test.pro.out code1.pro.out
	prolog < code2.pro > test.pro.out
	diff -c test.pro.out code2.pro.out
	rm -f test.*.out

.p.c:
	p2c $*.p
