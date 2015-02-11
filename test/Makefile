all:
	cc -c test.c
	cc -c main.c
	cc -dynamiclib -fPIC -o libtest.dylib test.o main.o

clean:
	rm -f *.o *.dylib
