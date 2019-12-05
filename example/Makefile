all:
	cc -c example.c
	cc -c main.c
	cc -shared -fPIC -o libexample.dylib example.o main.o

clean:
	rm -f *.o *.dylib
