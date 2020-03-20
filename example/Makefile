# This assumes that quicklisp is already enabled by your sbcl init
# files. If that is not the case, follow the installation instructions
# at https://www.quicklisp.org/beta/.
LISP=sbcl --noinform --non-interactive --eval '(push (truename "..") ql:*local-project-directories*)'
CFLAGS += -fPIC

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
	SOLINK += -dynamiclib
	SOEXT := dylib
else
	SOLINK += -shared
	SOEXT := so
endif

.PHONY: all clean test
all: libexample.$(SOEXT)

example.c example.h: example.lisp
	$(LISP) \
		--eval '(ql:quickload :callback-heaven-examples :silent t)' \
		--eval '(callback-heaven-examples::emit-library-api)' \
		--quit

libexample.$(SOEXT): example.o lispcall.o
	cc $(SOLINK) -fPIC -o $@ $^

test: libexample.$(SOEXT)
	$(LISP) \
		--eval '(ql:quickload :callback-heaven-examples :silent t)' \
		--load 'test.lisp' \
		--quit

clean:
	rm -f *.o *.$(SOEXT) example.[ch]
