CFLAGS += -I
LDLIBS += -L
LDLIBS += -L

SRCS    = $(wildcard *.c)
TARGETS = $(patsubst %.c,%,$(SRCS))

all: $(TARGETS)

install:
	@set -e; for i in $(TARGETS); do ln -f $$i ../../../bin/$$i ; done

clean:
	rm -f $(TARGETS)
