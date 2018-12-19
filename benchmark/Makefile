CFLAGS = -g -O3 -pedantic -Wall -Wextra -Wno-unused-parameter -Wno-missing-field-initializers
ERLANG_PATH = $(shell erl -eval 'io:format("~s", [lists:concat([code:root_dir(), "/erts-", erlang:system_info(version), "/include"])])' -s init stop -noshell)
CFLAGS += -I$(ERLANG_PATH)

ifneq ($(OS),Windows_NT)
	CFLAGS += -fPIC

	ifeq ($(shell uname),Darwin)
		LDFLAGS += -dynamiclib -undefined dynamic_lookup
	endif
endif

all: syscall.so

syscall.so: src/syscall.h src/syscall.c
	$(CC) $(CFLAGS) -shared $(LDFLAGS) -o $@ src/*.c

clear:
	$(RM) -r syscall.so.*

.PHONY: all clear
