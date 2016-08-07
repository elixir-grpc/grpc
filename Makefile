MIX = mix
MAKEFILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
GRPC_ELIXIR_PATH = $(patsubst %/,%,$(dir $(MAKEFILE_PATH)))
BUILD_DIR = $(GRPC_ELIXIR_PATH)/tmp/grpc_c

CFLAGS = -g -O3 -pedantic -Wall -Wextra -Wno-unused-parameter -Wno-missing-field-initializers

ERLANG_PATH = $(shell erl -eval 'io:format("~s", [lists:concat([code:root_dir(), "/erts-", erlang:system_info(version), "/include"])])' -s init stop -noshell)
CFLAGS += -I$(ERLANG_PATH)
CFLAGS += -I$(GRPC_ELIXIR_PATH)/include

GRPC_C_PATH = $(GRPC_ELIXIR_PATH)/src/grpc_c

ifneq ($(OS),Windows_NT)
	CFLAGS += -fPIC

	ifeq ($(shell uname),Darwin)
		LDFLAGS += -dynamiclib -undefined dynamic_lookup
	endif
endif

all: priv/grpc_c.so

priv/grpc_c.so: libgrpc.a
	$(CC) $(CFLAGS) -shared $(LDFLAGS) -o $@ $(GRPC_ELIXIR_PATH)/src/grpc/*.c $(BUILD_DIR)/libs/opt/libgrpc.a

libgrpc.a: grpc_c
	BUILDDIR=$(BUILD_DIR) $(MAKE) -C $(GRPC_C_PATH) $(BUILD_DIR)/libs/opt/libgrpc.a

grpc_c: $(GRPC_C_PATH)/include/grpc $(GRPC_C_PATH)/src/core $(GRPC_C_PATH)/src/boringssl $(GRPC_C_PATH)/src/zlib $(GRPC_C_PATH)/third_party/nanopb $(GRPC_C_PATH)/third_party/zlib $(GRPC_C_PATH)/third_party/boringssl $(GRPC_C_PATH)/Makefile

clean:
	BUILDDIR=$(BUILD_DIR) $(MAKE) -C $(GRPC_C_PATH) clean
	$(RM) priv/grpc_c.so

.PHONY: all grpc libgrpc.a grpc_c clean
