#ifndef GRPC_BENCH_SYSCALL_H
#define GRPC_BENCH_SYSCALL_H

#include <sys/time.h>
#include <sys/resource.h>
#include "erl_nif.h"

ERL_NIF_TERM enif_utils_raise_exception_compat(ErlNifEnv* env, const char* string);

ERL_NIF_TERM enif_getrusage0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);


#endif
