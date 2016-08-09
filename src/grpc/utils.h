#ifndef UTILS_H
#define UTILS_H

#include <stdlib.h>
#include <string.h>
#include "erl_nif.h"

#define ERL_NIL enif_make_atom(env, "nil")

ERL_NIF_TERM enif_raise_exception_compat(ErlNifEnv* env, const char* string);

#endif
