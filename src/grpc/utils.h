#ifndef UTILS_H
#define UTILS_H

#include <stdlib.h>
#include <string.h>
#include "erl_nif.h"

ERL_NIF_TERM enif_raise_exception_compat(ErlNifEnv* env, const char* string);

int better_get_atom(ErlNifEnv* env, ERL_NIF_TERM atom, char **buf);
int better_get_string(ErlNifEnv* env, ERL_NIF_TERM list, char **buf);

#endif
