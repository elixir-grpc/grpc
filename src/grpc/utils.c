#include "utils.h"

ERL_NIF_TERM enif_raise_exception_compat(ErlNifEnv* env, const char* string) {
  return enif_raise_exception(env, enif_make_string(env, string, ERL_NIF_LATIN1));
}
