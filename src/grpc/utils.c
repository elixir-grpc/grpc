#include "utils.h"

ERL_NIF_TERM enif_raise_exception_compat(ErlNifEnv* env, const char* string) {
  return enif_raise_exception(env, enif_make_string(env, string, ERL_NIF_LATIN1));
}

char *alloc_chars(int len) {
  return (char *)malloc(sizeof(char) * (len + 1));
}

int better_get_atom(ErlNifEnv* env, ERL_NIF_TERM atom, char **buf) {
  unsigned int length;
  if (!enif_get_atom_length(env, atom, &length, ERL_NIF_LATIN1)) {
    return 0;
  }
  *buf = alloc_chars(length);
  return enif_get_atom(env, atom, *buf, length + 1, ERL_NIF_LATIN1);
}

int better_get_string(ErlNifEnv* env, ERL_NIF_TERM string, char **buf) {
  unsigned int length;
  if (enif_get_list_length(env, string, &length)) {
    *buf = alloc_chars(length);
    return enif_get_string(env, string, *buf, length + 1, ERL_NIF_LATIN1);
  } else {
    ErlNifBinary *bin = NULL;
    if (enif_is_binary(env, string) && enif_term_to_binary(env, string, bin)) {
      *buf = (char *)bin->data;
      return bin->size;
    }
  }
  return 0;
}
