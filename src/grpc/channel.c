#include <grpc/grpc.h>
#include <string.h>
#include "erl_nif.h"
#include "grpc_nifs.h"
#include "utils.h"

#define INSECURE_CRED_STRING    "this_channel_is_insecure"

ERL_NIF_TERM nif_channel_create3(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  grpc_channel *channel = NULL;
  if (enif_is_atom(env, argv[2])) {
    unsigned int len = 0;
    enif_get_atom_length(env, argv[2], &len, ERL_NIF_LATIN1);
    char target[len];
    int res = enif_get_atom(env, argv[2], (char *)target, len + 1, ERL_NIF_LATIN1);
    if (res && strcmp(target, INSECURE_CRED_STRING) == 0) {
      channel = grpc_insecure_channel_create(target, NULL, NULL);
    } else {
      enif_raise_exception_compat(env, "The insecure cred must be atom :this_channel_is_insecure.");
      return ERL_NIL;
    }
  } else {
    /* TODO: For secure  */
  }

  if (channel == NULL) {
    enif_raise_exception_compat(env, "Could not create an rpc channel to the target.");
    return ERL_NIL;
  }

  return enif_make_resource(env, channel);
}
