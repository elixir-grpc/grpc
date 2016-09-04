#include <grpc/grpc.h>
#include <string.h>
#include "erl_nif.h"
#include "grpc_nifs.h"
#include "utils.h"

#define INSECURE_CRED_STRING  "this_channel_is_insecure"

ERL_NIF_TERM nif_channel_create3(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM term;
  wrapped_grpc_channel *wrapped_channel = enif_alloc_resource(grpc_channel_resource, sizeof(wrapped_grpc_channel));
  if (enif_is_atom(env, argv[2])) {
    char *target = NULL;
    char *credentials = NULL;
    int res1 = better_get_string(env, argv[0], &target);
    int res2 = better_get_atom(env, argv[2], &credentials);
    if (res1 && res2 && strcmp(credentials, INSECURE_CRED_STRING) == 0) {
      wrapped_channel->channel = grpc_insecure_channel_create(target, NULL, NULL);
    } else {
      return enif_raise_exception_compat(env, "The insecure cred must be atom :this_channel_is_insecure.");
    }
  } else {
    /* TODO: For secure  */
  }

  if (wrapped_channel->channel == NULL) {
    return enif_raise_exception_compat(env, "Could not create an rpc channel to the target.");
  }

  term = enif_make_resource(env, wrapped_channel);
  enif_release_resource(wrapped_channel);

  return term;
}
