#include "erl_nif.h"
#include "grpc_nifs.h"
#include <grpc/grpc.h>

ERL_NIF_TERM nif_completion_queue_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  grpc_completion_queue *cq = grpc_completion_queue_create(NULL);

  if (cq == NULL) {
    enif_raise_exception(env, enif_make_string(env, "Could not create a completion queue: not sure why", ERL_NIF_LATIN1));
  }

  return enif_make_resource(env, cq);
}
