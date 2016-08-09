#include <grpc/grpc.h>
#include "erl_nif.h"
#include "grpc_nifs.h"
#include "utils.h"

ERL_NIF_TERM nif_completion_queue_create0(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  grpc_completion_queue *cq = grpc_completion_queue_create(NULL);

  if (cq == NULL) {
    enif_raise_exception_compat(env, "Could not create a completion queue: not sure why");
    return ERL_NIL;
  }

  return enif_make_resource(env, cq);
}
