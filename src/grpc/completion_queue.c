#include <grpc/grpc.h>
#include "erl_nif.h"
#include "grpc_nifs.h"
#include "utils.h"

ERL_NIF_TERM nif_completion_queue_create0(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM term;
  wrapped_completion_queue *wrapped_cq = enif_alloc_resource(grpc_completion_queue_resource,
                                                             sizeof(wrapped_completion_queue));
  wrapped_cq->cq = grpc_completion_queue_create(NULL);

  if (wrapped_cq->cq == NULL) {
    return enif_raise_exception_compat(env, "Could not create a completion queue.");
  }

  term = enif_make_resource(env, wrapped_cq);
  enif_release_resource(wrapped_cq);

  return term;
}
