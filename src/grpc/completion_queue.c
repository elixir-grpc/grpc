#include <grpc/grpc.h>
#include "erl_nif.h"
#include "grpc_nifs.h"
#include "utils.h"

ERL_NIF_TERM nif_completion_queue_create0(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM term;
  wrapped_grpc_completion_queue *wrapped_cq = enif_alloc_resource(grpc_completion_queue_resource,
                                                             sizeof(wrapped_grpc_completion_queue));
  wrapped_cq->cq = grpc_completion_queue_create(NULL);

  if (wrapped_cq->cq == NULL) {
    return enif_raise_exception_compat(env, "Could not create a completion queue.");
  }

  term = enif_make_resource(env, wrapped_cq);
  enif_release_resource(wrapped_cq);

  return term;
}

grpc_event completion_queue_pluck_event(ErlNifEnv *env, wrapped_grpc_completion_queue *wrapped_cq,
                                        ERL_NIF_TERM tag, ERL_NIF_TERM timeout) {
  grpc_event event;
  event.type = GRPC_QUEUE_TIMEOUT;

  gpr_timespec increment = gpr_time_from_millis(100, GPR_TIMESPAN);
  gpr_timespec deadline = gpr_time_add(gpr_now(GPR_CLOCK_REALTIME), increment);
  // TODO: Segmentation fault sometimes
  event = grpc_completion_queue_next(wrapped_cq->cq, deadline, NULL);
  return event;
}
