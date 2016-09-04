#include <grpc/grpc.h>
#include <grpc/support/alloc.h>
#include "erl_nif.h"
#include "grpc_nifs.h"
#include "utils.h"

/* run_batch_stack_init ensures the run_batch_stack is properly
 * initialized */
static void run_batch_stack_init(run_batch_stack *st, unsigned write_flag) {
  grpc_metadata_array_init(&st->send_metadata);
  grpc_metadata_array_init(&st->send_trailing_metadata);
  grpc_metadata_array_init(&st->recv_metadata);
  grpc_metadata_array_init(&st->recv_trailing_metadata);
  st->op_num = 0;
  st->write_flag = write_flag;
}

/* run_batch_stack_cleanup ensures the run_batch_stack is properly
 * cleaned up */
static void run_batch_stack_cleanup(run_batch_stack *st) {
  size_t i = 0;

  grpc_metadata_array_destroy(&st->send_metadata);
  grpc_metadata_array_destroy(&st->send_trailing_metadata);
  grpc_metadata_array_destroy(&st->recv_metadata);
  grpc_metadata_array_destroy(&st->recv_trailing_metadata);

  if (st->recv_status_details != NULL) {
    gpr_free(st->recv_status_details);
  }
  if (st->recv_message != NULL) {
    grpc_byte_buffer_destroy(st->recv_message);
  }

  for (i = 0; i < st->op_num; i++) {
    if (st->ops[i].op == GRPC_OP_SEND_MESSAGE) {
      grpc_byte_buffer_destroy(st->ops[i].data.send_message);
    }
  }
}

ERL_NIF_TERM nif_call_create7(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM term;
  wrapped_grpc_call *wrapped_call = enif_alloc_resource(grpc_call_resource,
                                                         sizeof(wrapped_grpc_call));
  grpc_call *parent_call = NULL;
  wrapped_grpc_channel *wrapped_channel;
  wrapped_grpc_completion_queue *wrapped_cq;
  gpr_timespec deadline;
  char *method = NULL;
  char *host = NULL;
  int flags = GRPC_PROPAGATE_DEFAULTS;
  ErlNifSInt64 timeout;
  ERL_NIF_TERM ERL_NIL = enif_make_atom(env, "nil");

  if (!enif_get_resource(env, argv[0], grpc_channel_resource, (void **)&wrapped_channel)) {
    return enif_raise_exception_compat(env, "Channel is not a handle to the right resource object.");
  }
  if (!enif_get_resource(env, argv[3], grpc_completion_queue_resource, (void **)&wrapped_cq)) {
    return enif_raise_exception_compat(env, "CompletionQueue is not a handle to the right resource object.");
  }
  if (!better_get_string(env, argv[4], &method)) {
    return enif_raise_exception_compat(env, "method is invalid!");
  }
  // if (!better_get_string(env, argv[5], &host)) {
  //   return enif_raise_exception_compat(env, "host is invalid!");
  // }

  if (enif_get_int64(env, argv[6], &timeout)) {
    deadline = gpr_time_from_seconds((int64_t)timeout, GPR_CLOCK_REALTIME);
  } else {
    return enif_raise_exception_compat(env, "timeout is invalid!");
  }

  wrapped_call->call = grpc_channel_create_call(wrapped_channel->channel, parent_call, flags, wrapped_cq->cq, method, host, deadline, NULL);

  if (wrapped_call->call == NULL) {
    enif_raise_exception_compat(env, "Could not create call.");
    return ERL_NIL;
  }

  term = enif_make_resource(env, wrapped_call);
  enif_release_resource(wrapped_call);

  return term;
}
ERL_NIF_TERM nif_call_run_batch3(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  wrapped_grpc_call *wrapped_call;
  wrapped_run_batch_stack *stack = enif_alloc_resource(run_batch_stack_resource,
                                            sizeof(wrapped_run_batch_stack));
  grpc_call_error err;
  unsigned write_flag = 0;
  ERL_NIF_TERM term;
  ERL_NIF_TERM ERL_NIL = enif_make_atom(env, "nil");
  stack->stack = malloc(sizeof(run_batch_stack));

  if (!enif_get_resource(env, argv[0], grpc_call_resource, (void **)&wrapped_call)) {
    return enif_raise_exception_compat(env, "Could not get call.");
  }
  if (!enif_is_map(env, argv[1])) {
    return enif_raise_exception_compat(env, "ops should be a map");
  }
  // TODO: set write_flag if write_flag is passed

  run_batch_stack_init(stack->stack, write_flag);
  if (!run_batch_stack_fill_ops(env, stack->stack, argv[1])) {
    return ERL_NIL;
  }

  err = grpc_call_start_batch(wrapped_call->call, stack->stack->ops, stack->stack->op_num, NULL, NULL);
  if (err != GRPC_CALL_OK) {
    run_batch_stack_cleanup(stack->stack);
    return enif_raise_exception_compat(env, "Failed to start batch!");
  }

  term = enif_make_resource(env, stack);
  enif_release_resource(stack);

  return term;
}

