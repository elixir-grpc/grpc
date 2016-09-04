#ifndef GRPC_NIFS_H
#define GRPC_NIFS_H

#include "erl_nif.h"
#include <grpc/grpc.h>

/* NIF function declarations */
ERL_NIF_TERM nif_completion_queue_create0(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_channel_create3(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_call_create7(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_call_run_batch3(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_call_finish_batch4(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

/* ErlNifResourceType declarations */
ErlNifResourceType* grpc_completion_queue_resource;
ErlNifResourceType* grpc_channel_resource;
ErlNifResourceType* grpc_call_resource;
ErlNifResourceType* run_batch_stack_resource;

typedef struct {
  grpc_channel *channel;
} wrapped_grpc_channel;

typedef struct {
  grpc_completion_queue *cq;
} wrapped_grpc_completion_queue;

typedef struct {
  grpc_call *call;
} wrapped_grpc_call;

/* run_batch_stack holds various values used by the
 * nif_call_run_batch3 function */
typedef struct run_batch_stack {
  /* The batch ops */
  grpc_op ops[8]; /* 8 is the maximum number of operations */
  size_t op_num;  /* tracks the last added operation */

  /* Data being sent */
  grpc_metadata_array send_metadata;
  grpc_metadata_array send_trailing_metadata;

  /* Data being received */
  grpc_byte_buffer *recv_message;
  grpc_metadata_array recv_metadata;
  grpc_metadata_array recv_trailing_metadata;
  int recv_cancelled;
  grpc_status_code recv_status;
  char *recv_status_details;
  size_t recv_status_details_capacity;
  unsigned write_flag;
} run_batch_stack;

typedef struct {
  run_batch_stack *stack;
} wrapped_run_batch_stack;

#endif
