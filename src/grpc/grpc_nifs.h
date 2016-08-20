#ifndef GRPC_NIFS_H
#define GRPC_NIFS_H

#include "erl_nif.h"
#include <grpc/grpc.h>

/* NIF function declarations */
ERL_NIF_TERM nif_completion_queue_create0(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_channel_create3(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_call_create7(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

/* ErlNifResourceType declarations */
ErlNifResourceType* grpc_completion_queue_resource;
ErlNifResourceType* grpc_channel_resource;

typedef struct {
  grpc_channel *channel;
} wrapped_grpc_channel;

typedef struct {
  grpc_completion_queue *cq;
} wrapped_completion_queue;

#endif
