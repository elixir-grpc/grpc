#ifndef GRPC_NIFS_H
#define GRPC_NIFS_H

#include "erl_nif.h"

/* NIF function declarations */
ERL_NIF_TERM nif_completion_queue_create0(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_channel_create3(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

/* ErlNifResourceType declarations */
static ErlNifResourceType* grpc_completion_queue_resource;
static ErlNifResourceType* grpc_channel_resource;

#endif
