#ifndef COMPLETION_QUEUE_H_
#define COMPLETION_QUEUE_H_

#include <grpc/grpc.h>
#include "erl_nif.h"

grpc_event completion_queue_pluck_event(ErlNifEnv *env, wrapped_grpc_completion_queue *wrapped_cq,
                                        ERL_NIF_TERM tag, ERL_NIF_TERM timeout);

#endif
