#include "erl_nif.h"
#include "grpc_nifs.h"
#include <grpc/grpc.h>

static ErlNifFunc nif_funcs[] = {
  {"completion_queue_create", 0, nif_completion_queue_create0},
  {"channel_create", 3, nif_channel_create3},
  {"call_create", 7, nif_call_create7}
};

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info) {
  grpc_completion_queue_resource = enif_open_resource_type(env, NULL, "grpc_completion_queue",
                                    NULL, ERL_NIF_RT_CREATE, NULL);
  grpc_channel_resource = enif_open_resource_type(env, NULL, "grpc_channel_resource",
                                    NULL, ERL_NIF_RT_CREATE, NULL);
  grpc_call_resource = enif_open_resource_type(env, NULL, "grpc_call_resource",
                                    NULL, ERL_NIF_RT_CREATE, NULL);
  grpc_init();
  return 0;
}

ERL_NIF_INIT(Elixir.GRPC.Nifs, nif_funcs, &load, NULL, NULL, NULL)
