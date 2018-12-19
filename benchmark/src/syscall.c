#include "syscall.h"
#include <sys/time.h>

static ErlNifFunc nif_funcs[] = {
  {"getrusage", 0, enif_getrusage0}
};

ERL_NIF_INIT(Elixir.Benchmark.Syscall, nif_funcs, NULL, NULL, NULL, NULL)

ERL_NIF_TERM enif_utils_raise_exception_compat(ErlNifEnv *env,
                                               const char *string) {
  return enif_raise_exception(env,
                              enif_make_string(env, string, ERL_NIF_LATIN1));
}

ERL_NIF_TERM enif_getrusage0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM term = enif_make_new_map(env);

  struct rusage usage;

  if (getrusage(RUSAGE_SELF, &usage) != 0) {
    return enif_utils_raise_exception_compat(
        env, "getrusage(RUSAGE_SELF, _) failed");
  }

  // {usage.ru_utime.tv_sec, usage.ru_utime.tv_usec}
  ERL_NIF_TERM utime =
      enif_make_tuple2(env, enif_make_int(env, (int)usage.ru_utime.tv_sec), enif_make_int(env, (int)usage.ru_utime.tv_usec));
  // {usage.ru_stime.tv_sec, usage.ru_stime.tv_usec}
  ERL_NIF_TERM stime =
      enif_make_tuple2(env, enif_make_int(env, (int)usage.ru_stime.tv_sec), enif_make_int(env, (int)usage.ru_stime.tv_usec));

  int flag;
  flag = enif_make_map_put(env, term, enif_make_atom(env, "utime"), utime, &term);
  if (!flag) {
    return enif_utils_raise_exception_compat(env, "make map from arrays failed");
  }
  flag = enif_make_map_put(env, term, enif_make_atom(env, "stime"), stime, &term);
  // Supported from OTP21
  // flag = enif_make_map_from_arrays(env, keys, values, count, &term);
  if (!flag) {
    return enif_utils_raise_exception_compat(env, "make map from arrays failed");
  }
  return term;
}
