#include "byte_buffer.h"

#include <grpc/grpc.h>
#include "erl_nif.h"
#include "utils.h"
#include <grpc/byte_buffer_reader.h>
#include <grpc/support/slice.h>

grpc_byte_buffer* str_to_byte_buffer(char *string, size_t length) {
  gpr_slice slice = gpr_slice_from_copied_buffer(string, length);
  grpc_byte_buffer *buffer = grpc_raw_byte_buffer_create(&slice, 1);
  gpr_slice_unref(slice);
  return buffer;
}

ERL_NIF_TERM byte_buffer_to_binary(ErlNifEnv *env, grpc_byte_buffer *buffer) {
  if (buffer == NULL) {
    return enif_make_atom(env, "nil");
  }
  grpc_byte_buffer_reader reader;
  grpc_byte_buffer_reader_init(&reader, buffer);
  gpr_slice slice = grpc_byte_buffer_reader_readall(&reader);
  size_t length = GPR_SLICE_LENGTH(slice);
  char result[length];
  memcpy(result, GPR_SLICE_START_PTR(slice), length);

  return better_make_binary(env, result);
}
