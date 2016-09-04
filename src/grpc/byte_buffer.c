#include "byte_buffer.h"

#include <grpc/grpc.h>
#include "erl_nif.h"
#include "utils.h"
#include <grpc/byte_buffer_reader.h>
#include <grpc/support/slice.h>
#include <grpc/support/alloc.h>
#include <grpc/support/log.h>

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
  gpr_slice next;
  grpc_byte_buffer_reader_init(&reader, buffer);
  size_t length = grpc_byte_buffer_length(buffer);
  char result[length + 1];
  size_t offset = 0;
  while (grpc_byte_buffer_reader_next(&reader, &next) != 0) {
    GPR_ASSERT(offset + GPR_SLICE_LENGTH(next) <= length);
    memcpy(result + offset, (const char *)GPR_SLICE_START_PTR(next), GPR_SLICE_LENGTH(next));
    offset += GPR_SLICE_LENGTH(next);
    gpr_slice_unref(next);
  }

  return better_make_binary2(env, result, length);
}
