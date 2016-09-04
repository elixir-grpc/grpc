#include "byte_buffer.h"

#include <grpc/grpc.h>
#include "erl_nif.h"
#include "utils.h"

grpc_byte_buffer* str_to_byte_buffer(char *string, size_t length) {
  gpr_slice slice = gpr_slice_from_copied_buffer(string, length);
  grpc_byte_buffer *buffer = grpc_raw_byte_buffer_create(&slice, 1);
  gpr_slice_unref(slice);
  return buffer;
}
