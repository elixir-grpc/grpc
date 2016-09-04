#ifndef BYTE_BUFFER_H_
#define BYTE_BUFFER_H_

#include <grpc/grpc.h>
#include "erl_nif.h"

grpc_byte_buffer *str_to_byte_buffer(char *string, size_t length);

#endif
