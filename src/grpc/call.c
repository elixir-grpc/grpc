#include <grpc/grpc.h>
#include <grpc/support/alloc.h>
#include "erl_nif.h"
#include "grpc_nifs.h"
#include "utils.h"
#include "byte_buffer.h"
#include "completion_queue.h"

/* run_batch_stack_init ensures the run_batch_stack is properly
 * initialized */
static void run_batch_stack_init(run_batch_stack *st, unsigned write_flag) {
  grpc_metadata_array_init(&st->send_metadata);
  grpc_metadata_array_init(&st->send_trailing_metadata);
  grpc_metadata_array_init(&st->recv_metadata);
  grpc_metadata_array_init(&st->recv_trailing_metadata);
  st->op_num = 0;
  st->write_flag = write_flag;
}

/* run_batch_stack_cleanup ensures the run_batch_stack is properly
 * cleaned up */
static void run_batch_stack_cleanup(run_batch_stack *st) {
  size_t i = 0;

  grpc_metadata_array_destroy(&st->send_metadata);
  grpc_metadata_array_destroy(&st->send_trailing_metadata);
  grpc_metadata_array_destroy(&st->recv_metadata);
  grpc_metadata_array_destroy(&st->recv_trailing_metadata);

  if (st->recv_status_details != NULL) {
    gpr_free(st->recv_status_details);
  }
  if (st->recv_message != NULL) {
    grpc_byte_buffer_destroy(st->recv_message);
  }

  for (i = 0; i < st->op_num; i++) {
    if (st->ops[i].op == GRPC_OP_SEND_MESSAGE) {
      grpc_byte_buffer_destroy(st->ops[i].data.send_message);
    }
  }
}

ERL_NIF_TERM nif_call_create7(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM term;
  wrapped_grpc_call *wrapped_call = enif_alloc_resource(grpc_call_resource,
                                                         sizeof(wrapped_grpc_call));
  grpc_call *parent_call = NULL;
  wrapped_grpc_channel *wrapped_channel;
  wrapped_grpc_completion_queue *wrapped_cq;
  gpr_timespec deadline;
  char *method = NULL;
  char *host = NULL;
  int flags = GRPC_PROPAGATE_DEFAULTS;
  ErlNifSInt64 timeout;
  ERL_NIF_TERM ERL_NIL = enif_make_atom(env, "nil");

  if (!enif_get_resource(env, argv[0], grpc_channel_resource, (void **)&wrapped_channel)) {
    return enif_raise_exception_compat(env, "Channel is not a handle to the right resource object.");
  }
  if (!enif_get_resource(env, argv[3], grpc_completion_queue_resource, (void **)&wrapped_cq)) {
    return enif_raise_exception_compat(env, "CompletionQueue is not a handle to the right resource object.");
  }
  if (!better_get_string(env, argv[4], &method)) {
    return enif_raise_exception_compat(env, "method is invalid!");
  }
  // if (!better_get_string(env, argv[5], &host)) {
  //   return enif_raise_exception_compat(env, "host is invalid!");
  // }

  if (enif_get_int64(env, argv[6], &timeout)) {
    deadline = gpr_time_from_seconds((int64_t)timeout, GPR_CLOCK_REALTIME);
  } else {
    return enif_raise_exception_compat(env, "timeout is invalid!");
  }

  wrapped_call->call = grpc_channel_create_call(wrapped_channel->channel, parent_call, flags, wrapped_cq->cq, method, host, deadline, NULL);

  if (wrapped_call->call == NULL) {
    enif_raise_exception_compat(env, "Could not create call.");
    return ERL_NIL;
  }

  term = enif_make_resource(env, wrapped_call);
  enif_release_resource(wrapped_call);

  return term;
}

int set_capacity_of_medadata(ErlNifEnv *env, ERL_NIF_TERM md_map, grpc_metadata_array *md_array) {
  ERL_NIF_TERM key, value;
  ErlNifMapIterator iter;
  enif_map_iterator_create(env, md_map, &iter, ERL_NIF_MAP_ITERATOR_FIRST);
  while (enif_map_iterator_get_pair(env, &iter, &key, &value)) {
    if (enif_is_list(env, value)) {
      char *str;
      if (better_get_string(env, value, &str)) {
        md_array->capacity += 1;
      } else {
        unsigned len;
        enif_get_list_length(env, value, &len);
        md_array->capacity += len;
      }
    } else {
      enif_raise_exception_compat(env, "metadata values should be string or list!");
      return 0;
    }

    enif_map_iterator_next(env, &iter);
  }
  enif_map_iterator_destroy(env, &iter);

  return 1;
}

int metadata_header_is_valid(char *key, size_t key_len, char *val, size_t val_len) {
  if (grpc_is_binary_header(key, key_len) ||
      grpc_header_nonbin_value_is_legal(val, val_len)) {
    return 1;
  } else {
    return 0;
  }
}

int set_metadata_array(ErlNifEnv *env, ERL_NIF_TERM md_map, grpc_metadata_array *md_array) {
  char *key_str;
  char *val_str;
  size_t key_len;
  ERL_NIF_TERM key, value;
  ErlNifMapIterator iter;
  enif_map_iterator_create(env, md_map, &iter, ERL_NIF_MAP_ITERATOR_FIRST);
  while (enif_map_iterator_get_pair(env, &iter, &key, &value)) {
    key_len = better_get_string(env, key, &key_str);
    if (!key_len) {
      enif_raise_exception_compat(env, "Key of metadata should be a string!");
      return 0;
    }

    if (!grpc_header_key_is_legal(key_str, key_len)) {
      enif_raise_exception_compat(env, "header key is illegal!");
      return 0;
    }
    if (!enif_is_list(env, value)) {
      enif_raise_exception_compat(env, "Header value must be of type string or list!");
      return 0;
    }

    int val_len = better_get_string(env, value, &val_str);
    if (val_len) {
      if (!metadata_header_is_valid(key_str, key_len, val_str, val_len)) {
        enif_raise_exception_compat(env, "Header is invalid!");
        return 0;
      }
      md_array->metadata[md_array->count].key = key_str;
      md_array->metadata[md_array->count].value = val_str;
      md_array->metadata[md_array->count].value_length = val_len;
      md_array->count += 1;
    } else {
      ERL_NIF_TERM head;
      ERL_NIF_TERM tail = value;
      while (enif_get_list_cell(env, tail, &head, &tail)) {
        val_len = better_get_string(env, head, &val_str);
        if (!val_len) {
          enif_raise_exception_compat(env, "Value of each metadata array should be string!");
          return 0;
        }
        if (!metadata_header_is_valid(key_str, key_len, val_str, val_len)) {
          enif_raise_exception_compat(env, "Header is invalid!");
          return 0;
        }
        md_array->metadata[md_array->count].key = key_str;
        md_array->metadata[md_array->count].value = val_str;
        md_array->metadata[md_array->count].value_length = val_len;
        md_array->count += 1;
      }
    }

    enif_map_iterator_next(env, &iter);
  }
  enif_map_iterator_destroy(env, &iter);

  return 1;
}

int metadata_map_to_array(ErlNifEnv *env, ERL_NIF_TERM md_map, grpc_metadata_array *md_array) {
  if (!enif_is_map(env, md_map)) {
    enif_raise_exception_compat(env, "metadata should be a map!");
    return 0;
  }

  grpc_metadata_array_init(md_array);

  set_capacity_of_medadata(env, md_map, md_array);
  md_array->metadata = gpr_malloc(md_array->capacity * sizeof(grpc_metadata));
  set_metadata_array(env, md_map, md_array);

  return 1;
}

int run_batch_stack_fill_ops(ErlNifEnv *env, run_batch_stack *st, ERL_NIF_TERM ops_map) {
  ERL_NIF_TERM this_op, this_value;
  int int_op;
  ErlNifMapIterator iter;
  size_t index;
  ErlNifBinary bin;

  enif_map_iterator_create(env, ops_map, &iter, ERL_NIF_MAP_ITERATOR_FIRST);
  while (enif_map_iterator_get_pair(env, &iter, &this_op, &this_value)) {
    if (!enif_get_int(env, this_op, &int_op)) {
      enif_raise_exception_compat(env, "op this_op should be a valid int!");
      return 0;
    }
    index = st->op_num;
    st->ops[index].flags = 0;
    st->ops[index].reserved = NULL;
    switch (int_op) {
      case GRPC_OP_SEND_INITIAL_METADATA:
        metadata_map_to_array(env, this_value, &st->send_metadata);
        st->ops[index].op = GRPC_OP_SEND_INITIAL_METADATA;
        st->ops[index].data.send_initial_metadata.count = st->send_metadata.count;
        st->ops[index].data.send_initial_metadata.metadata = st->send_metadata.metadata;
        break;
      case GRPC_OP_SEND_MESSAGE:
        if (better_get_binary(env, this_value, &bin)) {
          st->ops[index].op = GRPC_OP_SEND_MESSAGE;
          st->ops[index].data.send_message = str_to_byte_buffer((char *)bin.data, (size_t)bin.size);
          st->ops[index].flags = st->write_flag;
        } else {
          enif_raise_exception_compat(env, "Sending message can't be parsed!");
          return 0;
        }
        break;
      case GRPC_OP_SEND_CLOSE_FROM_CLIENT:
        st->ops[index].op = GRPC_OP_SEND_CLOSE_FROM_CLIENT;
        break;
      case GRPC_OP_SEND_STATUS_FROM_SERVER:
        // TODO: For server
        st->ops[index].op = GRPC_OP_SEND_STATUS_FROM_SERVER;
        break;
      case GRPC_OP_RECV_INITIAL_METADATA:
        st->ops[index].op = GRPC_OP_RECV_INITIAL_METADATA;
        st->ops[index].data.recv_initial_metadata = &st->recv_metadata;
        break;
      case GRPC_OP_RECV_MESSAGE:
        st->ops[index].op = GRPC_OP_RECV_MESSAGE;
        st->ops[index].data.recv_message = &st->recv_message;
        break;
      case GRPC_OP_RECV_STATUS_ON_CLIENT:
        st->ops[index].op = GRPC_OP_RECV_STATUS_ON_CLIENT;
        st->ops[index].data.recv_status_on_client.trailing_metadata = &st->recv_trailing_metadata;
        st->ops[index].data.recv_status_on_client.status = &st->recv_status;
        st->ops[index].data.recv_status_on_client.status_details = &st->recv_status_details;
        st->ops[index].data.recv_status_on_client.status_details_capacity = &st->recv_status_details_capacity;
        break;
      case GRPC_OP_RECV_CLOSE_ON_SERVER:
        st->ops[index].op = GRPC_OP_RECV_CLOSE_ON_SERVER;
        st->ops[index].data.recv_close_on_server.cancelled = &st->recv_cancelled;
        break;
      default:
        run_batch_stack_cleanup(st);
        enif_raise_exception_compat(env, "Unknown op!");
        return 0;
    }
    st->op_num++;
    enif_map_iterator_next(env, &iter);
  }
  enif_map_iterator_destroy(env, &iter);

  return 1;
}

ERL_NIF_TERM metadata_array_to_map(ErlNifEnv *env, grpc_metadata_array *md_array) {
  ERL_NIF_TERM sym_nil = enif_make_atom(env, "nil");
  ERL_NIF_TERM key = sym_nil;
  ERL_NIF_TERM new_list = sym_nil;
  ERL_NIF_TERM value = sym_nil;
  ERL_NIF_TERM result = enif_make_new_map(env);

  for (size_t i = 0; i < md_array->count; i++) {
    key = better_make_binary(env, (char *)md_array->metadata[i].key);
    if (enif_get_map_value(env, result, key, &value)) {
      if (enif_is_list(env, value)) {
        ERL_NIF_TERM head = better_make_binary2(env, md_array->metadata[i].value,
                                                md_array->metadata[i].value_length);
        value = enif_make_list_cell(env, head, value);
      } else {
        ERL_NIF_TERM new_val = better_make_binary2(env, md_array->metadata[i].value,
                                                   md_array->metadata[i].value_length);
        new_list = enif_make_list(env, 2, value, new_val);
        enif_make_map_put(env, result, key, new_list, &result);
      }
    } else {
      value = better_make_binary2(env, md_array->metadata[i].value, md_array->metadata[i].value_length);
      enif_make_map_put(env, result, key, value, &result);
    }
  }
  return result;
}

static ERL_NIF_TERM run_batch_stack_build_result(ErlNifEnv *env, run_batch_stack *st) {
  ERL_NIF_TERM result = enif_make_new_map(env);

  ERL_NIF_TERM sym_true = enif_make_atom(env, "true");
  ERL_NIF_TERM sym_send_message = enif_make_atom(env, "send_message");
  ERL_NIF_TERM sym_send_metadata = enif_make_atom(env, "send_metadata");
  ERL_NIF_TERM sym_send_close = enif_make_atom(env, "send_close");
  ERL_NIF_TERM sym_send_status = enif_make_atom(env, "send_status");
  ERL_NIF_TERM sym_message = enif_make_atom(env, "message");
  ERL_NIF_TERM sym_metadata = enif_make_atom(env, "metadata");

  ERL_NIF_TERM status;

  for (size_t i = 0; i < st->op_num; i++) {
    switch (st->ops[i].op) {
      case GRPC_OP_SEND_INITIAL_METADATA:
        enif_make_map_put(env, result, sym_send_metadata, sym_true, &result);
        break;
      case GRPC_OP_SEND_MESSAGE:
        enif_make_map_put(env, result, sym_send_message, sym_true, &result);
        break;
      case GRPC_OP_SEND_CLOSE_FROM_CLIENT:
        enif_make_map_put(env, result, sym_send_close, sym_true, &result);
        break;
      case GRPC_OP_SEND_STATUS_FROM_SERVER:
        enif_make_map_put(env, result, sym_send_status, sym_true, &result);
        break;
      case GRPC_OP_RECV_INITIAL_METADATA:
        enif_make_map_put(env, result, sym_metadata, metadata_array_to_map(env, &st->recv_metadata), &result);
        break;
      case GRPC_OP_RECV_MESSAGE:
        enif_make_map_put(env, result, sym_message, byte_buffer_to_binary(env, st->recv_message), &result);
        break;
      case GRPC_OP_RECV_STATUS_ON_CLIENT:
        status = enif_make_new_map(env);
        enif_make_map_put(env, status, enif_make_atom(env, "code"),
                          enif_make_uint(env, st->recv_status), &status);
        if (st->recv_status_details != NULL) {
          enif_make_map_put(env, status, enif_make_atom(env, "details"),
                            better_make_binary(env, st->recv_status_details), &status);
        }
        enif_make_map_put(env, status, enif_make_atom(env, "metadata"),
                          metadata_array_to_map(env, &st->recv_trailing_metadata), &status);
        break;
      case GRPC_OP_RECV_CLOSE_ON_SERVER:
        enif_make_map_put(env, result, sym_send_close, sym_true, &result);
        break;
      default:
        break;
    }
  }

  return result;
}

ERL_NIF_TERM nif_call_run_batch3(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  wrapped_grpc_call *wrapped_call;
  wrapped_run_batch_stack *stack = enif_alloc_resource(run_batch_stack_resource,
                                            sizeof(wrapped_run_batch_stack));
  grpc_call_error err;
  unsigned write_flag = 0;
  ERL_NIF_TERM term;
  ERL_NIF_TERM ERL_NIL = enif_make_atom(env, "nil");
  stack->stack = malloc(sizeof(run_batch_stack));

  if (!enif_get_resource(env, argv[0], grpc_call_resource, (void **)&wrapped_call)) {
    return enif_raise_exception_compat(env, "Could not get call.");
  }
  if (!enif_is_map(env, argv[1])) {
    return enif_raise_exception_compat(env, "ops should be a map");
  }
  // TODO: set write_flag if write_flag is passed

  run_batch_stack_init(stack->stack, write_flag);
  if (!run_batch_stack_fill_ops(env, stack->stack, argv[1])) {
    return ERL_NIL;
  }

  err = grpc_call_start_batch(wrapped_call->call, stack->stack->ops, stack->stack->op_num, NULL, NULL);
  if (err != GRPC_CALL_OK) {
    run_batch_stack_cleanup(stack->stack);
    return enif_raise_exception_compat(env, "Failed to start batch!");
  }

  term = enif_make_resource(env, stack);
  enif_release_resource(stack);

  return term;
}

ERL_NIF_TERM nif_call_finish_batch4(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  wrapped_grpc_completion_queue *wrapped_cq;
  wrapped_run_batch_stack *stack;
  grpc_event event;
  ERL_NIF_TERM result;

  if (!enif_get_resource(env, argv[0], run_batch_stack_resource, (void **)&stack)) {
    return enif_raise_exception_compat(env, "Failed to get run_batch_stack!");
  }

  if (!enif_get_resource(env, argv[1], grpc_completion_queue_resource, (void **)&wrapped_cq)) {
    return enif_raise_exception_compat(env, "Failed to get completion_queue!");
  }

  // wait for it to complete using pluck_event
  event = completion_queue_pluck_event(env, wrapped_cq, argv[2], argv[3]);

  if (event.type == GRPC_QUEUE_TIMEOUT) {
    run_batch_stack_cleanup(stack->stack);
    return enif_raise_exception_compat(env, "grpc_call_start_batch timed out");
  }

  /* Build and return result, if there is an error, it's reflected in the status */
  result = run_batch_stack_build_result(env, stack->stack);
  run_batch_stack_cleanup(stack->stack);

  return result;
}
