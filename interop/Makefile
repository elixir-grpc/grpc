gen-protos:
	protoc -I $(GRPC_PATH) --elixir_out=plugins=grpc:lib \
		src/proto/grpc/testing/test.proto \
		src/proto/grpc/testing/empty.proto \
		src/proto/grpc/testing/messages.proto \
		&& mv lib/src/proto/grpc/testing/* lib/grpc_testing \
		&& rm -r lib/src/proto/grpc/testing
