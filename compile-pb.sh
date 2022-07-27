#!/usr/bin/env bash

set -o nounset
set -o errexit
set -o pipefail

protoc --elixir_out=gen_descriptors=true,plugins=grpc:./lib --proto_path=priv/protos/ priv/protos/grpc/reflection/v1alpha/reflection.proto
