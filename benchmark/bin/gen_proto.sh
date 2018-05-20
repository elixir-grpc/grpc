#!/bin/bash

if [ $# -le 0 ]; then
  echo "grpc repo dir is needed"
  exit 1
fi

GRPC_REPO=$1/src/proto/grpc
echo ${GRPC_REPO}

files=(
  testing/benchmark_service.proto
  testing/control.proto
  testing/messages.proto
  testing/payloads.proto
  testing/stats.proto
  testing/worker_service.proto
  core/stats.proto
)

mkdir -p lib/grpc

for proto in "${files[@]}"; do
  echo $proto
  mkdir -p "$(dirname proto/$proto)"
  cp ${GRPC_REPO}/$proto proto/$proto
  sed -i "" -e "s/import \"src\/proto\/grpc\//import \"/g" proto/$proto

  protoc -I proto --elixir_out=plugins=grpc:./lib/grpc proto/$proto
done
