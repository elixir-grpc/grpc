#!/bin/sh

# Copy grpc c code to src/grpc_c for dev and build

if [ $# -eq 0 ]; then
    echo "Error: grpc root path is needed!"
    exit 1
fi

grpc_path=$1
grpc_c_path=src/grpc_c

dirs="
include/grpc
src/core
src/boringssl
src/zlib
third_party/nanopb
third_party/zlib
third_party/boringssl
"

echo "Copy grpc c code to $grpc_c_path"

for path in $dirs
do
  mkdir -p $grpc_c_path/$path
  cp -R $grpc_path/$path/* $grpc_c_path/$path/
done

files="
third_party/objective_c/Cronet/cronet_c_for_grpc.h
Makefile
"
for path in $files
do
  dir=$grpc_c_path/$(dirname $path)
  mkdir -p $dir
  cp $grpc_path/$path $dir
done
