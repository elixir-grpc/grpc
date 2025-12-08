#!/usr/bin/env bash
# Run benchmark with optimized Erlang VM flags

set -e

# Optimize Erlang VM for performance
export ERL_FLAGS="+sbwt very_long +swt very_low +sub true +pc unicode"
export ELIXIR_ERL_OPTIONS="+fnu"

# Compiler optimizations
export ERL_COMPILER_OPTIONS="[inline,{inline_size,128}]"

# Run benchmark
cd "$(dirname "$0")/.."
MIX_ENV=prod mix benchmark.test --codec=proto --requests=30000
